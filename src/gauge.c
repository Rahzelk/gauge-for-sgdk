#include "gauge.h"

/* =============================================================================
   gauge.c - Simplified single-lane HUD gauge implementation for SGDK
   =============================================================================
 
   TILE STRIP SYSTEM:
   ------------------
   Each segment has pre-rendered "strips" of tiles stored in ROM.
   A 45-tile strip covers all fill combinations for one tile:
     - 9 levels of value (0..8 px) x 9 levels of trail (0..8 px)
     - Only valid combos where trail >= value (triangular matrix = 45 tiles)
   A 64-tile strip is used for trail-specific edge rendering.

   The LUT s_tileIndexByValueTrail[valuePx][trailPx] maps a (value, trail)
   pixel pair to the correct tile index within a strip (O(1) lookup).

   BREAK ZONE:
   -----------
   Most cells are trivially FULL (value=8, trail=8) or EMPTY (0,0).
   The "break zone" is the small region where value/trail partially fill a tile.
   Only break zone cells need per-pixel tile computation.

   Cell types in the break zone (fill direction: left to right):

     |FULL|FULL|VALUE_BREAK|TRAIL_FULL|TRAIL_BREAK|END|EMPTY|EMPTY|
      <--- value --->|<--------- trail -------->|
                     ^                          ^
                value edge                 trail edge

   VALUE_BREAK  : Cell where the value edge falls (0 < valuePx < 8)
   TRAIL_FULL   : Cells fully covered by trail but no value (valuePx=0, trailPx=8)
   TRAIL_BREAK  : Cell where the trail edge falls (0 < trailPx < 8)
   TRAIL_BREAK2 : Second trail break (before END, when bridge forces split)
   END          : Cell at the value/trail frontier (uses end tileset)

   BLINK-OFF:
   ----------
   During trail blink OFF frames, the break zone is recomputed using the
   "actual" trail (breakInfoActual) instead of the rendered trail (breakInfo).
   Trail-zone tiles get replaced with blink-off tilesets for the visual effect.

   PERFORMANCE:
   ------------
   - GaugeLogic_tick: ~50-100 cycles (mostly conditionals)
   - process_fixed_mode: ~200 cycles/cell (DMA dominant), lazy strip eval
   - process_dynamic_mode: ~300-500 cycles total (tilemap writes dominant)
   - Change detection avoids redundant DMA/tilemap writes

   ============================================================================= */

/* -----------------------------------------------------------------------------
   Constants
   ----------------------------------------------------------------------------- */
#define TILE_TO_PIXEL_SHIFT  3

/* Invalid/uninitialized marker for cache values */
#define CACHE_INVALID_U16    0xFFFF
#define CACHE_INVALID_U8     0xFF

/* Default animation speeds (bit shift dividers: higher = slower)
 * Animation step formula: step = (distance >> shift) + 1
 * Blink frequency formula: frequency = 60fps / (2 * (1 << shift)) Hz
 */
#define GAUGE_DEFAULT_VALUE_ANIM_SHIFT  4  /* Value moves at distance/16 + 1 px per frame */
#define GAUGE_DEFAULT_TRAIL_ANIM_SHIFT  4  /* Trail shrinks at distance/16 + 1 px per frame */
#define GAUGE_DEFAULT_BLINK_SHIFT       3  /* Blink toggles every 8 frames (~7.5 Hz @ 60fps) */

/* Helper macros */
#define CALC_ANIM_STEP(diff, shift)   ((u16)(((diff) >> (shift)) + 1))  /* Animation step: distance/2^shift + 1 (always >= 1) */
#define FILL_IDX_TO_OFFSET(idx)       (((u16)(idx)) << 3)              /* fillIndex * 8 = pixel offset (each cell = 8 px) */

/* Simple zero-init allocator wrappers (SGDK MEM_alloc / MEM_free). */
static void *gauge_alloc_bytes(u16 bytes)
{
    if (bytes == 0)
        return NULL;
    void *ptr = MEM_alloc(bytes);
    if (ptr)
        memset(ptr, 0, bytes);
    return ptr;
}

static void gauge_free_ptr(void **ptr)
{
    if (ptr && *ptr)
    {
        MEM_free(*ptr);
        *ptr = NULL;
    }
}

/* PIP compact strip state ordering */
#define PIP_STATE_EMPTY      0
#define PIP_STATE_VALUE      1
#define PIP_STATE_LOSS       2
#define PIP_STATE_GAIN       3
#define PIP_STATE_BLINK_OFF  4

/* Pre-computed strip indices for trivial cell states (avoid LUT indexing in hot path) */
#define STRIP_INDEX_EMPTY       0   /* s_tileIndexByValueTrail[0][0] : value=0, trail=0 */
#define STRIP_INDEX_FULL_TRAIL  8   /* s_tileIndexByValueTrail[0][8] : value=0, trail=8 */
#define STRIP_INDEX_FULL       44   /* s_tileIndexByValueTrail[8][8] : value=8, trail=8 */

/* Shared sentinel arrays for optional per-segment fields (no per-layout heap cost). */
static const u32 *s_nullSegmentTilesets[GAUGE_MAX_SEGMENTS] = {0};
static const u8 s_zeroSegmentFlags[GAUGE_MAX_SEGMENTS] = {0};
static const u8 s_oneSegmentFlags[GAUGE_MAX_SEGMENTS] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
};
static const u8 s_zeroCellFlags[GAUGE_MAX_LENGTH] = {0};
static const u8 s_oneCellFlags[GAUGE_MAX_LENGTH] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
};
static const u8 s_invalidCellIndexes[GAUGE_MAX_LENGTH] = {
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8,
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8,
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8,
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8
};

/**
 * Trail mode (internal state machine state).
 *
 * NONE:   No trail effect active. Trail equals value.
 *         This is the idle state (no damage/heal pending).
 *
 * DAMAGE: Classic damage trail. Value has decreased; the trail stays behind
 *         at the old position, holds, blinks, then shrinks toward the value.
 *         [####====........]   # = value, = = damage trail
 *
 * GAIN:   Gain trail. Value is increasing; the trail leads ahead of the
 *         current value, holds, blinks, then value catches up.
 *         [####====########]   = = gain trail (ahead of value)
 */
typedef enum
{
    GAUGE_TRAIL_NONE   = 0,
    GAUGE_TRAIL_DAMAGE = 1,
    GAUGE_TRAIL_GAIN   = 2
} GaugeTrailMode;


/* =============================================================================
   Tile index lookup table
   =============================================================================
   Maps (valuePxInTile, trailPxInTile) to tile index in 45-tile strip.

   Row = valuePxInTile (0..8)
   Column = trailPxInTile (0..8)

   Constraint: trail >= value (invalid combinations marked 0xFF = CACHE_INVALID_U8)

   The 45-tile strip is organized as triangular matrix (trail >= value):
   - Tiles 0-8:   value=0, trail=0..8  (9 tiles: empty to full trail)
   - Tiles 9-16:  value=1, trail=1..8  (8 tiles)
   - Tiles 17-23: value=2, trail=2..8  (7 tiles)
   - ...
   - Tile 44:     value=8, trail=8     (1 tile: fully filled)
   Total: 9+8+7+6+5+4+3+2+1 = 45 tiles

   Visual representation of a single tile's fill zones:
     +--------+
     |TTTTTTTT|  T = trail pixels (trailPx - valuePx)
     |VVVVTTTT|  V = value pixels
     |VVVV    |  (blank) = empty pixels
     +--------+

   ============================================================================= */
static const u8 s_tileIndexByValueTrail[9][9] =
{
    /*          trail=0  1    2    3    4    5    6    7    8  */
    /* val=0 */ {   0,   1,   2,   3,   4,   5,   6,   7,   8 },
    /* val=1 */ { 0xFF,  9,  10,  11,  12,  13,  14,  15,  16 },
    /* val=2 */ { 0xFF,0xFF, 17,  18,  19,  20,  21,  22,  23 },
    /* val=3 */ { 0xFF,0xFF,0xFF, 24,  25,  26,  27,  28,  29 },
    /* val=4 */ { 0xFF,0xFF,0xFF,0xFF, 30,  31,  32,  33,  34 },
    /* val=5 */ { 0xFF,0xFF,0xFF,0xFF,0xFF, 35,  36,  37,  38 },
    /* val=6 */ { 0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, 39,  40,  41 },
    /* val=7 */ { 0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, 42,  43 },
    /* val=8 */ { 0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, 44 }
};

/* =============================================================================
   Trail tile index lookup table (64-tile strip)
   =============================================================================
   Maps (valuePxInTile, trailPxInTile) to tile index in 64-tile trail strip.
   Used for TRAIL BREAK cells (transition zone between value and trail edges).

   Row = valuePxInTile (0..8)
   Column = trailPxInTile (0..8)

   Unlike the 45-tile body strip, this strip uses a different tile layout
   optimized for trail-specific rendering (e.g., different edge shapes).
   Column 8 maps to the same tile as column 7 (trail=7 and trail=8 share
   the same visual when the cell is fully covered by trail).

   Note: 0xFF entries (trail < value) are invalid and never accessed.
   ============================================================================= */
static const u8 s_trailTileIndexByValueTrail[9][9] =
{
    /*          trail=0  1    2    3    4    5    6    7    8  */
    /* val=0 */ {   0,   1,   2,   3,   4,   5,   6,   7,   7 },
    /* val=1 */ { 0xFF,  8,   9,  10,  11,  12,  13,  14,  14 },
    /* val=2 */ { 0xFF,  15, 16,  17,  18,  19,  20,  21,  21 },
    /* val=3 */ { 0xFF,  22, 23,  24,  25,  26,  27,  28,  28 },
    /* val=4 */ { 0xFF,  29, 30,  31,  32,  33,  34,  35,  35 },
    /* val=5 */ { 0xFF,  36, 37,  38,  39,  40,  41,  42,  42 },
    /* val=6 */ { 0xFF,  43, 44,  45,  46,  47,  48,  49,  49 },
    /* val=7 */ { 0xFF,  50, 51,  52,  53,  54,  55,  56,  56 },
    /* val=8 */ { 0xFF,  57, 58,  59,  60,  61,  62,  63,  63 }
};


/* =============================================================================
   Helper functions (inlined for performance)
   ============================================================================= */

/**
 * Clamp value to tile pixel range [0, 8].
 *
 * Lower clamp is branchless via arithmetic masking:
 * - If v < 0, bit 15 is set, so ~(v >> 15) = 0x0000 which zeroes v.
 * - If v >= 0, ~(v >> 15) = 0xFFFF which preserves v.
 * Upper clamp uses a single branch (rare: only on break cells).
 *
 * Cost: ~8-12 cycles (asr + not + and + cmp + branch)
 * vs original: ~10-20 cycles (2 branches worst-case)
 */
static inline u8 clamp_to_tile_size(s16 v)
{
    /* Branchless lower clamp: zero if negative.
     * v >> 15 is SIGNED (arithmetic) shift: gives -1 (0xFFFF) if v < 0, 0 otherwise.
     * ~(-1) = 0 zeroes v; ~0 = 0xFFFF preserves v.
     * IMPORTANT: must NOT cast v to u16 before shift - that would make it unsigned
     * and give 0 or 1 instead of 0 or 0xFFFF, breaking the mask. */
    u16 clamped = (u16)(v & ~(v >> 15));
    /* Upper clamp: cap at 8 (rare case, only at break cells) */
    return (clamped > GAUGE_PIXELS_PER_TILE) ? GAUGE_PIXELS_PER_TILE : (u8)clamped;
}

/**
 * Convert gauge value to pixel position.
 * Uses LUT if available, otherwise returns value directly (1:1 mapping).
 *
 * The LUT is auto-generated by Gauge_init() when maxValue != maxFillPixels.
 */
static inline u16 value_to_pixels(const GaugeLogic *logic, u16 value)
{
    if (logic->valueToPixelsLUT)
        return logic->valueToPixelsLUT[value];
    return value;
}

/**
 * Compute fill amounts for a given fillIndex (0..length-1).
 *
 * Core algorithm for pixel-perfect gauge rendering.
 * Determines how many pixels of value and trail are visible in one cell.
 *
 * @param layout       Layout containing fillOffset
 * @param fillIndex    Logical fill position (0 = first filled cell)
 * @param valuePixels  Current value fill in pixels
 * @param trailPixels  Current trail fill in pixels
 * @param outValuePx   [out] Value fill in this cell (0-8)
 * @param outTrailPx   [out] Trail fill in this cell (0-8, always >= outValuePx)
 *
 * Invariant: outTrailPx >= outValuePx (trail can never be behind value).
 * Cost: ~20 cycles (2x clamp + 1 compare)
 */
static inline void compute_fill_for_fill_index(const GaugeLayout *layout,
                                               u8 fillIndex,
                                               u16 valuePixels,
                                               u16 trailPixels,
                                               u8 *outValuePx,
                                               u8 *outTrailPx)
{
    const u16 cellStartPixel = layout->fillOffset + ((u16)fillIndex << TILE_TO_PIXEL_SHIFT);

    u8 valuePxInCell = clamp_to_tile_size((s16)valuePixels - (s16)cellStartPixel);
    u8 trailPxInCell = clamp_to_tile_size((s16)trailPixels - (s16)cellStartPixel);

    /* Enforce trail >= value invariant */
    if (trailPxInCell < valuePxInCell)
        trailPxInCell = valuePxInCell;

    *outValuePx = valuePxInCell;
    *outTrailPx = trailPxInCell;
}

/**
 * Compute fill amounts for a cell by its cellIndex.
 * Wrapper around compute_fill_for_fill_index using the fillIndexByCell lookup.
 *
 * @param layout       Layout configuration
 * @param cellIndex    Cell index in the layout (0..length-1)
 * @param valuePixels  Current value fill in pixels
 * @param trailPixels  Current trail fill in pixels
 * @param outValuePx   [out] Value fill in this cell (0-8)
 * @param outTrailPx   [out] Trail fill in this cell (0-8, always >= outValuePx)
 */
static inline void compute_fill_for_cell(const GaugeLayout *layout,
                                         u8 cellIndex,
                                         u16 valuePixels,
                                         u16 trailPixels,
                                         u8 *outValuePx,
                                         u8 *outTrailPx)
{
    compute_fill_for_fill_index(layout, layout->fillIndexByCell[cellIndex],
                                valuePixels, trailPixels, outValuePx, outTrailPx);
}

/**
 * Compute the break fillIndex and its pixel offset within that cell.
 *
 * Given a pixel position, determines which cell (by fillIndex) contains that
 * pixel and how many pixels are filled within that cell.
 *
 * Special case: exact tile boundary (pixel % 8 == 0 && pixel > 0) maps to
 * the previous cell with 8px filled, not the next cell with 0px.
 *
 * @param layout      Layout containing fillOffset and length
 * @param pixels      Pixel position to locate
 * @param outPxInTile [out] Pixels filled in the located cell (0-8)
 * @return fillIndex of the cell containing this pixel position
 *
 * Cost: ~25 cycles (subtract + shift + mask test + clamp)
 */
static inline u8 compute_fill_index_and_px(const GaugeLayout *layout,
                                           u16 pixels,
                                           u8 *outPxInTile)
{
    s16 rel = (s16)pixels - (s16)layout->fillOffset;
    if (rel < 0)
    {
        *outPxInTile = 0;
        return 0;
    }

    u16 fillIndex;
    u8 pxInTile;

    /* Exact tile boundary: use previous cell with 8px filled. */
    if ((rel > 0) && ((rel & (GAUGE_PIXELS_PER_TILE - 1)) == 0))
    {
        fillIndex = (u16)((rel >> TILE_TO_PIXEL_SHIFT) - 1);
        pxInTile = GAUGE_PIXELS_PER_TILE;
    }
    else
    {
        fillIndex = (u16)(rel >> TILE_TO_PIXEL_SHIFT);
        const u16 cellStartPixel = layout->fillOffset + ((u16)fillIndex << TILE_TO_PIXEL_SHIFT);
        pxInTile = clamp_to_tile_size((s16)pixels - (s16)cellStartPixel);
    }

    if (fillIndex >= layout->length)
    {
        fillIndex = (u16)(layout->length - 1);
        pxInTile = GAUGE_PIXELS_PER_TILE;
    }

    *outPxInTile = pxInTile;

    return (u8)fillIndex;
}

/**
 * Compute tile position on the WINDOW plane based on orientation.
 *
 * For horizontal gauges: X varies by cell, Y stays at origin.
 * For vertical gauges:   Y varies by cell, X stays at origin.
 *
 * @param orient  GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL
 * @param originX Tilemap X origin of the gauge
 * @param originY Tilemap Y origin of the gauge
 * @param cell    Cell index (0..length-1)
 * @param outX    Output: tilemap X coordinate
 * @param outY    Output: tilemap Y coordinate
 */
static inline void compute_tile_xy(u8 orient,
                                   u16 originX, u16 originY,
                                   u8 cell,
                                   u16 *outX, u16 *outY)
{
    if (orient == GAUGE_ORIENT_HORIZONTAL)
    {
        *outX = (u16)(originX + cell);
        *outY = originY;
    }
    else
    {
        *outX = originX;
        *outY = (u16)(originY + cell);
    }
}



/* =============================================================================
   GaugeBreakInfo - Pre-computed break/transition zone data

   Visual notation (single cell):
   F = BODY full   (value=8, trail=8)
   V = BODY empty  (value=0, trail=0)
   B = VALUE_BREAK (BODY tileset, transition before value edge)
   T = TRAIL full  (TRAIL tileset, value=0, trail=8)
   E = END         (END tileset, end of gauge)
   D = BRIDGE      (BRIDGE tileset, transition between segments)
   =============================================================================

   Shared by process_fixed_mode() and process_dynamic_mode().
   Contains all the information needed to classify cells in the break zone
   between the value edge and the trail edge.

   Gauge rendering visualized (horizontal, fill forward):

     Cell:     [0][1][2][3][4][5][6][7]
     Fill:     F  F  B  V  T  T  E  V
     Pixels:   F=8px, V=0px, B/T/E use partial LUTs

     F (FULL)     = value fills entire cell (8px)
     B (BREAK)    = transition cell before value edge (BODY tileset)
     VALUE CELL   = cell containing the value edge (partial value px)
     T (TRAIL)    = cells between value and trail edges (TRAIL tileset)
     E (END)      = cell containing the trail edge / cap (END tileset)
     V (EMPTY)    = no fill (0px)

   Multi-part gauges: if value or trail edges fall outside this layout's
   pixel range [fillOffset .. fillOffset + length*8], the corresponding
   break features are disabled for this part.
   ============================================================================= */

typedef struct
{
    /* Break cell fill indices (CACHE_INVALID_U8 if not applicable) */
    u8 valueFillIndex;          /* Fill index of the value break cell (B) */
    u8 trailFillIndex;          /* Fill index of the trail break cell (T) */
    u8 valuePxInBreakCell;      /* Pixels of value in the value break cell (B) */
    u8 trailPxInBreakCell;      /* Pixels of trail in the trail break cell (T) */

    /* Cell indices (via inverse LUT) */
    u8 valueCellIndex;          /* Layout cell index for value break (B) */
    u8 trailCellIndex;          /* Layout cell index for trail break (T) */

    /* END/BREAK zone indices */
    u8 endFillIndex;            /* Fill index for END cap tile (E) (CACHE_INVALID_U8 if none) */
    u8 valueBreakFillIndex;     /* Fill index for VALUE BREAK cell (B) (CACHE_INVALID_U8 if none) */
    u8 trailBreakFillIndex;     /* Fill index for TRAIL BREAK 1 (T) (CACHE_INVALID_U8 if none) */
    u8 trailBreakFillIndex2;    /* Fill index for TRAIL BREAK 2 (T) (CACHE_INVALID_U8 if none) */

    /* Pre-computed END cell fill values */
    u8 endValuePxInTile;        /* Value pixels in END cell (E) */
    u8 endTrailPxInTile;        /* Trail pixels in END cell (E) */

    /* Flags */
    u8 trailBreakActive;        /* Trail break zone is active */
    u8 trailBreakSecondActive;  /* Second trail break cell exists */
    u8 regionRenderActive;      /* Region-based rendering applies */

} GaugeBreakInfo;

/* Forward declarations for helpers used by compute_break_info */
static inline const u32 *select_base_strip(const u32 *normalStrip,
                                           const u32 *gainStrip,
                                           u8 trailMode);
static void build_pip_luts(GaugeLayout *layout);
static u16 compute_vram_size_for_layout(const GaugeLayout *layout,
                                        GaugeVramMode vramMode,
                                        u8 trailEnabled,
                                        GaugeValueMode valueMode);

/**
 * Compute all break/transition zone information for a gauge part.
 *
 * This function determines where the value and trail edges fall within the
 * layout, and pre-computes all indices and flags needed for per-cell
 * classification in the render loop.
 *
 * @param layout              Layout configuration (with inverse LUT)
 * @param valuePixels         Current value fill in pixels
 * @param trailPixelsRendered Current trail fill in pixels (after blink)
 * @param out                 [out] Pre-computed break zone information
 *
 * Cost: ~80 cycles (2x compute_fill_index_and_px + LUT lookups + flags)
 */
static inline void compute_break_info(const GaugeLayout *layout,
                                      u16 valuePixels,
                                      u16 trailPixelsRendered,
                                      u8 trailMode,
                                      GaugeBreakInfo *out)
{
    /* === Step 1: Compute value and trail fill positions ===
     * For each edge (value and trail), find which cell it falls in (fillIndex)
     * and how many pixels into that cell it extends (pxInBreakCell). */
    out->valueFillIndex = compute_fill_index_and_px(layout, valuePixels, &out->valuePxInBreakCell);
    out->trailFillIndex = compute_fill_index_and_px(layout, trailPixelsRendered, &out->trailPxInBreakCell);

    /* O(1) inverse LUT lookup instead of O(n) linear scan (~300 cycles saved) */
    out->valueCellIndex = layout->cellIndexByFillIndex[out->valueFillIndex];
    out->trailCellIndex = layout->cellIndexByFillIndex[out->trailFillIndex];

    const u8 valueSegId = layout->segmentIdByCell[out->valueCellIndex];
    const u8 trailSegId = layout->segmentIdByCell[out->trailCellIndex];
    const u8 valueSegmentHasEnd =
        (select_base_strip(layout->tilesetEndBySegment[valueSegId],
                           layout->gainTilesetEndBySegment[valueSegId],
                           trailMode) != NULL);
    const u8 trailSegmentHasEnd =
        (select_base_strip(layout->tilesetEndBySegment[trailSegId],
                           layout->gainTilesetEndBySegment[trailSegId],
                           trailMode) != NULL);
    const u8 valueSegmentHasTrail =
        (select_base_strip(layout->tilesetTrailBySegment[valueSegId],
                           layout->gainTilesetTrailBySegment[valueSegId],
                           trailMode) != NULL);

    /* Detect if break cells belong to this layout (multi-part gauges) */
    const u16 layoutStart = layout->fillOffset;
    const u16 layoutEnd = (u16)(layoutStart + ((u16)layout->length << TILE_TO_PIXEL_SHIFT));
    const u8 valueInLayout = (valuePixels >= layoutStart && valuePixels <= layoutEnd);
    const u8 trailInLayout = (trailPixelsRendered >= layoutStart && trailPixelsRendered <= layoutEnd);

    /* === Step 2: Compute END cell position (transition tile) ===
     * END cap (E):
     * - Normal case: place END on trail edge when trail is visible.
     * - Offset/window fallback:
     *   keep one END inside the visible window even when trail edge is outside,
     *   so partial gauges with END/BREAK assets remain visually stable.
     *
     * Priority order is important:
     * 1) trail in layout      -> END on trail edge
     * 2) trail right of view  -> END on last visible cell
     * 3) trail left of view   -> END on first visible cell
     * 4) value in layout      -> END on value edge
     * 5) no END in this part
     */
    if (trailInLayout && trailSegmentHasEnd)
    {
        out->endFillIndex = out->trailFillIndex;
    }
    else if (trailPixelsRendered > layoutEnd && trailSegmentHasEnd)
    {
        out->endFillIndex = (u8)(layout->length - 1);
    }
    else if (trailPixelsRendered < layoutStart && trailSegmentHasEnd)
    {
        out->endFillIndex = 0;
    }
    else if (valueInLayout && valueSegmentHasEnd)
    {
        out->endFillIndex = out->valueFillIndex;
    }
    else
    {
        out->endFillIndex = CACHE_INVALID_U8;
    }

    /* === Step 3: Compute VALUE_BREAK position ===
     * VALUE BREAK (B): one cell before value break */
    out->valueBreakFillIndex =
        (valueInLayout && valueSegmentHasEnd && out->valueFillIndex > 0)
        ? (u8)(out->valueFillIndex - 1) : CACHE_INVALID_U8;

    /* === Step 4: Compute TRAIL_BREAK positions (first and second) ===
     * TRAIL BREAK (T): active when the trail zone intersects this layout window
     * and value/trail edges are not in the same cell. */
    const u8 trailZoneIntersectsLayout =
        (trailPixelsRendered > layoutStart) && (valuePixels < layoutEnd);

    out->trailBreakActive =
        (trailZoneIntersectsLayout && valueSegmentHasEnd && valueSegmentHasTrail &&
         (out->valueCellIndex != out->trailCellIndex));
    out->trailBreakFillIndex = out->trailBreakActive ? out->valueFillIndex : CACHE_INVALID_U8;
    out->trailBreakSecondActive =
        (out->trailBreakActive && (out->trailFillIndex > (u8)(out->valueFillIndex + 1)));
    out->trailBreakFillIndex2 = out->trailBreakSecondActive
        ? (u8)(out->trailFillIndex - 1) : CACHE_INVALID_U8;

    /* === Step 5: Check if region-based rendering is needed ===
     * Region rendering: active when both edges are visible and have END tilesets */
    out->regionRenderActive = (valueInLayout && trailInLayout &&
                               valueSegmentHasEnd && trailSegmentHasEnd);

    /* Precompute END (E) cell fill values */
    out->endValuePxInTile = 0;
    out->endTrailPxInTile = 0;
    if (out->endFillIndex != CACHE_INVALID_U8)
    {
        compute_fill_for_fill_index(layout, out->endFillIndex,
                                    valuePixels, trailPixelsRendered,
                                    &out->endValuePxInTile, &out->endTrailPxInTile);
    }
}

/**
 * Check if a cell (by fill index) is in the break zone (END/TRAIL/VALUE_BREAK).
 *
 * Returns 1 if the cell is any of: END, VALUE_BREAK, TRAIL_BREAK, TRAIL_BREAK2,
 * or TRAIL_FULL. Used to decide whether blink-off tilesets should override.
 *
 * @param breakInfo     Pre-computed break zone info
 * @param cellFillIndex Fill index of the cell to check
 * @return 1 if cell is in break zone, 0 otherwise
 */
static inline u8 is_cell_in_trail_zone(const GaugeBreakInfo *breakInfo, u8 cellFillIndex)
{
    if (breakInfo->endFillIndex != CACHE_INVALID_U8 && cellFillIndex == breakInfo->endFillIndex)
        return 1;
    if (breakInfo->valueBreakFillIndex != CACHE_INVALID_U8 && cellFillIndex == breakInfo->valueBreakFillIndex)
        return 1;
    if (breakInfo->trailBreakActive)
    {
        if (cellFillIndex == breakInfo->trailBreakFillIndex)
            return 1;
        if (breakInfo->trailBreakSecondActive && cellFillIndex == breakInfo->trailBreakFillIndex2)
            return 1;
        if (cellFillIndex > breakInfo->valueFillIndex && cellFillIndex < breakInfo->trailFillIndex)
            return 1;
    }
    return 0;
}


/* =============================================================================
   Cap Start classification result
   =============================================================================

   Cap Start is a fixed border tile at the start of a gauge (cell 0 in fill order).
   It uses dedicated tilesets that vary depending on the gauge state at cell 0:

   Visual representation (horizontal, fill forward):
     Cell:    [CAP][1][2][3][4][5][6][7]
     State:    CS   F  F  B  V  T  T  E

   Cap start has 3 tileset variants:
   - capStartStrip:      default cap (used when cell is END or FULL)
   - capStartBreakStrip: cap while value hasn't reached cell 0 yet (empty/break state)
   - capStartTrailStrip: cap while trail zone is visible (trail break/full)

   Priority rules for tileset + fill index selection:
   1. Cell is END (E)             -> capStartStrip + endIndex
   2. Cell is TRAIL (break/full)  -> capStartTrailStrip + trail LUT
   3. Next cell is END (E)        -> capStartBreakStrip + endIndex (aligned to E)
   4. Else (cell is FULL)         -> capStartBreakStrip + index 44 (full)

   ============================================================================= */
typedef struct
{
    const u32 *strip;     /* Selected ROM strip for this frame */
    u8 fillStripIndex;    /* Index into the selected strip */
    u8 usesBreak;         /* 1 if using the break variant strip */
    u8 usesTrail;         /* 1 if using the trail variant strip */
} CapStartResult;

/**
 * Classify cap start cell and select the appropriate tileset strip + tile index.
 *
 * Applies the 4-priority rule (see CapStartResult block comment above):
 * 1. Cell is END          -> capStartStrip + endIndex
 * 2. Cell is TRAIL zone   -> capStartTrailStrip + trail LUT index
 * 3. Next cell is END     -> capStartBreakStrip + endIndex
 * 4. Default (FULL/EMPTY) -> capStartBreakStrip + STRIP_INDEX_FULL
 *
 * @param breakInfo          Pre-computed break zone info
 * @param cellFillIndex      Fill index of the cap start cell
 * @param capStartStrip      Default cap tileset (ROM strip)
 * @param capStartBreakStrip Cap tileset for break/full state (can be NULL)
 * @param capStartTrailStrip Cap tileset for trail zone (can be NULL)
 * @param out                Output: selected strip, tile index, and flags
 */
static inline void classify_cap_start_with_strips(const GaugeBreakInfo *breakInfo,
                                                  u8 cellFillIndex,
                                                  const u32 *capStartStrip,
                                                  const u32 *capStartBreakStrip,
                                                  const u32 *capStartTrailStrip,
                                                  CapStartResult *out)
{
    const u8 endValid = (breakInfo->endFillIndex != CACHE_INVALID_U8);
    const u8 isEndHere = (endValid && cellFillIndex == breakInfo->endFillIndex);
    const u8 isEndNext = (endValid && (u8)(cellFillIndex + 1) == breakInfo->endFillIndex);

    /* Pre-compute the END fill index (used in multiple branches) */
    const u8 endIndex = endValid
        ? s_tileIndexByValueTrail[breakInfo->endValuePxInTile][breakInfo->endTrailPxInTile]
        : STRIP_INDEX_FULL;

    /* Detect trail zone states */
    const u8 isTrailBreak = (breakInfo->trailBreakActive && capStartTrailStrip &&
                             cellFillIndex == breakInfo->trailBreakFillIndex);
    const u8 isTrailBreak2 = (breakInfo->trailBreakSecondActive && capStartTrailStrip &&
                              cellFillIndex == breakInfo->trailBreakFillIndex2);
    const u8 isTrailFull = (breakInfo->trailBreakActive && capStartTrailStrip &&
                            cellFillIndex > breakInfo->valueFillIndex &&
                            cellFillIndex < breakInfo->trailFillIndex);
    const u8 useTrail = (isTrailBreak || isTrailBreak2 || isTrailFull);

    /* --- Select strip --- */
    if (isEndHere)
    {
        /* Priority 1: cell is END -> use main cap start strip */
        out->strip = capStartStrip;
        out->usesBreak = 0;
        out->usesTrail = 0;
    }
    else if (useTrail)
    {
        /* Priority 2: cell is in trail zone -> use trail variant */
        out->strip = capStartTrailStrip;
        out->usesBreak = 0;
        out->usesTrail = 1;
    }
    else
    {
        /* Priority 3-4: break/full -> use break variant (fallback to main cap) */
        out->strip = capStartBreakStrip ? capStartBreakStrip : capStartStrip;
        out->usesBreak = 1;
        out->usesTrail = 0;
    }

    /* --- Select fill index --- */
    if (isEndHere)
    {
        /* END cell: use the end fill index directly */
        out->fillStripIndex = endIndex;
    }
    else if (isTrailBreak)
    {
        /* Trail break #1: transition cell right after value edge.
         * If a second trail break exists, this cell is fully covered by trail (trailPx=8).
         * Otherwise, use the actual trail px in the break cell. */
        if (breakInfo->trailBreakSecondActive)
            out->fillStripIndex = s_trailTileIndexByValueTrail[breakInfo->valuePxInBreakCell][8];
        else
            out->fillStripIndex = s_trailTileIndexByValueTrail[breakInfo->valuePxInBreakCell][breakInfo->trailPxInBreakCell];
    }
    else if (isTrailBreak2)
    {
        /* Trail break #2: second transition cell (before END).
         * If END index is 8 (full trail column), use trail LUT for (0,8).
         * Otherwise reuse the END fill index on the trail strip. */
        if (endIndex == 8)
            out->fillStripIndex = s_trailTileIndexByValueTrail[0][8];
        else
            out->fillStripIndex = endIndex;
    }
    else if (isTrailFull)
    {
        /* Full trail zone: fixed index 7 (value=0, trail=full in trail strip) */
        out->fillStripIndex = 7;
    }
    else if (isEndNext)
    {
        /* Next cell is END: align break cap to match END fill index */
        out->fillStripIndex = endIndex;
    }
    else
    {
        /* Default: fully filled (index 44) */
        out->fillStripIndex = STRIP_INDEX_FULL;
    }
}

/**
 * Classify the cap start cell and determine which strip + index to use.
 *
 * @param layout         Layout configuration
 * @param breakInfo            Pre-computed break zone info
 * @param cellFillIndex  Fill index of the cap start cell
 * @param segmentId          Segment ID of the cap start cell
 * @param out            [out] Classification result (strip + index)
 *
 * Cost: ~30-50 cycles (a few comparisons + LUT lookups)
 */
static inline void classify_cap_start(const GaugeLayout *layout,
                                       const GaugeBreakInfo *breakInfo,
                                       u8 cellFillIndex,
                                       u8 segmentId,
                                       CapStartResult *out)
{
    classify_cap_start_with_strips(breakInfo, cellFillIndex,
                                   layout->tilesetCapStartBySegment[segmentId],
                                   layout->tilesetCapStartBreakBySegment[segmentId],
                                   layout->tilesetCapStartTrailBySegment[segmentId],
                                   out);
}

/* =============================================================================
   Bridge strip index computation
   =============================================================================

   A BRIDGE tile sits at the last cell of a segment (before a segment transition).
   Its fill index must match what the NEXT cell would show, so the visual
   transition between segments is seamless.

   Visual example with 2 segments (A and B):
     Cell:    [A][A][A][D][B][B][B][E]
                         ^
                    Bridge cell (D) mirrors what cell B+1 would show

   The bridge strip index is computed by looking ahead at the next fillIndex
   and determining what visual state it would have (full, partial, empty, trail).

   ============================================================================= */

/**
 * Compute the fill strip index for a bridge tile.
 *
 * The bridge tile mirrors the fill state of the next cell in fill order.
 * This ensures a seamless visual transition between segments.
 *
 * @param layout              Layout configuration
 * @param breakInfo                 Pre-computed break zone info (for rendered trail)
 * @param cellFillIndex       Fill index of the bridge cell
 * @param valuePixels         Current value fill in pixels
 * @param trailPixelsRendered Current trail fill in pixels (after blink)
 * @return Strip index to use for the bridge tile
 *
 * Cost: ~20-40 cycles (a few comparisons + optional fill computation)
 */
static inline u8 compute_bridge_strip_index(const GaugeLayout *layout,
                                             const GaugeBreakInfo *breakInfo,
                                             u8 cellFillIndex,
                                             u16 valuePixels,
                                             u16 trailPixelsRendered)
{
    const u8 nextFillIndex = (u8)(cellFillIndex + 1);

    /* Case 1: next cell is END -> use END fill index */
    if (breakInfo->endFillIndex == nextFillIndex)
    {
        return s_tileIndexByValueTrail[breakInfo->endValuePxInTile][breakInfo->endTrailPxInTile];
    }

    /* Case 2: next cell is trail break #1 -> compute its fill */
    if (breakInfo->trailBreakActive && nextFillIndex == breakInfo->trailBreakFillIndex)
    {
        u8 nextValuePx = 0;
        u8 nextTrailPx = 0;
        compute_fill_for_fill_index(layout, nextFillIndex,
                                    valuePixels, trailPixelsRendered,
                                    &nextValuePx, &nextTrailPx);
        return s_tileIndexByValueTrail[nextValuePx][nextTrailPx];
    }

    /* Case 3: next cell is full trail (between value and trail edges) */
    if (breakInfo->trailBreakActive &&
        nextFillIndex > breakInfo->valueFillIndex &&
        nextFillIndex < breakInfo->trailFillIndex)
    {
        return STRIP_INDEX_FULL_TRAIL;
    }

    /* Default: fully filled (index 44) */
    return STRIP_INDEX_FULL;
}


/**
 * Determine if cap start/end are enabled for a given layout.
 *
 * Cap start applies to the first cell in fill order (fillIndex 0).
 * Cap end applies to the last cell in fill order (fillIndex length-1).
 * Rule: if layout is 1 cell and both caps are enabled, cap end wins.
 *
 * @param layout           Layout configuration
 * @param outCapStartEnabled [out] 1 if cap start is enabled
 * @param outCapEndEnabled   [out] 1 if cap end is enabled
 */
static inline void detect_caps_enabled(const GaugeLayout *layout,
                                        u8 *outCapStartEnabled,
                                        u8 *outCapEndEnabled)
{
    const u8 cellStart = layout->cellIndexByFillIndex[0];
    const u8 cellEnd = layout->cellIndexByFillIndex[layout->length - 1];
    const u8 startSegId = layout->segmentIdByCell[cellStart];
    const u8 endSegId = layout->segmentIdByCell[cellEnd];

    *outCapEndEnabled = (layout->capEndBySegment[endSegId] != 0) &&
                        (layout->tilesetCapEndBySegment[endSegId] != NULL ||
                         layout->gainTilesetCapEndBySegment[endSegId] != NULL);
    *outCapStartEnabled = (layout->tilesetCapStartBySegment[startSegId] != NULL ||
                           layout->gainTilesetCapStartBySegment[startSegId] != NULL);

    /* Single-cell layout: cap end takes priority over cap start */
    if (layout->length == 1 && *outCapEndEnabled)
        *outCapStartEnabled = 0;
}

/**
 * Select base tileset for current trail mode.
 * - GAIN: use gain tileset if provided, else fallback to normal
 * - DAMAGE/NONE: use normal tileset (fallback to gain only if normal is NULL)
 */
static inline const u32 *select_base_strip(const u32 *normalStrip,
                                           const u32 *gainStrip,
                                           u8 trailMode)
{
    if (trailMode == GAUGE_TRAIL_GAIN)
        return gainStrip ? gainStrip : normalStrip;
    return normalStrip ? normalStrip : gainStrip;
}

/**
 * Select blink-off tileset for current trail mode.
 * No cross-mode fallback (gain blink-off is distinct).
 */
static inline const u32 *select_blink_strip(const u32 *normalBlinkStrip,
                                            const u32 *gainBlinkStrip,
                                            u8 trailMode)
{
    return (trailMode == GAUGE_TRAIL_GAIN) ? gainBlinkStrip : normalBlinkStrip;
}

/**
 * Apply blink-off strip overrides for a single cell.
 *
 * Checks whether the cell at cellFillIndex falls into a blink-off zone
 * (end, trail, or value break) using the actual trail zone (breakInfoActual).
 * If so, overrides the corresponding strip pointer to the blink-off variant.
 *
 * Shared between process_fixed_mode() and process_dynamic_mode() to avoid
 * duplicating ~40 lines of identical classification logic.
 *
 * @return 1 if blink-off was applied, 0 otherwise.
 */
static inline u8 apply_blink_off_overrides(
    const GaugeBreakInfo *breakInfoActual,
    u8 cellFillIndex,
    const u32 *blinkOffEndStrip,
    const u32 *blinkOffTrailStrip,
    const u32 *blinkOffBodyStrip,
    const u32 **endStrip,
    const u32 **trailStrip,
    const u32 **bodyStrip)
{
    const u8 cellIsEnd = (breakInfoActual->endFillIndex != CACHE_INVALID_U8 &&
                          *endStrip && cellFillIndex == breakInfoActual->endFillIndex);
    const u8 cellIsTrailBreak =
        (breakInfoActual->trailBreakActive && *trailStrip &&
         cellFillIndex == breakInfoActual->trailBreakFillIndex);
    const u8 cellIsTrailBreak2 =
        (breakInfoActual->trailBreakSecondActive && *trailStrip &&
         cellFillIndex == breakInfoActual->trailBreakFillIndex2);
    const u8 cellIsTrailFull =
        (breakInfoActual->trailBreakActive && *trailStrip &&
         cellFillIndex > breakInfoActual->valueFillIndex &&
         cellFillIndex < breakInfoActual->trailFillIndex);
    const u8 cellIsValueBreak = (breakInfoActual->valueBreakFillIndex != CACHE_INVALID_U8 &&
                                 cellFillIndex == breakInfoActual->valueBreakFillIndex);

    if (cellIsEnd && blinkOffEndStrip)
    {
        *endStrip = blinkOffEndStrip;
        return 1;
    }
    if ((cellIsTrailBreak || cellIsTrailBreak2 || cellIsTrailFull) &&
        blinkOffTrailStrip)
    {
        *trailStrip = blinkOffTrailStrip;
        return 1;
    }
    if (cellIsValueBreak && blinkOffBodyStrip)
    {
        *bodyStrip = blinkOffBodyStrip;
        return 1;
    }
    return 0;
}

/**
 * Check if any blink-off tileset is provided for the active trail mode.
 *
 * Scans all segments for non-NULL blink-off tilesets (damage or gain).
 * Called once per trail mode change to cache the result in GaugeLayout.
 *
 * @param layout    Layout to inspect
 * @param trailMode GAUGE_TRAIL_DAMAGE or GAUGE_TRAIL_GAIN
 * @return 1 if any segment has blink-off tilesets, 0 otherwise
 */
static inline u8 layout_has_blink_off_mode(const GaugeLayout *layout, u8 trailMode)
{
    if (trailMode == GAUGE_TRAIL_GAIN)
    {
        for (u8 i = 0; i < layout->segmentCount; i++)
        {
            if (layout->gainBlinkOffTilesetBySegment[i] ||
                layout->gainBlinkOffTilesetEndBySegment[i] ||
                layout->gainBlinkOffTilesetTrailBySegment[i] ||
                layout->gainBlinkOffTilesetBridgeBySegment[i] ||
                layout->gainBlinkOffTilesetCapStartBySegment[i] ||
                layout->gainBlinkOffTilesetCapEndBySegment[i] ||
                layout->gainBlinkOffTilesetCapStartBreakBySegment[i] ||
                layout->gainBlinkOffTilesetCapStartTrailBySegment[i])
            {
                return 1;
            }
        }
        return 0;
    }

    for (u8 i = 0; i < layout->segmentCount; i++)
    {
        if (layout->blinkOffTilesetBySegment[i] ||
            layout->blinkOffTilesetEndBySegment[i] ||
            layout->blinkOffTilesetTrailBySegment[i] ||
            layout->blinkOffTilesetBridgeBySegment[i] ||
            layout->blinkOffTilesetCapStartBySegment[i] ||
            layout->blinkOffTilesetCapEndBySegment[i] ||
            layout->blinkOffTilesetCapStartBreakBySegment[i] ||
            layout->blinkOffTilesetCapStartTrailBySegment[i])
        {
            return 1;
        }
    }
    return 0;
}


/**
 * Upload a tile from a ROM strip to VRAM if not already cached (fixed mode).
 *
 * Compares the desired strip and tile index against the cell's cache.
 * If different, queues a DMA transfer and updates the cache.
 *
 * @param cell            Stream cell with VRAM address and cache
 * @param wantedStrip     ROM strip to read from (NULL = skip)
 * @param desiredStripIdx Tile index within the strip (0..44 for 45-tile, 0..63 for 64-tile)
 */
static inline void upload_cell_if_needed(GaugeStreamCell *cell,
                                         const u32 *wantedStrip,
                                         u8 desiredStripIdx)
{
    if (!wantedStrip) return;
    if (cell->cachedFillIndex == desiredStripIdx &&
        cell->cachedStrip == wantedStrip)
        return;

    const u32 *src = wantedStrip + FILL_IDX_TO_OFFSET(desiredStripIdx);
    VDP_loadTileData(src, cell->vramTileIndex, 1, DMA_QUEUE);
    cell->cachedFillIndex = desiredStripIdx;
    cell->cachedStrip = wantedStrip;
}

/**
 * Upload a fill tile from ROM strip to VRAM.
 */
static inline void upload_fill_tile(const u32 *strip, u8 fillIndex, u16 vramTile, u8 dmaMode)
{
    const u32 *src = strip + FILL_IDX_TO_OFFSET(fillIndex);
    VDP_loadTileData(src, vramTile, 1, dmaMode);
}

static inline u8 segment_tileset_array_has_any(const u32 * const *tilesetsBySegment,
                                               u8 segmentCount)
{
    if (!tilesetsBySegment)
        return 0;

    for (u8 segmentId = 0; segmentId < segmentCount; segmentId++)
    {
        if (tilesetsBySegment[segmentId] != NULL)
            return 1;
    }
    return 0;
}

static inline u8 segment_flag_array_has_any(const u8 *flagsBySegment,
                                            u8 segmentCount)
{
    if (!flagsBySegment)
        return 0;

    for (u8 segmentId = 0; segmentId < segmentCount; segmentId++)
    {
        if (flagsBySegment[segmentId] != 0)
            return 1;
    }
    return 0;
}

/* Layout lazy-allocation helpers (defined later in GaugeLayout section). */
static u8 layout_ensure_segment_tileset_storage(const u32 ***segmentTilesetsBySegment,
                                                u8 segmentCount);
static u8 layout_ensure_segment_flag_storage(u8 **segmentFlags,
                                             u8 segmentCount,
                                             const u8 *defaultView,
                                             u8 defaultValue);
static u8 layout_ensure_cell_flag_storage(u8 **cellFlags,
                                          u8 cellCount,
                                          const u8 *defaultView,
                                          u8 defaultValue);
static void layout_free_optional_ptr(void **ptr,
                                     const void *defaultViewA,
                                     const void *defaultViewB,
                                     const void *defaultViewC);
static u8 layout_sync_optional_segment_tilesets_by_usage(u8 hasAny,
                                                         const u32 ***segmentTilesetsBySegment,
                                                         u8 segmentCount,
                                                         char *allocErrorLog);
static u8 layout_sync_optional_segment_flags_by_usage(u8 hasAny,
                                                      u8 **segmentFlags,
                                                      u8 segmentCount,
                                                      const u8 *defaultView,
                                                      u8 defaultValue,
                                                      char *allocErrorLog);
static void layout_copy_segment_tilesets(const u32 **destinationTilesets,
                                         const u32 * const *sourceTilesets,
                                         u8 segmentCount);
static void layout_copy_segment_bool_flags(u8 *destinationFlags,
                                           const u8 *sourceFlags,
                                           u8 segmentCount,
                                           const u8 *defaultView);

/**
 * Build bridge/break lookup tables (by fillIndex).
 *
 * Scans consecutive cells in fill order. At each segment boundary
 * (where segmentId changes between adjacent cells), applies these rules:
 *
 * - If outgoing segment has a BRIDGE tileset:
 *     -> last cell of the segment is flagged as bridgeEnd (shows bridge tile)
 *     -> cell before that is flagged as bridgeBreak (forced BREAK, index 44)
 * - If outgoing segment has NO BRIDGE tileset:
 *     -> last cell of the segment is flagged as bridgeBreak (forced BREAK)
 *
 * Example with segments A (has bridge) and B (no bridge):
 *   fillIndex:   0   1   2   3   4   5   6   7
 *   segment:     A   A   A   A   B   B   B   B
 *                          ^   ^
 *                   break(2)  bridge(3)
 *
 * Called by GaugeLayout_setFillForward/Reverse after fill order is set.
 */
static void build_bridge_luts(GaugeLayout *layout)
{
    const u8 hasBaseBridge =
        segment_tileset_array_has_any(layout->tilesetBridgeBySegment, layout->segmentCount);
    const u8 hasGainBridge =
        segment_tileset_array_has_any(layout->gainTilesetBridgeBySegment, layout->segmentCount);

    if (!hasBaseBridge && !hasGainBridge)
    {
        layout_free_optional_ptr((void **)&layout->bridgeEndByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->bridgeBreakByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->bridgeBreakBoundaryByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout->bridgeEndByFillIndex = (u8 *)s_zeroCellFlags;
        layout->bridgeBreakByFillIndex = (u8 *)s_zeroCellFlags;
        layout->bridgeBreakBoundaryByFillIndex = (u8 *)s_zeroCellFlags;
        return;
    }

    if (!layout_ensure_cell_flag_storage(&layout->bridgeEndByFillIndex,
                                         layout->length,
                                         s_zeroCellFlags,
                                         0) ||
        !layout_ensure_cell_flag_storage(&layout->bridgeBreakByFillIndex,
                                         layout->length,
                                         s_zeroCellFlags,
                                         0) ||
        !layout_ensure_cell_flag_storage(&layout->bridgeBreakBoundaryByFillIndex,
                                         layout->length,
                                         s_zeroCellFlags,
                                         0))
    {
        KLog("Gauge bridge LUT alloc failed");
        layout_free_optional_ptr((void **)&layout->bridgeEndByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->bridgeBreakByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->bridgeBreakBoundaryByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout->bridgeEndByFillIndex = (u8 *)s_zeroCellFlags;
        layout->bridgeBreakByFillIndex = (u8 *)s_zeroCellFlags;
        layout->bridgeBreakBoundaryByFillIndex = (u8 *)s_zeroCellFlags;
        return;
    }

    for (u8 i = 0; i < layout->length; i++)
    {
        layout->bridgeEndByFillIndex[i] = 0;
        layout->bridgeBreakByFillIndex[i] = 0;
        layout->bridgeBreakBoundaryByFillIndex[i] = 0;
    }

    /* Scan adjacent cell pairs looking for segment boundaries.
     * At each boundary we mark:
     *   bridgeEnd[i]   = 1  -> cell i is a bridge cell (last cell of its segment,
     *                          rendered with the bridge tileset instead of body)
     *   bridgeBreak[j] = 1  -> cell j is a BREAK cell (forced fully-filled when
     *                          gauge edge is past this point, so the bridge
     *                          transition tile has a clean left neighbor)
     *   bridgeBreakBoundary[j] = fillIndex of the bridge cell that j protects */
    for (u8 fillIndex = 0; (u16)(fillIndex + 1) < layout->length; fillIndex++)
    {
        const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
        const u8 nextCellIndex = layout->cellIndexByFillIndex[fillIndex + 1];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u8 nextSegId = layout->segmentIdByCell[nextCellIndex];

        /* Same segment: no boundary here */
        if (segmentId == nextSegId)
            continue;

        /* Segment boundary detected between fillIndex and fillIndex+1 */
        if (layout->tilesetBridgeBySegment[segmentId] || layout->gainTilesetBridgeBySegment[segmentId])
        {
            /* Bridge tileset exists: mark this cell as bridge end */
            layout->bridgeEndByFillIndex[fillIndex] = 1;
            if (fillIndex > 0)
            {
                /* Mark the cell before the bridge as BREAK (forced full) */
                const u8 prevCellIndex = layout->cellIndexByFillIndex[fillIndex - 1];
                if (layout->segmentIdByCell[prevCellIndex] == segmentId)
                {
                    layout->bridgeBreakByFillIndex[fillIndex - 1] = 1;
                    layout->bridgeBreakBoundaryByFillIndex[fillIndex - 1] = fillIndex;
                }
            }
        }
        else
        {
            /* No bridge tileset: treat boundary cell itself as BREAK */
            layout->bridgeBreakByFillIndex[fillIndex] = 1;
            layout->bridgeBreakBoundaryByFillIndex[fillIndex] = fillIndex;
        }
    }
}


/* =============================================================================
   GaugeLayout implementation
   ============================================================================= */

static void layout_zero_optional_flags(GaugeLayout *layout)
{
    layout->hasBlinkOff = 0;
    layout->hasGainBlinkOff = 0;
    layout->capStartEnabled = 0;
    layout->capEndEnabled = 0;
}

static void layout_set_optional_views_to_defaults(GaugeLayout *layout)
{
    layout->tilesetEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->tilesetTrailBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->tilesetBridgeBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->tilesetCapStartBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->tilesetCapEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->tilesetCapStartBreakBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->tilesetCapStartTrailBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->capEndBySegment = (u8 *)s_zeroSegmentFlags;

    layout->gainTilesetBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainTilesetEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainTilesetTrailBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainTilesetBridgeBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainTilesetCapStartBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainTilesetCapEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainTilesetCapStartBreakBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainTilesetCapStartTrailBySegment = (const u32 **)s_nullSegmentTilesets;

    layout->blinkOffTilesetBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->blinkOffTilesetEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->blinkOffTilesetTrailBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->blinkOffTilesetBridgeBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->blinkOffTilesetCapStartBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->blinkOffTilesetCapEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->blinkOffTilesetCapStartBreakBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->blinkOffTilesetCapStartTrailBySegment = (const u32 **)s_nullSegmentTilesets;

    layout->gainBlinkOffTilesetBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainBlinkOffTilesetEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainBlinkOffTilesetTrailBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainBlinkOffTilesetBridgeBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainBlinkOffTilesetCapStartBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainBlinkOffTilesetCapEndBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainBlinkOffTilesetCapStartBreakBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->gainBlinkOffTilesetCapStartTrailBySegment = (const u32 **)s_nullSegmentTilesets;

    layout->pipTilesetBySegment = (const u32 **)s_nullSegmentTilesets;
    layout->pipWidthBySegment = (u8 *)s_oneSegmentFlags;

    layout->pipIndexByFillIndex = (u8 *)s_invalidCellIndexes;
    layout->pipLocalTileByFillIndex = (u8 *)s_zeroCellFlags;
    layout->pipWidthByPipIndex = (u8 *)s_oneCellFlags;

    layout->bridgeEndByFillIndex = (u8 *)s_zeroCellFlags;
    layout->bridgeBreakByFillIndex = (u8 *)s_zeroCellFlags;
    layout->bridgeBreakBoundaryByFillIndex = (u8 *)s_zeroCellFlags;
}

static void layout_free_optional_ptr(void **ptr,
                                     const void *defaultViewA,
                                     const void *defaultViewB,
                                     const void *defaultViewC)
{
    if (!ptr || !*ptr)
        return;

    if (*ptr != defaultViewA &&
        *ptr != defaultViewB &&
        *ptr != defaultViewC)
    {
        MEM_free(*ptr);
    }
    *ptr = NULL;
}

static u8 layout_ensure_segment_tileset_storage(const u32 ***segmentTilesetsBySegment,
                                                u8 segmentCount)
{
    if (!segmentTilesetsBySegment)
        return 0;

    if (*segmentTilesetsBySegment != NULL &&
        *segmentTilesetsBySegment != s_nullSegmentTilesets)
    {
        return 1;
    }

    const u32 **allocatedTilesets = (const u32 **)gauge_alloc_bytes(
        (u16)(segmentCount * (u8)sizeof(const u32 *)));
    if (!allocatedTilesets)
    {
        *segmentTilesetsBySegment = (const u32 **)s_nullSegmentTilesets;
        return 0;
    }

    *segmentTilesetsBySegment = allocatedTilesets;
    return 1;
}

static u8 layout_ensure_segment_flag_storage(u8 **segmentFlags,
                                             u8 segmentCount,
                                             const u8 *defaultView,
                                             u8 defaultValue)
{
    if (!segmentFlags)
        return 0;

    if (*segmentFlags != NULL && *segmentFlags != defaultView)
        return 1;

    u8 *allocatedFlags = (u8 *)gauge_alloc_bytes((u16)segmentCount);
    if (!allocatedFlags)
    {
        *segmentFlags = (u8 *)defaultView;
        return 0;
    }

    *segmentFlags = allocatedFlags;

    if (defaultValue == 0)
    {
        memset(*segmentFlags, 0, segmentCount);
    }
    else
    {
        for (u8 i = 0; i < segmentCount; i++)
            (*segmentFlags)[i] = defaultValue;
    }
    return 1;
}

static u8 layout_ensure_cell_flag_storage(u8 **cellFlags,
                                          u8 cellCount,
                                          const u8 *defaultView,
                                          u8 defaultValue)
{
    if (!cellFlags)
        return 0;

    if (*cellFlags != NULL && *cellFlags != defaultView)
        return 1;

    u8 *allocatedFlags = (u8 *)gauge_alloc_bytes((u16)cellCount);
    if (!allocatedFlags)
    {
        *cellFlags = (u8 *)defaultView;
        return 0;
    }

    *cellFlags = allocatedFlags;

    if (defaultValue == 0)
    {
        memset(*cellFlags, 0, cellCount);
    }
    else if (defaultValue == CACHE_INVALID_U8)
    {
        memset(*cellFlags, CACHE_INVALID_U8, cellCount);
    }
    else
    {
        for (u8 i = 0; i < cellCount; i++)
            (*cellFlags)[i] = defaultValue;
    }
    return 1;
}

static u8 layout_sync_optional_segment_tilesets_by_usage(u8 hasAny,
                                                         const u32 ***segmentTilesetsBySegment,
                                                         u8 segmentCount,
                                                         char *allocErrorLog)
{
    if (!segmentTilesetsBySegment)
        return 0;

    if (!hasAny)
    {
        layout_free_optional_ptr((void **)segmentTilesetsBySegment, s_nullSegmentTilesets, NULL, NULL);
        *segmentTilesetsBySegment = (const u32 **)s_nullSegmentTilesets;
        return 1;
    }

    if (!layout_ensure_segment_tileset_storage(segmentTilesetsBySegment, segmentCount))
    {
        if (allocErrorLog)
            KLog(allocErrorLog);
        *segmentTilesetsBySegment = (const u32 **)s_nullSegmentTilesets;
        return 0;
    }

    return 1;
}

static u8 layout_sync_optional_segment_flags_by_usage(u8 hasAny,
                                                      u8 **segmentFlags,
                                                      u8 segmentCount,
                                                      const u8 *defaultView,
                                                      u8 defaultValue,
                                                      char *allocErrorLog)
{
    if (!segmentFlags)
        return 0;

    if (!hasAny)
    {
        layout_free_optional_ptr((void **)segmentFlags, defaultView, NULL, NULL);
        *segmentFlags = (u8 *)defaultView;
        return 1;
    }

    if (!layout_ensure_segment_flag_storage(segmentFlags,
                                            segmentCount,
                                            defaultView,
                                            defaultValue))
    {
        if (allocErrorLog)
            KLog(allocErrorLog);
        *segmentFlags = (u8 *)defaultView;
        return 0;
    }

    return 1;
}

static void layout_copy_segment_tilesets(const u32 **destinationTilesets,
                                         const u32 * const *sourceTilesets,
                                         u8 segmentCount)
{
    if (!destinationTilesets || destinationTilesets == s_nullSegmentTilesets)
        return;

    for (u8 segmentId = 0; segmentId < segmentCount; segmentId++)
    {
        destinationTilesets[segmentId] = sourceTilesets ? sourceTilesets[segmentId] : NULL;
    }
}

static void layout_copy_segment_bool_flags(u8 *destinationFlags,
                                           const u8 *sourceFlags,
                                           u8 segmentCount,
                                           const u8 *defaultView)
{
    if (!destinationFlags || destinationFlags == defaultView)
        return;

    for (u8 segmentId = 0; segmentId < segmentCount; segmentId++)
    {
        destinationFlags[segmentId] = sourceFlags ? (sourceFlags[segmentId] ? 1 : 0) : 0;
    }
}

static void layout_free_buffers(GaugeLayout *layout)
{
    if (!layout)
        return;

    gauge_free_ptr((void **)&layout->segmentIdByCell);
    gauge_free_ptr((void **)&layout->fillIndexByCell);
    gauge_free_ptr((void **)&layout->cellIndexByFillIndex);
    gauge_free_ptr((void **)&layout->tilemapPosByCell);

    gauge_free_ptr((void **)&layout->tilesetBySegment);

    layout_free_optional_ptr((void **)&layout->tilesetEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->tilesetTrailBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->tilesetBridgeBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->tilesetCapStartBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->tilesetCapEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->tilesetCapStartBreakBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->tilesetCapStartTrailBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->capEndBySegment, s_zeroSegmentFlags, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->gainTilesetBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainTilesetEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainTilesetTrailBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainTilesetBridgeBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainTilesetCapStartBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainTilesetCapEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainTilesetCapStartBreakBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainTilesetCapStartTrailBySegment, s_nullSegmentTilesets, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->blinkOffTilesetBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->blinkOffTilesetEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->blinkOffTilesetTrailBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->blinkOffTilesetBridgeBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->blinkOffTilesetCapStartBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->blinkOffTilesetCapEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->blinkOffTilesetCapStartBreakBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->blinkOffTilesetCapStartTrailBySegment, s_nullSegmentTilesets, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetTrailBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetBridgeBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetCapStartBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetCapEndBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetCapStartBreakBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->gainBlinkOffTilesetCapStartTrailBySegment, s_nullSegmentTilesets, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->pipTilesetBySegment, s_nullSegmentTilesets, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipWidthBySegment, s_oneSegmentFlags, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->pipIndexByFillIndex, s_invalidCellIndexes, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipLocalTileByFillIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipWidthByPipIndex, s_oneCellFlags, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->bridgeEndByFillIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->bridgeBreakByFillIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->bridgeBreakBoundaryByFillIndex, s_zeroCellFlags, NULL, NULL);

    layout->length = 0;
    layout->segmentCount = 0;
    layout->pipCount = 0;
    layout_set_optional_views_to_defaults(layout);
    layout_zero_optional_flags(layout);
}

static u8 layout_alloc_buffers(GaugeLayout *layout, u8 length, u8 segmentCount)
{
    const u16 cellBytes = (u16)length;
    const u16 segPtrBytes = (u16)(segmentCount * (u8)sizeof(const u32 *));

    layout->segmentIdByCell = (u8 *)gauge_alloc_bytes(cellBytes);
    layout->fillIndexByCell = (u8 *)gauge_alloc_bytes(cellBytes);
    layout->cellIndexByFillIndex = (u8 *)gauge_alloc_bytes(cellBytes);
    layout->tilemapPosByCell = (Vect2D_u16 *)gauge_alloc_bytes((u16)(length * (u8)sizeof(Vect2D_u16)));

    layout->tilesetBySegment = (const u32 **)gauge_alloc_bytes(segPtrBytes);

    if (!layout->segmentIdByCell || !layout->fillIndexByCell || !layout->cellIndexByFillIndex ||
        !layout->tilemapPosByCell || !layout->tilesetBySegment)
    {
        layout_free_buffers(layout);
        KLog("Gauge layout alloc failed");
        return 0;
    }

    layout->length = length;
    layout->segmentCount = segmentCount;
    layout->pipCount = 0;
    layout_set_optional_views_to_defaults(layout);

    return 1;
}

void GaugeLayout_initEx(GaugeLayout *layout,
                        u8 length,
                        GaugeFillDirection fillDirection,
                        const u32 * const *bodyTilesets,
                        const u32 * const *endTilesets,
                        const u32 * const *trailTilesets,
                        const u32 * const *bridgeTilesets,
                        const u8 *segmentIdByCell,
                        GaugeOrientation orientation,
                        u8 palette,
                        u8 priority,
                        u8 verticalFlip,
                        u8 horizontalFlip)
{
    if (!layout)
        return;

    /* Validate and clamp length */
    if (length == 0) length = 1;
    if (length > GAUGE_MAX_LENGTH) length = GAUGE_MAX_LENGTH;

    /* Segment count = highest segment id present in cell map + 1 */
    u8 maxSegmentId = 0;
    if (segmentIdByCell)
    {
        for (u8 i = 0; i < length; i++)
        {
            if (segmentIdByCell[i] > maxSegmentId)
                maxSegmentId = segmentIdByCell[i];
        }
    }
    if (maxSegmentId >= GAUGE_MAX_SEGMENTS)
        maxSegmentId = (u8)(GAUGE_MAX_SEGMENTS - 1);
    const u8 segmentCount = (u8)(maxSegmentId + 1);

    /* Rebuild dynamic buffers (layout must not be retained while rebuilt). */
    if (layout->refCount != 0)
    {
        KLog_U1("GaugeLayout_initEx refused, refCount: ", layout->refCount);
        return;
    }
    layout_free_buffers(layout);
    if (!layout_alloc_buffers(layout, length, segmentCount))
        return;

    layout->fillOffset = 0;
    layout->refCount = 0;

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(endTilesets, layout->segmentCount),
        &layout->tilesetEndBySegment,
        layout->segmentCount,
        "Gauge layout optional alloc failed: end");

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(trailTilesets, layout->segmentCount),
        &layout->tilesetTrailBySegment,
        layout->segmentCount,
        "Gauge layout optional alloc failed: trail");

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(bridgeTilesets, layout->segmentCount),
        &layout->tilesetBridgeBySegment,
        layout->segmentCount,
        "Gauge layout optional alloc failed: bridge");

    /* Copy mandatory BODY tilesets. */
    for (u8 i = 0; i < layout->segmentCount; i++)
    {
        layout->tilesetBySegment[i] = bodyTilesets ? bodyTilesets[i] : NULL;
    }
    layout_copy_segment_tilesets(layout->tilesetEndBySegment, endTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->tilesetTrailBySegment, trailTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->tilesetBridgeBySegment, bridgeTilesets, layout->segmentCount);

    /* Copy segment IDs per cell (NULL = all segment 0). */
    for (u8 i = 0; i < layout->length; i++)
    {
        u8 segmentId = segmentIdByCell ? segmentIdByCell[i] : 0;
        if (segmentId >= layout->segmentCount)
            segmentId = 0;
        layout->segmentIdByCell[i] = segmentId;
    }

    /* Set fill direction */
    if (fillDirection == GAUGE_FILL_REVERSE)
        GaugeLayout_setFillReverse(layout);
    else
        GaugeLayout_setFillForward(layout);

    /* Set visual properties */
    layout->orientation = orientation;
    layout->palette = palette;
    layout->priority = priority ? 1 : 0;
    layout->verticalFlip = verticalFlip ? 1 : 0;
    layout->horizontalFlip = horizontalFlip ? 1 : 0;

    /* Initialize cached flags (computed later by setters/build) */
    layout_zero_optional_flags(layout);
}

void GaugeLayout_init(GaugeLayout *layout,
                      u8 length,
                      GaugeFillDirection fillDirection,
                      const u32 * const *tilesets,
                      const u8 *segmentIdByCell,
                      GaugeOrientation orientation,
                      u8 palette,
                      u8 priority,
                      u8 verticalFlip,
                      u8 horizontalFlip)
{
    GaugeLayout_initEx(layout, length, fillDirection, tilesets, NULL,
                       NULL, NULL, segmentIdByCell, orientation, palette,
                       priority, verticalFlip, horizontalFlip);
}

/** Set fill direction to forward: cell 0 fills first (left-to-right / top-to-bottom). */
void GaugeLayout_setFillForward(GaugeLayout *layout)
{
    if (!layout)
        return;

    for (u8 c = 0; c < layout->length; c++)
    {
        layout->fillIndexByCell[c] = c;
        layout->cellIndexByFillIndex[c] = c;
    }

    build_bridge_luts(layout);
    build_pip_luts(layout);
}

/** Set pixel offset for fill computation (used for multi-part gauge windows). */
void GaugeLayout_setFillOffset(GaugeLayout *layout, u16 fillOffsetPixels)
{
    if (!layout)
        return;

    layout->fillOffset = fillOffsetPixels;
}

/** Set fill direction to reverse: last cell fills first (right-to-left / bottom-to-top). */
void GaugeLayout_setFillReverse(GaugeLayout *layout)
{
    if (!layout)
        return;

    for (u8 c = 0; c < layout->length; c++)
    {
        const u8 fillIdx = (u8)(layout->length - 1 - c);
        layout->fillIndexByCell[c] = fillIdx;
        layout->cellIndexByFillIndex[fillIdx] = c;
    }

    build_bridge_luts(layout);
    build_pip_luts(layout);
}

/**
 * Create a mirrored copy of a layout (for P2/opponent side).
 *
 * Reverses segment order, swaps fill direction, and sets appropriate flip flags.
 * Copies all tilesets by reference (no deep copy needed since tilesets are in ROM).
 */
void GaugeLayout_makeMirror(GaugeLayout *dst, const GaugeLayout *src)
{
    if (!dst || !src)
        return;

    /* Rebuild target layout with reversed segment map and same segment arrays. */
    u8 reversedSegmentIds[GAUGE_MAX_LENGTH];
    for (u8 c = 0; c < src->length; c++)
    {
        const u8 srcCell = (u8)(src->length - 1 - c);
        reversedSegmentIds[c] = src->segmentIdByCell[srcCell];
    }

    GaugeLayout_initEx(dst,
                       src->length,
                       (src->fillIndexByCell[0] == 0) ? GAUGE_FILL_REVERSE : GAUGE_FILL_FORWARD,
                       src->tilesetBySegment,
                       src->tilesetEndBySegment,
                       src->tilesetTrailBySegment,
                       src->tilesetBridgeBySegment,
                       reversedSegmentIds,
                       src->orientation,
                       src->palette,
                       src->priority,
                       src->verticalFlip,
                       src->horizontalFlip);
    dst->fillOffset = src->fillOffset;

    /* Copy optional style groups through public setters (lazy allocations). */
    GaugeLayout_setCaps(dst,
                        src->tilesetCapStartBySegment,
                        src->tilesetCapEndBySegment,
                        src->tilesetCapStartBreakBySegment,
                        src->tilesetCapStartTrailBySegment,
                        src->capEndBySegment);
    GaugeLayout_setBlinkOff(dst,
                            src->blinkOffTilesetBySegment,
                            src->blinkOffTilesetEndBySegment,
                            src->blinkOffTilesetTrailBySegment,
                            src->blinkOffTilesetBridgeBySegment,
                            src->blinkOffTilesetCapStartBySegment,
                            src->blinkOffTilesetCapEndBySegment,
                            src->blinkOffTilesetCapStartBreakBySegment,
                            src->blinkOffTilesetCapStartTrailBySegment);
    GaugeLayout_setGainTrail(dst,
                             src->gainTilesetBySegment,
                             src->gainTilesetEndBySegment,
                             src->gainTilesetTrailBySegment,
                             src->gainTilesetBridgeBySegment,
                             src->gainTilesetCapStartBySegment,
                             src->gainTilesetCapEndBySegment,
                             src->gainTilesetCapStartBreakBySegment,
                             src->gainTilesetCapStartTrailBySegment);
    GaugeLayout_setGainBlinkOff(dst,
                                src->gainBlinkOffTilesetBySegment,
                                src->gainBlinkOffTilesetEndBySegment,
                                src->gainBlinkOffTilesetTrailBySegment,
                                src->gainBlinkOffTilesetBridgeBySegment,
                                src->gainBlinkOffTilesetCapStartBySegment,
                                src->gainBlinkOffTilesetCapEndBySegment,
                                src->gainBlinkOffTilesetCapStartBreakBySegment,
                                src->gainBlinkOffTilesetCapStartTrailBySegment);
    GaugeLayout_setPipStyles(dst,
                             src->pipTilesetBySegment,
                             src->pipWidthBySegment);

    /* Opposite fill direction from source */
    if (src->fillIndexByCell[0] == 0)
        GaugeLayout_setFillReverse(dst);
    else
        GaugeLayout_setFillForward(dst);


    /* Copy base visual properties */
    dst->orientation = src->orientation;
    dst->palette = src->palette;
    dst->priority = src->priority;

    /* Set flip based on orientation */
    if (src->orientation == GAUGE_ORIENT_HORIZONTAL)
    {
        dst->horizontalFlip = 1;
        dst->verticalFlip = 0;
    }
    else
    {
        dst->horizontalFlip = 0;
        dst->verticalFlip = 1;
    }

}

void GaugeLayout_retain(GaugeLayout *layout)
{
    if (!layout)
        return;
    layout->refCount++;
}

void GaugeLayout_release(GaugeLayout *layout)
{
    if (!layout)
        return;

    if (layout->refCount > 0)
        layout->refCount--;

    if (layout->refCount == 0)
        layout_free_buffers(layout);
}

void GaugeLayout_setCaps(GaugeLayout *layout,
                         const u32 * const *capStartTilesets,
                         const u32 * const *capEndTilesets,
                         const u32 * const *capStartBreakTilesets,
                         const u32 * const *capStartTrailTilesets,
                         const u8 *capEndBySegment)
{
    if (!layout)
        return;

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(capStartTilesets, layout->segmentCount),
        &layout->tilesetCapStartBySegment,
        layout->segmentCount,
        "Gauge capStart alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(capEndTilesets, layout->segmentCount),
        &layout->tilesetCapEndBySegment,
        layout->segmentCount,
        "Gauge capEnd alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(capStartBreakTilesets, layout->segmentCount),
        &layout->tilesetCapStartBreakBySegment,
        layout->segmentCount,
        "Gauge capStartBreak alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(capStartTrailTilesets, layout->segmentCount),
        &layout->tilesetCapStartTrailBySegment,
        layout->segmentCount,
        "Gauge capStartTrail alloc failed");
    layout_sync_optional_segment_flags_by_usage(
        segment_flag_array_has_any(capEndBySegment, layout->segmentCount),
        &layout->capEndBySegment,
        layout->segmentCount,
        s_zeroSegmentFlags,
        0,
        "Gauge capEnd flags alloc failed");

    layout_copy_segment_tilesets(layout->tilesetCapStartBySegment, capStartTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->tilesetCapEndBySegment, capEndTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->tilesetCapStartBreakBySegment, capStartBreakTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->tilesetCapStartTrailBySegment, capStartTrailTilesets, layout->segmentCount);
    layout_copy_segment_bool_flags(layout->capEndBySegment,
                                   capEndBySegment,
                                   layout->segmentCount,
                                   s_zeroSegmentFlags);

    /* Update cached cap flags */
    detect_caps_enabled(layout, &layout->capStartEnabled, &layout->capEndEnabled);
}

void GaugeLayout_setBlinkOff(GaugeLayout *layout,
                             const u32 * const *blinkOffBodyTilesets,
                             const u32 * const *blinkOffEndTilesets,
                             const u32 * const *blinkOffTrailTilesets,
                             const u32 * const *blinkOffBridgeTilesets,
                             const u32 * const *blinkOffCapStartTilesets,
                             const u32 * const *blinkOffCapEndTilesets,
                             const u32 * const *blinkOffCapStartBreakTilesets,
                             const u32 * const *blinkOffCapStartTrailTilesets)
{
    if (!layout)
        return;

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffBodyTilesets, layout->segmentCount),
        &layout->blinkOffTilesetBySegment,
        layout->segmentCount,
        "Gauge blink-off BODY alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffEndTilesets, layout->segmentCount),
        &layout->blinkOffTilesetEndBySegment,
        layout->segmentCount,
        "Gauge blink-off END alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffTrailTilesets, layout->segmentCount),
        &layout->blinkOffTilesetTrailBySegment,
        layout->segmentCount,
        "Gauge blink-off TRAIL alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffBridgeTilesets, layout->segmentCount),
        &layout->blinkOffTilesetBridgeBySegment,
        layout->segmentCount,
        "Gauge blink-off BRIDGE alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffCapStartTilesets, layout->segmentCount),
        &layout->blinkOffTilesetCapStartBySegment,
        layout->segmentCount,
        "Gauge blink-off CAP START alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffCapEndTilesets, layout->segmentCount),
        &layout->blinkOffTilesetCapEndBySegment,
        layout->segmentCount,
        "Gauge blink-off CAP END alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffCapStartBreakTilesets, layout->segmentCount),
        &layout->blinkOffTilesetCapStartBreakBySegment,
        layout->segmentCount,
        "Gauge blink-off CAP START BREAK alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(blinkOffCapStartTrailTilesets, layout->segmentCount),
        &layout->blinkOffTilesetCapStartTrailBySegment,
        layout->segmentCount,
        "Gauge blink-off CAP START TRAIL alloc failed");

    layout_copy_segment_tilesets(layout->blinkOffTilesetBySegment, blinkOffBodyTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->blinkOffTilesetEndBySegment, blinkOffEndTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->blinkOffTilesetTrailBySegment, blinkOffTrailTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->blinkOffTilesetBridgeBySegment, blinkOffBridgeTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->blinkOffTilesetCapStartBySegment, blinkOffCapStartTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->blinkOffTilesetCapEndBySegment, blinkOffCapEndTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->blinkOffTilesetCapStartBreakBySegment, blinkOffCapStartBreakTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->blinkOffTilesetCapStartTrailBySegment, blinkOffCapStartTrailTilesets, layout->segmentCount);

    layout->hasBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_DAMAGE);
}


/* =============================================================================
   GaugeLogic implementation (internal -- not exposed in gauge.h)
   ============================================================================= */

/* Forward declaration: GaugeLogic_init calls GaugeLogic_initWithAnim */
static void GaugeLogic_initWithAnim(GaugeLogic *logic,
                                    u16 maxValue,
                                    u16 maxFillPixels,
                                    const u16 *valueToPixelsLUT,
                                    u8 trailEnabled,
                                    u16 initialValue,
                                    u8 valueAnimEnabled,
                                    u8 valueAnimShift,
                                    u8 trailAnimShift,
                                    u8 blinkShift);

static void GaugeLogic_init(GaugeLogic *logic,
                     u16 maxValue,
                     u16 maxFillPixels,
                     const u16 *valueToPixelsLUT,
                     u8 trailEnabled,
                     u16 initialValue)
{
    GaugeLogic_initWithAnim(logic, maxValue, maxFillPixels, valueToPixelsLUT,
                            trailEnabled, initialValue,
                            0,  /* valueAnimEnabled = instant */
                            GAUGE_DEFAULT_VALUE_ANIM_SHIFT,
                            GAUGE_DEFAULT_TRAIL_ANIM_SHIFT,
                            GAUGE_DEFAULT_BLINK_SHIFT);
}

static void GaugeLogic_initWithAnim(GaugeLogic *logic,
                                    u16 maxValue,
                                    u16 maxFillPixels,
                                    const u16 *valueToPixelsLUT,
                                    u8 trailEnabled,
                                    u16 initialValue,
                                    u8 valueAnimEnabled,
                             u8 valueAnimShift,
                             u8 trailAnimShift,
                             u8 blinkShift)
{
    /* Core state */
    logic->maxValue = maxValue;
    logic->currentValue = (initialValue > maxValue) ? maxValue : initialValue;

    logic->maxFillPixels = maxFillPixels;
    logic->valueToPixelsLUT = valueToPixelsLUT;

    /* Compute initial pixel values */
    const u16 initialPixels = value_to_pixels(logic, logic->currentValue);
    logic->valueTargetPixels = (initialPixels > maxFillPixels) ? maxFillPixels : initialPixels;
    logic->valuePixels = logic->valueTargetPixels;
    logic->trailPixels = logic->valuePixels;

    /* Timers */
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->blinkTimer = 0;

    /* Animation config */
    logic->valueAnimEnabled = valueAnimEnabled ? 1 : 0;
    logic->valueAnimShift = (valueAnimShift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : valueAnimShift;
    logic->trailAnimShift = (trailAnimShift == 0) ? GAUGE_DEFAULT_TRAIL_ANIM_SHIFT : trailAnimShift;
    logic->blinkShift = (blinkShift == 0) ? GAUGE_DEFAULT_BLINK_SHIFT : blinkShift;

    logic->trailEnabled = trailEnabled ? 1 : 0;
    logic->trailMode = GAUGE_TRAIL_NONE;
    logic->lastTrailMode = GAUGE_TRAIL_NONE;

    /* Initialize render cache to force first render */
    logic->lastValuePixels = CACHE_INVALID_U16;
    logic->lastTrailPixelsRendered = CACHE_INVALID_U16;
    logic->lastBlinkOn = CACHE_INVALID_U8;
    logic->needUpdate = 1;

    /* If trail disabled, ensure consistent state */
    if (!logic->trailEnabled)
    {
        logic->trailPixels = logic->valuePixels;
        logic->holdFramesRemaining = 0;
        logic->blinkFramesRemaining = 0;
        logic->trailMode = GAUGE_TRAIL_NONE;
    }
}

/**
 * Build PIP lookup tables (by fillIndex).
 *
 * PIP boundaries are defined by contiguous runs of segment IDs in fill order.
 * For a given segment style, one logical pip spans pipWidthBySegment[segmentId] cells.
 * A run can therefore contain multiple pips with the same style.
 */
static void build_pip_luts(GaugeLayout *layout)
{
    const u8 hasPipStyles =
        segment_tileset_array_has_any(layout->pipTilesetBySegment, layout->segmentCount);

    if (!hasPipStyles)
    {
        layout->pipCount = 0;
        layout_free_optional_ptr((void **)&layout->pipIndexByFillIndex, s_invalidCellIndexes, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipLocalTileByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipWidthByPipIndex, s_oneCellFlags, NULL, NULL);
        layout->pipIndexByFillIndex = (u8 *)s_invalidCellIndexes;
        layout->pipLocalTileByFillIndex = (u8 *)s_zeroCellFlags;
        layout->pipWidthByPipIndex = (u8 *)s_oneCellFlags;
        return;
    }

    if (!layout_ensure_cell_flag_storage(&layout->pipIndexByFillIndex,
                                         layout->length,
                                         s_invalidCellIndexes,
                                         CACHE_INVALID_U8) ||
        !layout_ensure_cell_flag_storage(&layout->pipLocalTileByFillIndex,
                                         layout->length,
                                         s_zeroCellFlags,
                                         0) ||
        !layout_ensure_cell_flag_storage(&layout->pipWidthByPipIndex,
                                         layout->length,
                                         s_oneCellFlags,
                                         1))
    {
        KLog("Gauge pip LUT alloc failed");
        layout->pipCount = 0;
        layout_free_optional_ptr((void **)&layout->pipIndexByFillIndex, s_invalidCellIndexes, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipLocalTileByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipWidthByPipIndex, s_oneCellFlags, NULL, NULL);
        layout->pipIndexByFillIndex = (u8 *)s_invalidCellIndexes;
        layout->pipLocalTileByFillIndex = (u8 *)s_zeroCellFlags;
        layout->pipWidthByPipIndex = (u8 *)s_oneCellFlags;
        return;
    }

    layout->pipCount = 0;

    for (u8 i = 0; i < layout->length; i++)
    {
        layout->pipIndexByFillIndex[i] = CACHE_INVALID_U8;
        layout->pipLocalTileByFillIndex[i] = 0;
        layout->pipWidthByPipIndex[i] = 1;
    }

    u8 fillIndex = 0;
    while (fillIndex < layout->length && layout->pipCount < layout->length)
    {
        const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        u8 styleWidth = layout->pipWidthBySegment[segmentId];
        if (styleWidth == 0)
            styleWidth = 1;

        /* Measure contiguous run for this style in fill order. */
        u8 runLength = 0;
        while ((u16)(fillIndex + runLength) < layout->length)
        {
            const u8 runCellIndex = layout->cellIndexByFillIndex[fillIndex + runLength];
            if (layout->segmentIdByCell[runCellIndex] != segmentId)
                break;
            runLength++;
        }
        if (runLength == 0)
            runLength = 1;

        /* Split the run into logical pips of styleWidth cells. */
        u8 remaining = runLength;
        while (remaining > 0 && layout->pipCount < layout->length)
        {
            const u8 pipWidth = (remaining >= styleWidth) ? styleWidth : remaining;
            layout->pipWidthByPipIndex[layout->pipCount] = pipWidth;

            for (u8 local = 0; local < pipWidth; local++)
            {
                layout->pipIndexByFillIndex[fillIndex + local] = layout->pipCount;
                layout->pipLocalTileByFillIndex[fillIndex + local] = local;
            }

            fillIndex = (u8)(fillIndex + pipWidth);
            remaining = (u8)(remaining - pipWidth);
            layout->pipCount++;
        }
    }
}

/**
 * GaugeLogic_tick -- Advance the logic state machine by one frame.
 *
 * State machine transitions (per frame):
 *
 *   [IDLE] -----(Gauge_decrease)-----> [HOLD]
 *     ^                                  |
 *     |                          holdFramesRemaining--
 *     |                                  |
 *     |                          holdFramesRemaining==0
 *     |                                  v
 *     |                               [BLINK]
 *     |                                  |
 *     |                         blinkFramesRemaining--
 *     |                         blinkTimer++
 *     |                                  |
 *     |                         blinkFramesRemaining==0
 *     |                                  v
 *     |                              [SHRINK]
 *     |                                  |
 *     |                        trailPixels -= step
 *     |                                  |
 *     |                        trailPixels <= valuePixels
 *     +----------------------------------+
 *
 * Value animation (independent of trail):
 *   valuePixels converges toward valueTargetPixels using exponential decay:
 *   step = (distance >> valueAnimShift) + 1
 *
 * OPTIMIZATION NOTES:
 * - Early returns for common cases (no animation needed)
 * - Shift operations instead of division
 * - Saturating subtract via arithmetic masking (branchless)
 * - Minimal branches in hot paths
 *
 * Cost: ~50-100 cycles (mostly conditionals, no memory-intensive ops)
 */
static void GaugeLogic_tick(GaugeLogic *logic)
{
    const u8 gainMode = (logic->trailMode == GAUGE_TRAIL_GAIN);

    /* --- Value animation (if enabled) --- */
    if (logic->valueAnimEnabled && logic->valuePixels != logic->valueTargetPixels)
    {
        if (logic->valuePixels < logic->valueTargetPixels)
        {
            /* Increasing (heal animation).
             * In gain mode, value waits until hold+blink complete before catching up. */
            if (!(gainMode && (logic->holdFramesRemaining > 0 || logic->blinkFramesRemaining > 0)))
            {
                const u16 diff = (u16)(logic->valueTargetPixels - logic->valuePixels);
                const u16 step = CALC_ANIM_STEP(diff, logic->valueAnimShift);
                logic->valuePixels = (u16)(logic->valuePixels + step);

                if (logic->valuePixels > logic->valueTargetPixels)
                    logic->valuePixels = logic->valueTargetPixels;
                if (logic->valuePixels > logic->maxFillPixels)
                    logic->valuePixels = logic->maxFillPixels;
            }
        }
        else
        {
            /* Decreasing (damage animation)
             * Saturating subtract: if result wraps negative, mask zeroes it out.
             * Cost: ~10 cycles (sub + asr + not + and) vs ~14 cycles (cmp + branch + clr) */
            const u16 diff = (u16)(logic->valuePixels - logic->valueTargetPixels);
            const u16 step = CALC_ANIM_STEP(diff, logic->valueAnimShift);
            s16 result = (s16)logic->valuePixels - (s16)step;
            logic->valuePixels = (u16)(result & ~(result >> 15));

            if (logic->valuePixels < logic->valueTargetPixels)
                logic->valuePixels = logic->valueTargetPixels;
        }
    }


    /* --- Trail handling --- */
    if (!logic->trailEnabled)
    {
        /* Trail disabled: keep in sync with value */
        logic->trailPixels = logic->valuePixels;
        logic->trailMode = GAUGE_TRAIL_NONE;
        return;
    }

    if (gainMode)
    {
        logic->trailPixels = logic->valueTargetPixels;

        /* Hold phase: trail stays at target */
        if (logic->holdFramesRemaining > 0)
        {
            logic->holdFramesRemaining--;
            return;
        }

        /* Blink phase: trail stays at target */
        if (logic->blinkFramesRemaining > 0)
        {
            logic->blinkTimer++;
            logic->blinkFramesRemaining--;
            if (logic->blinkFramesRemaining == 0)
                logic->blinkTimer = 0;
            return;
        }

        if (logic->valuePixels >= logic->valueTargetPixels)
        {
            logic->trailPixels = logic->valuePixels;
            logic->trailMode = GAUGE_TRAIL_NONE;
        }
        return;
    }

    /* Hold phase: trail stays at previous position */
    if (logic->holdFramesRemaining > 0)
    {
        logic->holdFramesRemaining--;
        return;
    }

    /* Blink phase: trail stays at previous position */
    if (logic->blinkFramesRemaining > 0)
    {
        logic->blinkTimer++;
        logic->blinkFramesRemaining--;
        if (logic->blinkFramesRemaining == 0)
            logic->blinkTimer = 0;
        return;
    }

    /* Trail shrink phase: move toward value.
     * Uses saturating subtract to avoid underflow without branch. */
    if (logic->trailPixels > logic->valuePixels)
    {
        const u16 diff = (u16)(logic->trailPixels - logic->valuePixels);
        const u16 step = CALC_ANIM_STEP(diff, logic->trailAnimShift);
        s16 result = (s16)logic->trailPixels - (s16)step;
        logic->trailPixels = (u16)(result & ~(result >> 15));

        if (logic->trailPixels < logic->valuePixels)
            logic->trailPixels = logic->valuePixels;
    }
    else
    {
        logic->trailPixels = logic->valuePixels;
    }

    /* Stop blink as soon as trail has caught up to value */
    if (logic->trailPixels == logic->valuePixels)
    {
        logic->blinkFramesRemaining = 0;
        logic->blinkTimer = 0;
        if (logic->trailMode == GAUGE_TRAIL_DAMAGE)
            logic->trailMode = GAUGE_TRAIL_NONE;
    }
}


/* =============================================================================
   GaugePart internals
   ============================================================================= */

/* =============================================================================
   Dynamic VRAM mode implementation
   ============================================================================= */

/**
 * Initialize dynamic mode VRAM allocation.
 *
 * VRAM layout (allocated sequentially from vramBase):
 *
 *   Per used segment (repeated for each segment with a body tileset):
 *   +-------------------+
 *   | Empty tile (0,0)  |  1 tile  - value=0, trail=0
 *   +-------------------+
 *   | Full value (8,8)  |  1 tile  - value=8, trail=8
 *   +-------------------+
 *   | Full trail (0,8)  |  1 tile  - value=0, trail=8  (only if trailEnabled)
 *   +-------------------+
 *
 *   Partial tiles (1 per GaugePart, shared across all segments):
 *   +-------------------+
 *   | Partial value     |  1 tile  - streamed on demand (also "both" case)
 *   +-------------------+
 *   | Partial trail     |  1 tile  - streamed on demand (only if trailEnabled)
 *   +-------------------+
 *   | Partial END       |  1 tile  - cap tile (only if any segment has END)
 *   +-------------------+
 *   | Partial trail 2nd |  1 tile  - 2nd trail break (only if trail + END)
 *   +-------------------+
 *
 * Standard tiles are pre-loaded once at init (preload_dynamic_standard_tiles).
 * Partial tiles are streamed per-frame only when their content changes.
 *
 * @param dyn          Dynamic mode data to initialize
 * @param layout       Layout configuration (segments, tilesets)
 * @param vramBase     Base VRAM tile index
 * @param trailEnabled Whether trail rendering is active
 */
static void free_dynamic_buffers(GaugeDynamic *dyn)
{
    if (!dyn)
        return;
    gauge_free_ptr((void **)&dyn->vramTileEmpty);
    gauge_free_ptr((void **)&dyn->vramTileFullValue);
    gauge_free_ptr((void **)&dyn->vramTileFullTrail);
    gauge_free_ptr((void **)&dyn->vramTileBridge);
    gauge_free_ptr((void **)&dyn->cachedFillIndexBridge);
    gauge_free_ptr((void **)&dyn->cellCurrentTileIndex);
    gauge_free_ptr((void **)&dyn->cellValid);
    dyn->segmentCount = 0;
    dyn->cellCount = 0;
}

static u8 alloc_dynamic_buffers(GaugeDynamic *dyn, u8 segmentCount, u8 cellCount)
{
    free_dynamic_buffers(dyn);
    dyn->vramTileEmpty = (u16 *)gauge_alloc_bytes((u16)(segmentCount * (u8)sizeof(u16)));
    dyn->vramTileFullValue = (u16 *)gauge_alloc_bytes((u16)(segmentCount * (u8)sizeof(u16)));
    dyn->vramTileFullTrail = (u16 *)gauge_alloc_bytes((u16)(segmentCount * (u8)sizeof(u16)));
    dyn->vramTileBridge = (u16 *)gauge_alloc_bytes((u16)(segmentCount * (u8)sizeof(u16)));
    dyn->cachedFillIndexBridge = (u8 *)gauge_alloc_bytes(segmentCount);
    dyn->cellCurrentTileIndex = (u16 *)gauge_alloc_bytes((u16)(cellCount * (u8)sizeof(u16)));
    dyn->cellValid = (u8 *)gauge_alloc_bytes(cellCount);
    if (!dyn->vramTileEmpty || !dyn->vramTileFullValue || !dyn->vramTileFullTrail ||
        !dyn->vramTileBridge || !dyn->cachedFillIndexBridge ||
        !dyn->cellCurrentTileIndex || !dyn->cellValid)
    {
        free_dynamic_buffers(dyn);
        KLog("Gauge dynamic alloc failed");
        return 0;
    }
    dyn->segmentCount = segmentCount;
    dyn->cellCount = cellCount;
    return 1;
}

static u8 init_dynamic_vram(GaugeDynamic *dyn, const GaugeLayout *layout, u16 vramBase, u8 trailEnabled)
{
    u16 nextVram = vramBase;
    u8 hasEndTileset = 0;
    u8 capStartEnabled = 0;
    u8 capEndEnabled = 0;

    if (!alloc_dynamic_buffers(dyn, layout->segmentCount, layout->length))
        return 0;

    /* Initialize cache to invalid */
    for (u8 i = 0; i < dyn->segmentCount; i++)
    {
        dyn->vramTileEmpty[i] = 0;
        dyn->vramTileFullValue[i] = 0;
        dyn->vramTileFullTrail[i] = 0;
        dyn->vramTileBridge[i] = 0;
        dyn->cachedFillIndexBridge[i] = CACHE_INVALID_U8;
    }

    dyn->vramTileCapStart = 0;
    dyn->vramTileCapEnd = 0;

    dyn->loadedSegmentPartialValue = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialValue = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrail = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialTrail = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialEnd = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialEnd = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrailSecond = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialTrailSecond = CACHE_INVALID_U8;
    dyn->cachedFillIndexCapStart = CACHE_INVALID_U8;
    dyn->cachedFillIndexCapEnd = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesBreak = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesTrail = CACHE_INVALID_U8;

    /* Initialize tilemap cache */
    for (u8 i = 0; i < dyn->cellCount; i++)
    {
        dyn->cellCurrentTileIndex[i] = CACHE_INVALID_U16;
    }

    /* Determine which segments are used */
    u8 segmentUsed[GAUGE_MAX_SEGMENTS] = {0};
    for (u8 i = 0; i < layout->length; i++)
    {
        const u8 segmentId = layout->segmentIdByCell[i];
        if (layout->tilesetBySegment[segmentId] || layout->gainTilesetBySegment[segmentId])
        {
            segmentUsed[segmentId] = 1;
        }
    }

    /* Determine if any used segment has END tiles */
    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        if (segmentUsed[segmentId] &&
            (layout->tilesetEndBySegment[segmentId] || layout->gainTilesetEndBySegment[segmentId]))
            hasEndTileset = 1;
    }

    /* Read cached cap flags */
    capStartEnabled = layout->capStartEnabled;
    capEndEnabled = layout->capEndEnabled;

    /* Allocate standard tiles for each used segment */
    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        if (!segmentUsed[segmentId])
            continue;

        /* Empty tile (0,0) */
        dyn->vramTileEmpty[segmentId] = nextVram;
        nextVram++;

        /* Full value tile (8,8) */
        dyn->vramTileFullValue[segmentId] = nextVram;
        nextVram++;

        /* Full trail tile (0,8) - only if trail enabled */
        if (trailEnabled)
        {
            dyn->vramTileFullTrail[segmentId] = nextVram;
            nextVram++;
        }

        /* Bridge tile (per segment) */
        if (layout->tilesetBridgeBySegment[segmentId] || layout->gainTilesetBridgeBySegment[segmentId])
        {
            dyn->vramTileBridge[segmentId] = nextVram;
            nextVram++;
        }
    }

    /* Partial tiles (scalars - 1 per GaugePart, streamed on demand) */
    dyn->vramTilePartialValue = nextVram;  /* Also used for "both" case */
    nextVram++;

    if (trailEnabled)
    {
        dyn->vramTilePartialTrail = nextVram;
        nextVram++;
    }
    else
    {
        /* If trail disabled, set to 0 (won't be used) */
        dyn->vramTilePartialTrail = 0;
    }

    if (hasEndTileset)
    {
        dyn->vramTilePartialEnd = nextVram;
        nextVram++;
    }
    else
    {
        dyn->vramTilePartialEnd = 0;
    }

    if (trailEnabled && hasEndTileset)
    {
        dyn->vramTilePartialTrailSecond = nextVram;
        nextVram++;
    }
    else
    {
        dyn->vramTilePartialTrailSecond = 0;
    }

    /* Cap tiles (1 per part if enabled) */
    if (capStartEnabled)
    {
        dyn->vramTileCapStart = nextVram;
        nextVram++;
    }
    if (capEndEnabled)
    {
        dyn->vramTileCapEnd = nextVram;
        nextVram++;
    }

    return 1;
}

/**
 * Preload standard tiles for dynamic mode (called once at init).
 *
 * Uploads the 3 "static" tiles for each segment (empty, full value, full trail)
 * to their pre-allocated VRAM slots. These tiles never change at runtime.
 *
 * @param dyn          Dynamic VRAM data (contains VRAM tile indices)
 * @param layout       Layout with tileset pointers
 * @param trailEnabled 1 if trail effect is active (uploads full trail tiles)
 */
static void preload_dynamic_standard_tiles(GaugeDynamic *dyn, const GaugeLayout *layout, u8 trailEnabled)
{
    const u8 emptyIndex = STRIP_INDEX_EMPTY;
    const u8 fullValueIndex = STRIP_INDEX_FULL;
    const u8 fullTrailIndexBody = STRIP_INDEX_FULL_TRAIL;
    const u8 fullTrailIndexTrail = s_trailTileIndexByValueTrail[0][8]; /* value=0, trail=8 */

    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        const u32 *bodyStrip = select_base_strip(layout->tilesetBySegment[segmentId],
                                                 layout->gainTilesetBySegment[segmentId],
                                                 GAUGE_TRAIL_DAMAGE);
        if (!bodyStrip)
            continue;

        /* Upload empty tile */
        if (dyn->vramTileEmpty[segmentId] != 0)
            upload_fill_tile(bodyStrip, emptyIndex, dyn->vramTileEmpty[segmentId], DMA_QUEUE);

        /* Upload full value tile */
        if (dyn->vramTileFullValue[segmentId] != 0)
            upload_fill_tile(bodyStrip, fullValueIndex, dyn->vramTileFullValue[segmentId], DMA_QUEUE);

        /* Upload full trail tile (only if trail enabled) */
        if (trailEnabled && dyn->vramTileFullTrail[segmentId] != 0)
        {
            const u32 *trailStrip = select_base_strip(layout->tilesetTrailBySegment[segmentId],
                                                      layout->gainTilesetTrailBySegment[segmentId],
                                                      GAUGE_TRAIL_DAMAGE);
            if (trailStrip)
                upload_fill_tile(trailStrip, fullTrailIndexTrail, dyn->vramTileFullTrail[segmentId], DMA_QUEUE);
            else
                upload_fill_tile(bodyStrip, fullTrailIndexBody, dyn->vramTileFullTrail[segmentId], DMA_QUEUE);
        }
    }

    /* Preload cap end tile (index 0 of END strip) if enabled */
    if (dyn->vramTileCapEnd != 0)
    {
        const u8 cellEnd = layout->cellIndexByFillIndex[layout->length - 1];
        const u8 endSegId = layout->segmentIdByCell[cellEnd];
        const u32 *capEndStrip = select_base_strip(layout->tilesetCapEndBySegment[endSegId],
                                                   layout->gainTilesetCapEndBySegment[endSegId],
                                                   GAUGE_TRAIL_DAMAGE);
        if (capEndStrip)
            upload_fill_tile(capEndStrip, emptyIndex, dyn->vramTileCapEnd, DMA);
    }
}

void GaugeLayout_setGainTrail(GaugeLayout *layout,
                              const u32 * const *gainBodyTilesets,
                              const u32 * const *gainEndTilesets,
                              const u32 * const *gainTrailTilesets,
                              const u32 * const *gainBridgeTilesets,
                              const u32 * const *gainCapStartTilesets,
                              const u32 * const *gainCapEndTilesets,
                              const u32 * const *gainCapStartBreakTilesets,
                              const u32 * const *gainCapStartTrailTilesets)
{
    if (!layout)
        return;

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBodyTilesets, layout->segmentCount),
        &layout->gainTilesetBySegment,
        layout->segmentCount,
        "Gauge gain BODY alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainEndTilesets, layout->segmentCount),
        &layout->gainTilesetEndBySegment,
        layout->segmentCount,
        "Gauge gain END alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainTrailTilesets, layout->segmentCount),
        &layout->gainTilesetTrailBySegment,
        layout->segmentCount,
        "Gauge gain TRAIL alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBridgeTilesets, layout->segmentCount),
        &layout->gainTilesetBridgeBySegment,
        layout->segmentCount,
        "Gauge gain BRIDGE alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainCapStartTilesets, layout->segmentCount),
        &layout->gainTilesetCapStartBySegment,
        layout->segmentCount,
        "Gauge gain CAP START alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainCapEndTilesets, layout->segmentCount),
        &layout->gainTilesetCapEndBySegment,
        layout->segmentCount,
        "Gauge gain CAP END alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainCapStartBreakTilesets, layout->segmentCount),
        &layout->gainTilesetCapStartBreakBySegment,
        layout->segmentCount,
        "Gauge gain CAP START BREAK alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainCapStartTrailTilesets, layout->segmentCount),
        &layout->gainTilesetCapStartTrailBySegment,
        layout->segmentCount,
        "Gauge gain CAP START TRAIL alloc failed");

    layout_copy_segment_tilesets(layout->gainTilesetBySegment, gainBodyTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainTilesetEndBySegment, gainEndTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainTilesetTrailBySegment, gainTrailTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainTilesetBridgeBySegment, gainBridgeTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainTilesetCapStartBySegment, gainCapStartTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainTilesetCapEndBySegment, gainCapEndTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainTilesetCapStartBreakBySegment, gainCapStartBreakTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainTilesetCapStartTrailBySegment, gainCapStartTrailTilesets, layout->segmentCount);

    /* Gain bridges can affect bridge LUTs */
    build_bridge_luts(layout);
}

void GaugeLayout_setGainBlinkOff(GaugeLayout *layout,
                                 const u32 * const *gainBlinkOffBodyTilesets,
                                 const u32 * const *gainBlinkOffEndTilesets,
                                 const u32 * const *gainBlinkOffTrailTilesets,
                                 const u32 * const *gainBlinkOffBridgeTilesets,
                                 const u32 * const *gainBlinkOffCapStartTilesets,
                                 const u32 * const *gainBlinkOffCapEndTilesets,
                                 const u32 * const *gainBlinkOffCapStartBreakTilesets,
                                 const u32 * const *gainBlinkOffCapStartTrailTilesets)
{
    if (!layout)
        return;

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffBodyTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetBySegment,
        layout->segmentCount,
        "Gauge gain blink-off BODY alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffEndTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetEndBySegment,
        layout->segmentCount,
        "Gauge gain blink-off END alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffTrailTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetTrailBySegment,
        layout->segmentCount,
        "Gauge gain blink-off TRAIL alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffBridgeTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetBridgeBySegment,
        layout->segmentCount,
        "Gauge gain blink-off BRIDGE alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffCapStartTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetCapStartBySegment,
        layout->segmentCount,
        "Gauge gain blink-off CAP START alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffCapEndTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetCapEndBySegment,
        layout->segmentCount,
        "Gauge gain blink-off CAP END alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffCapStartBreakTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetCapStartBreakBySegment,
        layout->segmentCount,
        "Gauge gain blink-off CAP START BREAK alloc failed");
    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(gainBlinkOffCapStartTrailTilesets, layout->segmentCount),
        &layout->gainBlinkOffTilesetCapStartTrailBySegment,
        layout->segmentCount,
        "Gauge gain blink-off CAP START TRAIL alloc failed");

    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetBySegment, gainBlinkOffBodyTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetEndBySegment, gainBlinkOffEndTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetTrailBySegment, gainBlinkOffTrailTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetBridgeBySegment, gainBlinkOffBridgeTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetCapStartBySegment, gainBlinkOffCapStartTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetCapEndBySegment, gainBlinkOffCapEndTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetCapStartBreakBySegment, gainBlinkOffCapStartBreakTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->gainBlinkOffTilesetCapStartTrailBySegment, gainBlinkOffCapStartTrailTilesets, layout->segmentCount);

    layout->hasGainBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_GAIN);
}

void GaugeLayout_setPipStyles(GaugeLayout *layout,
                              const u32 * const *pipTilesets,
                              const u8 *pipWidthBySegment)
{
    if (!layout)
        return;

    const u8 hasPipStyles = segment_tileset_array_has_any(pipTilesets, layout->segmentCount);
    u8 hasCustomWidths = 0;

    if (hasPipStyles && pipWidthBySegment)
    {
        for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
        {
            if (pipWidthBySegment[segmentId] > 1)
            {
                hasCustomWidths = 1;
                break;
            }
        }
    }

    layout_sync_optional_segment_tilesets_by_usage(
        hasPipStyles,
        &layout->pipTilesetBySegment,
        layout->segmentCount,
        "Gauge pip tileset alloc failed");
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles && hasCustomWidths,
        &layout->pipWidthBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1,
        "Gauge pip width alloc failed");

    layout_copy_segment_tilesets(layout->pipTilesetBySegment, pipTilesets, layout->segmentCount);
    if (layout->pipWidthBySegment != s_oneSegmentFlags)
    {
        for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
        {
            u8 widthInTiles = pipWidthBySegment ? pipWidthBySegment[segmentId] : 1;
            if (widthInTiles == 0)
                widthInTiles = 1;
            layout->pipWidthBySegment[segmentId] = widthInTiles;
        }
    }

    build_pip_luts(layout);
}

/**
 * Build a layout from a GaugeLayoutInit config (preferred one-call API).
 *
 * Initializes geometry, copies all segment styles (base/gain/blinkOff/gainBlinkOff),
 * and sets cached flags. Equivalent to calling initEx + setCaps + setGainTrail +
 * setBlinkOff + setGainBlinkOff individually.
 */
void GaugeLayout_build(GaugeLayout *layout, const GaugeLayoutInit *init)
{
    if (!layout || !init)
        return;

    /* Initialize geometry/visual properties first. */
    GaugeLayout_initEx(layout,
                       init->length,
                       init->fillDirection,
                       NULL, NULL, NULL, NULL,
                       init->segmentIdByCell,
                       init->orientation,
                       init->palette,
                       init->priority,
                       init->verticalFlip,
                       init->horizontalFlip);

    /* Apply segment styles directly with lazy optional allocations. */
    if (init->segmentStyles)
    {
        u8 hasBaseEnd = 0;
        u8 hasBaseTrail = 0;
        u8 hasBaseBridge = 0;
        u8 hasCapStart = 0;
        u8 hasCapEnd = 0;
        u8 hasCapStartBreak = 0;
        u8 hasCapStartTrail = 0;
        u8 hasCapEndFlags = 0;

        u8 hasGainBody = 0;
        u8 hasGainEnd = 0;
        u8 hasGainTrail = 0;
        u8 hasGainBridge = 0;
        u8 hasGainCapStart = 0;
        u8 hasGainCapEnd = 0;
        u8 hasGainCapStartBreak = 0;
        u8 hasGainCapStartTrail = 0;

        u8 hasBlinkBody = 0;
        u8 hasBlinkEnd = 0;
        u8 hasBlinkTrail = 0;
        u8 hasBlinkBridge = 0;
        u8 hasBlinkCapStart = 0;
        u8 hasBlinkCapEnd = 0;
        u8 hasBlinkCapStartBreak = 0;
        u8 hasBlinkCapStartTrail = 0;

        u8 hasGainBlinkBody = 0;
        u8 hasGainBlinkEnd = 0;
        u8 hasGainBlinkTrail = 0;
        u8 hasGainBlinkBridge = 0;
        u8 hasGainBlinkCapStart = 0;
        u8 hasGainBlinkCapEnd = 0;
        u8 hasGainBlinkCapStartBreak = 0;
        u8 hasGainBlinkCapStartTrail = 0;

        for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
        {
            const GaugeSegmentStyle *style = &init->segmentStyles[segmentId];
            hasBaseEnd |= (style->base.end != NULL);
            hasBaseTrail |= (style->base.trail != NULL);
            hasBaseBridge |= (style->base.bridge != NULL);
            hasCapStart |= (style->base.capStart != NULL);
            hasCapEnd |= (style->base.capEnd != NULL);
            hasCapStartBreak |= (style->base.capStartBreak != NULL);
            hasCapStartTrail |= (style->base.capStartTrail != NULL);
            hasCapEndFlags |= (style->capEndEnabled != 0);

            hasGainBody |= (style->gain.body != NULL);
            hasGainEnd |= (style->gain.end != NULL);
            hasGainTrail |= (style->gain.trail != NULL);
            hasGainBridge |= (style->gain.bridge != NULL);
            hasGainCapStart |= (style->gain.capStart != NULL);
            hasGainCapEnd |= (style->gain.capEnd != NULL);
            hasGainCapStartBreak |= (style->gain.capStartBreak != NULL);
            hasGainCapStartTrail |= (style->gain.capStartTrail != NULL);

            hasBlinkBody |= (style->blinkOff.body != NULL);
            hasBlinkEnd |= (style->blinkOff.end != NULL);
            hasBlinkTrail |= (style->blinkOff.trail != NULL);
            hasBlinkBridge |= (style->blinkOff.bridge != NULL);
            hasBlinkCapStart |= (style->blinkOff.capStart != NULL);
            hasBlinkCapEnd |= (style->blinkOff.capEnd != NULL);
            hasBlinkCapStartBreak |= (style->blinkOff.capStartBreak != NULL);
            hasBlinkCapStartTrail |= (style->blinkOff.capStartTrail != NULL);

            hasGainBlinkBody |= (style->gainBlinkOff.body != NULL);
            hasGainBlinkEnd |= (style->gainBlinkOff.end != NULL);
            hasGainBlinkTrail |= (style->gainBlinkOff.trail != NULL);
            hasGainBlinkBridge |= (style->gainBlinkOff.bridge != NULL);
            hasGainBlinkCapStart |= (style->gainBlinkOff.capStart != NULL);
            hasGainBlinkCapEnd |= (style->gainBlinkOff.capEnd != NULL);
            hasGainBlinkCapStartBreak |= (style->gainBlinkOff.capStartBreak != NULL);
            hasGainBlinkCapStartTrail |= (style->gainBlinkOff.capStartTrail != NULL);
        }

        layout_sync_optional_segment_tilesets_by_usage(hasBaseEnd,
                                                       &layout->tilesetEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build base END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBaseTrail,
                                                       &layout->tilesetTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build base TRAIL alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBaseBridge,
                                                       &layout->tilesetBridgeBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build base BRIDGE alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasCapStart,
                                                       &layout->tilesetCapStartBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build base CAP START alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasCapEnd,
                                                       &layout->tilesetCapEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build base CAP END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasCapStartBreak,
                                                       &layout->tilesetCapStartBreakBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build base CAP START BREAK alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasCapStartTrail,
                                                       &layout->tilesetCapStartTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build base CAP START TRAIL alloc failed");
        layout_sync_optional_segment_flags_by_usage(hasCapEndFlags,
                                                    &layout->capEndBySegment,
                                                    layout->segmentCount,
                                                    s_zeroSegmentFlags,
                                                    0,
                                                    "Gauge build capEnd flags alloc failed");

        layout_sync_optional_segment_tilesets_by_usage(hasGainBody,
                                                       &layout->gainTilesetBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain BODY alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainEnd,
                                                       &layout->gainTilesetEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainTrail,
                                                       &layout->gainTilesetTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain TRAIL alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBridge,
                                                       &layout->gainTilesetBridgeBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain BRIDGE alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainCapStart,
                                                       &layout->gainTilesetCapStartBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain CAP START alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainCapEnd,
                                                       &layout->gainTilesetCapEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain CAP END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainCapStartBreak,
                                                       &layout->gainTilesetCapStartBreakBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain CAP START BREAK alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainCapStartTrail,
                                                       &layout->gainTilesetCapStartTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain CAP START TRAIL alloc failed");

        layout_sync_optional_segment_tilesets_by_usage(hasBlinkBody,
                                                       &layout->blinkOffTilesetBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink BODY alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBlinkEnd,
                                                       &layout->blinkOffTilesetEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBlinkTrail,
                                                       &layout->blinkOffTilesetTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink TRAIL alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBlinkBridge,
                                                       &layout->blinkOffTilesetBridgeBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink BRIDGE alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBlinkCapStart,
                                                       &layout->blinkOffTilesetCapStartBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink CAP START alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBlinkCapEnd,
                                                       &layout->blinkOffTilesetCapEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink CAP END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBlinkCapStartBreak,
                                                       &layout->blinkOffTilesetCapStartBreakBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink CAP START BREAK alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasBlinkCapStartTrail,
                                                       &layout->blinkOffTilesetCapStartTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build blink CAP START TRAIL alloc failed");

        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkBody,
                                                       &layout->gainBlinkOffTilesetBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink BODY alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkEnd,
                                                       &layout->gainBlinkOffTilesetEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkTrail,
                                                       &layout->gainBlinkOffTilesetTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink TRAIL alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkBridge,
                                                       &layout->gainBlinkOffTilesetBridgeBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink BRIDGE alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkCapStart,
                                                       &layout->gainBlinkOffTilesetCapStartBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink CAP START alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkCapEnd,
                                                       &layout->gainBlinkOffTilesetCapEndBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink CAP END alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkCapStartBreak,
                                                       &layout->gainBlinkOffTilesetCapStartBreakBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink CAP START BREAK alloc failed");
        layout_sync_optional_segment_tilesets_by_usage(hasGainBlinkCapStartTrail,
                                                       &layout->gainBlinkOffTilesetCapStartTrailBySegment,
                                                       layout->segmentCount,
                                                       "Gauge build gain blink CAP START TRAIL alloc failed");

        for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
        {
            const GaugeSegmentStyle *style = &init->segmentStyles[segmentId];

            layout->tilesetBySegment[segmentId] = style->base.body;
            if (layout->tilesetEndBySegment != s_nullSegmentTilesets)
                layout->tilesetEndBySegment[segmentId] = style->base.end;
            if (layout->tilesetTrailBySegment != s_nullSegmentTilesets)
                layout->tilesetTrailBySegment[segmentId] = style->base.trail;
            if (layout->tilesetBridgeBySegment != s_nullSegmentTilesets)
                layout->tilesetBridgeBySegment[segmentId] = style->base.bridge;
            if (layout->tilesetCapStartBySegment != s_nullSegmentTilesets)
                layout->tilesetCapStartBySegment[segmentId] = style->base.capStart;
            if (layout->tilesetCapEndBySegment != s_nullSegmentTilesets)
                layout->tilesetCapEndBySegment[segmentId] = style->base.capEnd;
            if (layout->tilesetCapStartBreakBySegment != s_nullSegmentTilesets)
                layout->tilesetCapStartBreakBySegment[segmentId] = style->base.capStartBreak;
            if (layout->tilesetCapStartTrailBySegment != s_nullSegmentTilesets)
                layout->tilesetCapStartTrailBySegment[segmentId] = style->base.capStartTrail;
            if (layout->capEndBySegment != s_zeroSegmentFlags)
                layout->capEndBySegment[segmentId] = style->capEndEnabled ? 1 : 0;

            if (layout->gainTilesetBySegment != s_nullSegmentTilesets)
                layout->gainTilesetBySegment[segmentId] = style->gain.body;
            if (layout->gainTilesetEndBySegment != s_nullSegmentTilesets)
                layout->gainTilesetEndBySegment[segmentId] = style->gain.end;
            if (layout->gainTilesetTrailBySegment != s_nullSegmentTilesets)
                layout->gainTilesetTrailBySegment[segmentId] = style->gain.trail;
            if (layout->gainTilesetBridgeBySegment != s_nullSegmentTilesets)
                layout->gainTilesetBridgeBySegment[segmentId] = style->gain.bridge;
            if (layout->gainTilesetCapStartBySegment != s_nullSegmentTilesets)
                layout->gainTilesetCapStartBySegment[segmentId] = style->gain.capStart;
            if (layout->gainTilesetCapEndBySegment != s_nullSegmentTilesets)
                layout->gainTilesetCapEndBySegment[segmentId] = style->gain.capEnd;
            if (layout->gainTilesetCapStartBreakBySegment != s_nullSegmentTilesets)
                layout->gainTilesetCapStartBreakBySegment[segmentId] = style->gain.capStartBreak;
            if (layout->gainTilesetCapStartTrailBySegment != s_nullSegmentTilesets)
                layout->gainTilesetCapStartTrailBySegment[segmentId] = style->gain.capStartTrail;

            if (layout->blinkOffTilesetBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetBySegment[segmentId] = style->blinkOff.body;
            if (layout->blinkOffTilesetEndBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetEndBySegment[segmentId] = style->blinkOff.end;
            if (layout->blinkOffTilesetTrailBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetTrailBySegment[segmentId] = style->blinkOff.trail;
            if (layout->blinkOffTilesetBridgeBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetBridgeBySegment[segmentId] = style->blinkOff.bridge;
            if (layout->blinkOffTilesetCapStartBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetCapStartBySegment[segmentId] = style->blinkOff.capStart;
            if (layout->blinkOffTilesetCapEndBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetCapEndBySegment[segmentId] = style->blinkOff.capEnd;
            if (layout->blinkOffTilesetCapStartBreakBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetCapStartBreakBySegment[segmentId] = style->blinkOff.capStartBreak;
            if (layout->blinkOffTilesetCapStartTrailBySegment != s_nullSegmentTilesets)
                layout->blinkOffTilesetCapStartTrailBySegment[segmentId] = style->blinkOff.capStartTrail;

            if (layout->gainBlinkOffTilesetBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetBySegment[segmentId] = style->gainBlinkOff.body;
            if (layout->gainBlinkOffTilesetEndBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetEndBySegment[segmentId] = style->gainBlinkOff.end;
            if (layout->gainBlinkOffTilesetTrailBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetTrailBySegment[segmentId] = style->gainBlinkOff.trail;
            if (layout->gainBlinkOffTilesetBridgeBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetBridgeBySegment[segmentId] = style->gainBlinkOff.bridge;
            if (layout->gainBlinkOffTilesetCapStartBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetCapStartBySegment[segmentId] = style->gainBlinkOff.capStart;
            if (layout->gainBlinkOffTilesetCapEndBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetCapEndBySegment[segmentId] = style->gainBlinkOff.capEnd;
            if (layout->gainBlinkOffTilesetCapStartBreakBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetCapStartBreakBySegment[segmentId] = style->gainBlinkOff.capStartBreak;
            if (layout->gainBlinkOffTilesetCapStartTrailBySegment != s_nullSegmentTilesets)
                layout->gainBlinkOffTilesetCapStartTrailBySegment[segmentId] = style->gainBlinkOff.capStartTrail;
        }
    }

    layout->hasBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_DAMAGE);
    layout->hasGainBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_GAIN);

    /* Compute cached cap flags */
    detect_caps_enabled(layout, &layout->capStartEnabled, &layout->capEndEnabled);

    /* Bridges depend on both base and gain bridge arrays, recompute once. */
    build_bridge_luts(layout);
    build_pip_luts(layout);
}

/**
 * Reset all dynamic VRAM caches to force re-upload on next render.
 *
 * Called when blink state or trail mode changes, since the cached tiles
 * may now be from the wrong tileset context (e.g., base vs blink-off).
 *
 * @param dyn  Dynamic VRAM data to reset
 */
static void reset_dynamic_blink_cache(GaugeDynamic *dyn)
{
    dyn->loadedSegmentPartialValue = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialValue = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrail = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialTrail = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialEnd = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialEnd = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrailSecond = CACHE_INVALID_U8;
    dyn->cachedFillIndexPartialTrailSecond = CACHE_INVALID_U8;
    dyn->cachedFillIndexCapStart = CACHE_INVALID_U8;
    dyn->cachedFillIndexCapEnd = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesBreak = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesTrail = CACHE_INVALID_U8;

    for (u8 i = 0; i < dyn->segmentCount; i++)
    {
        dyn->cachedFillIndexBridge[i] = CACHE_INVALID_U8;
    }
}

/**
 * Reload the "full trail" standard tiles when blink state or trail mode changes.
 *
 * In dynamic mode, full trail tiles (value=0, trail=8) are pre-loaded once.
 * When blink toggles, they must be re-uploaded from the correct tileset
 * (normal or blink-off) so that TRAIL_FULL cells display correctly.
 *
 * @param dyn          Dynamic VRAM data
 * @param layout       Layout with tileset pointers
 * @param useBlinkOff  1 if blink-off tilesets should be used
 * @param trailMode    Current trail mode (DAMAGE or GAIN)
 */
static void reload_dynamic_full_trail_tiles(GaugeDynamic *dyn,
                                            const GaugeLayout *layout,
                                            u8 useBlinkOff,
                                            u8 trailMode)
{
    const u8 fullTrailIndexBody = STRIP_INDEX_FULL_TRAIL;
    const u8 fullTrailIndexTrail = s_trailTileIndexByValueTrail[0][8];

    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        const u16 vramTile = dyn->vramTileFullTrail[segmentId];
        if (vramTile == 0)
            continue;

        const u32 *blinkOffTrailStrip = select_blink_strip(
            layout->blinkOffTilesetTrailBySegment[segmentId],
            layout->gainBlinkOffTilesetTrailBySegment[segmentId],
            trailMode);

        if (useBlinkOff && blinkOffTrailStrip)
        {
            upload_fill_tile(blinkOffTrailStrip,
                             fullTrailIndexTrail, vramTile, DMA);
        }
        else
        {
            const u32 *trailStrip = select_base_strip(layout->tilesetTrailBySegment[segmentId],
                                                      layout->gainTilesetTrailBySegment[segmentId],
                                                      trailMode);
            const u32 *bodyStrip = select_base_strip(layout->tilesetBySegment[segmentId],
                                                     layout->gainTilesetBySegment[segmentId],
                                                     trailMode);
            if (trailStrip)
                upload_fill_tile(trailStrip, fullTrailIndexTrail, vramTile, DMA);
            else if (bodyStrip)
                upload_fill_tile(bodyStrip, fullTrailIndexBody, vramTile, DMA);
        }
    }
}

/**
 * Reload full value/empty tiles when trail mode changes (dynamic mode).
 */
static void reload_dynamic_full_body_tiles(GaugeDynamic *dyn,
                                           const GaugeLayout *layout,
                                           u8 trailMode)
{
    const u8 fullValueIndex = STRIP_INDEX_FULL;
    const u8 emptyIndex = STRIP_INDEX_EMPTY;

    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        const u32 *bodyStrip = select_base_strip(layout->tilesetBySegment[segmentId],
                                                 layout->gainTilesetBySegment[segmentId],
                                                 trailMode);
        if (!bodyStrip)
            continue;

        const u16 vramTileFull = dyn->vramTileFullValue[segmentId];
        if (vramTileFull)
            upload_fill_tile(bodyStrip, fullValueIndex, vramTileFull, DMA);

        const u16 vramTileEmpty = dyn->vramTileEmpty[segmentId];
        if (vramTileEmpty)
            upload_fill_tile(bodyStrip, emptyIndex, vramTileEmpty, DMA);
    }
}

/**
 * Initialize tilemap positions, cell validity, and write initial tilemap (all empty).
 *
 * Pre-computes tilemap X/Y coordinates and cell validity for each cell to avoid
 * per-frame recalculation. Then writes all cells as empty tiles to the WINDOW plane.
 *
 * @param part  GaugePart to initialize (must have layout and dyn set)
 */
static void init_dynamic_tilemap(GaugePart *part)
{
    const GaugeLayout *layout = part->layout;
    GaugeDynamic *dyn = &part->dyn;

    /* Pre-calculate tilemap positions and cell validity for each cell */
    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        compute_tile_xy(layout->orientation, part->originX, part->originY, cellIndex,
                        &layout->tilemapPosByCell[cellIndex].x,
                        &layout->tilemapPosByCell[cellIndex].y);

        /* Pre-compute cell validity (avoids tileset NULL check in render loop) */
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        dyn->cellValid[cellIndex] = (layout->tilesetBySegment[segmentId] != NULL ||
                                     layout->gainTilesetBySegment[segmentId] != NULL) ? 1 : 0;
    }

    /* Write initial tilemap (all empty tiles) */
    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        if (!dyn->cellValid[cellIndex])
            continue;

        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u16 vramTile = dyn->vramTileEmpty[segmentId];
        const u16 attr = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        layout->verticalFlip, layout->horizontalFlip, vramTile);

        VDP_setTileMapXY(WINDOW, attr,
                         layout->tilemapPosByCell[cellIndex].x,
                         layout->tilemapPosByCell[cellIndex].y);

        /* Initialize cache to avoid redundant writes on first update */
        dyn->cellCurrentTileIndex[cellIndex] = vramTile;
    }
}

/**
 * Initialize tilemap for PIP mode (one VRAM tile per cell, fixed VRAM).
 *
 * Allocates one VRAM tile per cell, uploads the EMPTY state from each segment's
 * compact strip, and writes the initial tilemap. Also sets up GaugeStreamCell
 * entries for per-cell DMA updates at runtime.
 *
 * @param part  GaugePart to initialize (must have layout set)
 */
static void write_tilemap_pip_init(GaugePart *part)
{
    const GaugeLayout *layout = part->layout;
    part->cellCount = 0;

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *pipStrip = layout->pipTilesetBySegment[segmentId];
        if (!pipStrip)
            continue;

        if (part->cellCount >= layout->length)
            break;

        const u16 vramTile = (u16)(part->vramBase + part->cellCount);
        const u16 attr = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        layout->verticalFlip, layout->horizontalFlip, vramTile);

        u16 x, y;
        compute_tile_xy(layout->orientation, part->originX, part->originY, cellIndex, &x, &y);
        VDP_setTileMapXY(WINDOW, attr, x, y);

        const u8 fillIndex = layout->fillIndexByCell[cellIndex];
        const u8 pipIndex = layout->pipIndexByFillIndex[fillIndex];
        u8 pipWidth = (pipIndex == CACHE_INVALID_U8) ? 1 : layout->pipWidthByPipIndex[pipIndex];
        if (pipWidth == 0)
            pipWidth = 1;

        u8 localTile = layout->pipLocalTileByFillIndex[fillIndex];
        if (localTile >= pipWidth)
            localTile = 0;

        /* Preload EMPTY tile for this local position. */
        upload_fill_tile(pipStrip, localTile, vramTile, DMA_QUEUE);

        part->cells[part->cellCount].vramTileIndex = vramTile;
        part->cells[part->cellCount].bodyFillStrip45 = pipStrip;
        part->cells[part->cellCount].endFillStrip45 = NULL;
        part->cells[part->cellCount].trailFillStrip64 = NULL;
        part->cells[part->cellCount].bridgeFillStrip45 = NULL;
        part->cells[part->cellCount].cachedStrip = pipStrip;
        part->cells[part->cellCount].cachedFillIndex = localTile;
        part->cells[part->cellCount].cellIndex = cellIndex;
        part->cellCount++;
    }
}

/**
 * Initialize tilemap for fixed VRAM mode (one VRAM tile per cell, fill mode).
 *
 * Allocates one VRAM tile per valid cell, pre-caches tileset strip pointers
 * (body/end/trail/bridge) per cell to avoid per-frame segment lookups, uploads
 * initial empty tiles, and writes the tilemap on the WINDOW plane.
 *
 * @param part  GaugePart to initialize (must have layout and vramBase set)
 */
static void write_tilemap_fixed_init(GaugePart *part)
{
    const GaugeLayout *layout = part->layout;
    part->cellCount = 0;

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *bodyStrip = layout->tilesetBySegment[segmentId];
        const u32 *gainBodyStrip = layout->gainTilesetBySegment[segmentId];

        if (!bodyStrip && !gainBodyStrip)
            continue;

        if (part->cellCount >= layout->length)
            break;

        const u16 vramTile = (u16)(part->vramBase + part->cellCount);
        const u16 attr = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        layout->verticalFlip, layout->horizontalFlip, vramTile);

        u16 x, y;
        compute_tile_xy(layout->orientation, part->originX, part->originY, cellIndex, &x, &y);

        VDP_setTileMapXY(WINDOW, attr, x, y);

        /* Precompute per-cell strips to avoid segment lookups every frame */
        const u32 *endStrip = layout->tilesetEndBySegment[segmentId];
        const u32 *trailStrip = layout->tilesetTrailBySegment[segmentId];
        const u32 *bridgeStrip = layout->tilesetBridgeBySegment[segmentId];

        part->cells[part->cellCount].vramTileIndex = vramTile;
        part->cells[part->cellCount].bodyFillStrip45 = bodyStrip;
        part->cells[part->cellCount].endFillStrip45 = endStrip;
        part->cells[part->cellCount].trailFillStrip64 = trailStrip;
        part->cells[part->cellCount].bridgeFillStrip45 = bridgeStrip;
        part->cells[part->cellCount].cachedStrip = NULL;
        part->cells[part->cellCount].cachedFillIndex = CACHE_INVALID_U8;
        part->cells[part->cellCount].cellIndex = cellIndex;
        part->cellCount++;

    } 
}

/* =============================================================================
   Break zone cell classification (shared by fixed and dynamic modes)
   =============================================================================

   Classifies a cell within the break zone into one of these types:

     |FULL|VALUE_BREAK|TRAIL_FULL|TRAIL_BREAK|TRAIL_BREAK2|END|EMPTY|
      <-- value -->|<---------- trail ---------->|

   Both process_fixed_mode() and process_dynamic_mode() use the same
   classification logic but differ in how they act on the result:
   - Fixed:   selects ROM strip → DMA tile to per-cell VRAM
   - Dynamic: selects VRAM tile → optional DMA + tilemap update

   ============================================================================= */
typedef enum
{
    CELL_TYPE_END,           /* Value/trail frontier (uses end tileset) */
    CELL_TYPE_TRAIL_BREAK,   /* Trail edge cell, after VALUE_BREAK (uses trail tileset) */
    CELL_TYPE_TRAIL_BREAK2,  /* Second trail break, before END (uses trail tileset) */
    CELL_TYPE_TRAIL_FULL,    /* Fully covered by trail, no value (uses trail tileset) */
    CELL_TYPE_VALUE_BREAK,   /* Value edge cell, before trail zone (uses body tileset) */
    CELL_TYPE_REGION_FULL,   /* Region rendering: fully filled body */
    CELL_TYPE_REGION_EMPTY,  /* Region rendering: empty body */
    CELL_TYPE_REGION_TRAIL,  /* Region rendering: full trail */
    CELL_TYPE_DEFAULT        /* Standard fill computation (no special break zone) */
} CellBreakType;

/**
 * Classify a cell's break zone type based on pre-computed break info.
 *
 * This is the shared classification logic used by both process_fixed_mode
 * and process_dynamic_mode to determine which tileset and tile index to use.
 *
 * @param breakInfoUsed  Break zone info (normal or blink-off adjusted)
 * @param cellFillIndex  Fill index of the cell to classify
 * @param hasEndStrip    1 if segment has an end tileset
 * @param hasTrailStrip  1 if segment has a trail tileset
 * @return CellBreakType indicating which visual mode applies
 */
static inline CellBreakType classify_break_zone_cell(
    const GaugeBreakInfo *breakInfoUsed,
    u8 cellFillIndex,
    u8 hasEndStrip,
    u8 hasTrailStrip)
{
    if (breakInfoUsed->endFillIndex != CACHE_INVALID_U8 &&
        hasEndStrip && cellFillIndex == breakInfoUsed->endFillIndex)
        return CELL_TYPE_END;

    if (breakInfoUsed->trailBreakActive && hasTrailStrip &&
        cellFillIndex == breakInfoUsed->trailBreakFillIndex)
        return CELL_TYPE_TRAIL_BREAK;

    if (breakInfoUsed->trailBreakSecondActive && hasTrailStrip &&
        cellFillIndex == breakInfoUsed->trailBreakFillIndex2)
        return CELL_TYPE_TRAIL_BREAK2;

    if (breakInfoUsed->trailBreakActive && hasTrailStrip &&
        cellFillIndex > breakInfoUsed->valueFillIndex &&
        cellFillIndex < breakInfoUsed->trailFillIndex)
        return CELL_TYPE_TRAIL_FULL;

    if (breakInfoUsed->valueBreakFillIndex != CACHE_INVALID_U8 &&
        cellFillIndex == breakInfoUsed->valueBreakFillIndex)
        return CELL_TYPE_VALUE_BREAK;

    if (breakInfoUsed->regionRenderActive)
    {
        if (cellFillIndex < breakInfoUsed->valueFillIndex)
            return CELL_TYPE_REGION_FULL;
        if (cellFillIndex > breakInfoUsed->trailFillIndex)
            return CELL_TYPE_REGION_EMPTY;
        if (cellFillIndex > breakInfoUsed->valueFillIndex &&
            cellFillIndex < breakInfoUsed->trailFillIndex)
            return CELL_TYPE_REGION_TRAIL;
    }

    return CELL_TYPE_DEFAULT;
}


/**
 * Process fixed mode: stream tiles for all cells via DMA.
 *
 * Each cell has its own dedicated VRAM tile. When the gauge value changes,
 * only the affected cells get new tile data DMA'd from ROM strips.
 * Change detection per cell (cachedFillIndex + cachedStrip) avoids
 * redundant DMA transfers.
 *
 * Algorithm:
 * 1. Compute break zone boundaries (breakLow..breakHigh)
 * 2. For each cell (countdown loop):
 *    a. Early-exit if trivially full or empty (skip classification)
 *    b. Classify cell into visual mode (END(E)/TRAIL(T)/VALUE_BREAK(B)/EMPTY(V)/FULL(F))
 *    c. Select appropriate ROM strip and fill index
 *    d. DMA tile data if changed (upload_cell_if_needed)
 *
 * @param part                 Part to render
 * @param valuePixels          Current value fill in pixels
 * @param trailPixelsRendered  Current trail fill in pixels (after blink)
 * @param trailPixelsActual    Current trail fill in pixels (before blink)
 * @param blinkOffActive       1 if blink-off rendering is active this frame
 * @param trailMode            Current trail mode (DAMAGE/GAIN/NONE)
 *
 * Cost: ~200 cycles per cell (DMA setup dominant), ~50 cycles for trivial cells
 */
static void process_fixed_mode(GaugePart *part,
                               u16 valuePixels,
                               u16 trailPixelsRendered,
                               u16 trailPixelsActual,
                               u8 blinkOffActive,
                               u8 trailMode)
{
    const GaugeLayout *layout = part->layout;

    /* --- Break zone computation ---
     * breakInfo: used for normal cell classification (trail with blink applied)
     * breakInfoActual: used for bridge visibility (trail WITHOUT blink, so bridges
     *            don't flicker on/off during blink phase) */
    GaugeBreakInfo breakInfo;
    compute_break_info(layout, valuePixels, trailPixelsRendered, trailMode, &breakInfo);
    GaugeBreakInfo breakInfoActual = breakInfo;
    if (trailPixelsActual != trailPixelsRendered)
        compute_break_info(layout, valuePixels, trailPixelsActual, trailMode, &breakInfoActual);

    /* Early-exit boundaries: cells with fillIndex outside [breakLow..breakHigh]
     * are trivially full or empty (~70-80% of cells). Skips full classification. */
    const u8 breakLow = (breakInfo.valueBreakFillIndex != CACHE_INVALID_U8)
                       ? breakInfo.valueBreakFillIndex : breakInfo.valueFillIndex;
    const u8 breakHigh = breakInfo.trailFillIndex;

    /* Cap detection */
    const u8 capStartCellIndex = layout->cellIndexByFillIndex[0];
    const u8 capEndCellIndex = layout->cellIndexByFillIndex[layout->length - 1];
    const u8 capStartSegId = layout->segmentIdByCell[capStartCellIndex];
    const u8 capEndSegId = layout->segmentIdByCell[capEndCellIndex];
    const u8 capStartEnabled = layout->capStartEnabled;
    const u8 capEndEnabled = layout->capEndEnabled;

    /* Countdown loop: 68000 zero-flag test is free after decrement (dbra) */
    u8 i = part->cellCount;
    while (i--)
    {
        GaugeStreamCell *cell = &part->cells[i];
        const u8 cellIndex = cell->cellIndex;
        const u8 cellFillIndex = layout->fillIndexByCell[cellIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *bodyStrip = select_base_strip(cell->bodyFillStrip45,
                                                 layout->gainTilesetBySegment[segmentId],
                                                 trailMode);
        const u32 *blinkOffBodyStrip = select_blink_strip(
            layout->blinkOffTilesetBySegment[segmentId],
            layout->gainBlinkOffTilesetBySegment[segmentId],
            trailMode);
        /* endStrip, trailStrip, bridgeStrip and blink-off variants are
         * computed lazily below — only when needed (bridge or break zone).
         * Saves 6 x select calls (~6-8 cycles each) for ~70-80% trivial cells. */

        /* === Cap end (always uses its tileset) === */
        if (cellIndex == capEndCellIndex && capEndEnabled && segmentId == capEndSegId)
        {
            u8 capValuePx = 0;
            u8 capTrailPx = 0;
            const u32 *capEndStrip = select_base_strip(
                layout->tilesetCapEndBySegment[segmentId],
                layout->gainTilesetCapEndBySegment[segmentId],
                trailMode);
            u16 capTrailPixels = trailPixelsRendered;

            const u32 *capEndBlinkStrip = select_blink_strip(
                layout->blinkOffTilesetCapEndBySegment[segmentId],
                layout->gainBlinkOffTilesetCapEndBySegment[segmentId],
                trailMode);

            if (blinkOffActive &&
                capEndBlinkStrip &&
                is_cell_in_trail_zone(&breakInfoActual, cellFillIndex))
            {
                capEndStrip = capEndBlinkStrip;
                capTrailPixels = trailPixelsActual;
            }

            compute_fill_for_cell(layout, cellIndex, valuePixels, capTrailPixels,
                                  &capValuePx, &capTrailPx);
            upload_cell_if_needed(cell,
                                  capEndStrip,
                                  s_tileIndexByValueTrail[capValuePx][capTrailPx]);
            continue;
        }

        /* === Cap start (fixed border at cell 0) === */
        if (cellIndex == capStartCellIndex && capStartEnabled && segmentId == capStartSegId)
        {
            const u32 *capStartStrip = select_base_strip(
                layout->tilesetCapStartBySegment[segmentId],
                layout->gainTilesetCapStartBySegment[segmentId],
                trailMode);
            const u32 *capStartBreakStrip = select_base_strip(
                layout->tilesetCapStartBreakBySegment[segmentId],
                layout->gainTilesetCapStartBreakBySegment[segmentId],
                trailMode);
            const u32 *capStartTrailStrip = select_base_strip(
                layout->tilesetCapStartTrailBySegment[segmentId],
                layout->gainTilesetCapStartTrailBySegment[segmentId],
                trailMode);
            const u32 *capStartBlinkStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBySegment[segmentId],
                trailMode);
            const u32 *capStartBlinkBreakStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBreakBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBreakBySegment[segmentId],
                trailMode);
            const u32 *capStartBlinkTrailStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartTrailBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartTrailBySegment[segmentId],
                trailMode);

            if (capStartStrip)
            {
                CapStartResult capResult;
                if (blinkOffActive)
                {
                    CapStartResult capActual;
                    classify_cap_start_with_strips(&breakInfoActual,
                                                   cellFillIndex,
                                                   capStartStrip,
                                                   capStartBreakStrip,
                                                   capStartTrailStrip,
                                                   &capActual);

                    const u8 inTrailZone = is_cell_in_trail_zone(&breakInfoActual, cellFillIndex);
                    const u32 *blinkStrip = NULL;

                    if (inTrailZone)
                    {
                        if (capActual.usesTrail)
                            blinkStrip = capStartBlinkTrailStrip;
                        else if (capActual.usesBreak)
                            blinkStrip = capStartBlinkBreakStrip;
                        else
                            blinkStrip = capStartBlinkStrip;
                    }

                    if (blinkStrip)
                    {
                        capActual.strip = blinkStrip;
                        capResult = capActual;
                    }
                    else
                    {
                        classify_cap_start_with_strips(&breakInfo,
                                                       cellFillIndex,
                                                       capStartStrip,
                                                       capStartBreakStrip,
                                                       capStartTrailStrip,
                                                       &capResult);
                    }
                }
                else
                {
                    classify_cap_start_with_strips(&breakInfo,
                                                   cellFillIndex,
                                                   capStartStrip,
                                                   capStartBreakStrip,
                                                   capStartTrailStrip,
                                                   &capResult);
                }
                upload_cell_if_needed(cell, capResult.strip, capResult.fillStripIndex);
                continue;
            }
        }

        /* === Bridge/BREAK forced rendering (D/B before gauge END) ===
         * Bridge cells sit at segment boundaries. When the gauge's value/trail
         * edge is past this cell, the bridge shows a transition tile.
         * breakInfoActual uses trailPixelsActual (ignoring blink) so bridges don't
         * flicker during blink phase. */
        if (breakInfoActual.endFillIndex != CACHE_INVALID_U8)
        {
            /* Bridge end cell: last cell of a segment with a bridge tileset */
            if (layout->bridgeEndByFillIndex[cellFillIndex] &&
                breakInfoActual.endFillIndex > cellFillIndex &&
                breakInfoActual.valueFillIndex > cellFillIndex)
            {
                const u8 nextFillIndex = (u8)(cellFillIndex + 1);
                const u8 bridgeInTrailZone = is_cell_in_trail_zone(&breakInfoActual, nextFillIndex);

                /* Lazy: bridge strips computed only for bridge cells (rare) */
                const u32 *bridgeStrip = select_base_strip(cell->bridgeFillStrip45,
                                                           layout->gainTilesetBridgeBySegment[segmentId],
                                                           trailMode);
                const u32 *blinkOffBridgeStrip = select_blink_strip(
                    layout->blinkOffTilesetBridgeBySegment[segmentId],
                    layout->gainBlinkOffTilesetBridgeBySegment[segmentId],
                    trailMode);

                if (bridgeStrip)
                {
                    const u8 useBlinkOffBridge = (blinkOffActive &&
                                                  blinkOffBridgeStrip &&
                                                  bridgeInTrailZone);
                    const GaugeBreakInfo *bridgeBreakInfo = useBlinkOffBridge ? &breakInfoActual : &breakInfo;
                    const u16 bridgeTrailPixels = useBlinkOffBridge ? trailPixelsActual : trailPixelsRendered;
                    const u32 *bridgeStripUse = useBlinkOffBridge
                                                ? blinkOffBridgeStrip
                                                : bridgeStrip;
                    const u8 bridgeIdx = compute_bridge_strip_index(
                        layout, bridgeBreakInfo, cellFillIndex, valuePixels, bridgeTrailPixels);
                    upload_cell_if_needed(cell, bridgeStripUse, bridgeIdx);
                }
                else
                {
                    /* No bridge tileset: fall back to body, fully filled */
                    const u32 *fallbackStrip = bodyStrip;
                    if (blinkOffActive && blinkOffBodyStrip && bridgeInTrailZone)
                    {
                        fallbackStrip = blinkOffBodyStrip;
                    }
                    upload_cell_if_needed(cell, fallbackStrip,
                                          STRIP_INDEX_FULL);
                }
                continue;
            }

            /* Forced BREAK cell: cell before a bridge that must show as fully filled */
            if (layout->bridgeBreakByFillIndex[cellFillIndex])
            {
                const u8 boundaryFillIndex = layout->bridgeBreakBoundaryByFillIndex[cellFillIndex];
                if (breakInfoActual.endFillIndex > boundaryFillIndex &&
                    breakInfoActual.valueFillIndex > boundaryFillIndex)
                {
                    const u32 *breakStripUse = bodyStrip;
                    if (blinkOffActive && blinkOffBodyStrip &&
                        is_cell_in_trail_zone(&breakInfoActual, cellFillIndex))
                    {
                        breakStripUse = blinkOffBodyStrip;
                    }
                    upload_cell_if_needed(cell, breakStripUse,
                                          STRIP_INDEX_FULL);
                    continue;
                }
            }
        }

        /* === Early-exit: trivial cells (FULL = F or EMPTY = V) ===
         * ~70-80% of cells hit this path, skipping the 5-boolean
         * classification below. Saves ~50 cycles per cell. */
        /* Blink-off needs per-cell classification for mixed segments. */
        if (!blinkOffActive)
        {
            if (cellFillIndex < breakLow)
            {
                upload_cell_if_needed(cell, bodyStrip,
                                      STRIP_INDEX_FULL);
                continue;
            }
            if (cellFillIndex > breakHigh)
            {
                upload_cell_if_needed(cell, bodyStrip,
                                      STRIP_INDEX_EMPTY);
                continue;
            }
        }

        /* Lazy: remaining strips only for break zone cells (~20-30%).
         * bodyStrip and blinkOffBodyStrip already computed above. */
        const u32 *endStrip = select_base_strip(cell->endFillStrip45,
                                                layout->gainTilesetEndBySegment[segmentId],
                                                trailMode);
        const u32 *trailStrip = select_base_strip(cell->trailFillStrip64,
                                                  layout->gainTilesetTrailBySegment[segmentId],
                                                  trailMode);
        const u32 *blinkOffEndStrip = select_blink_strip(
            layout->blinkOffTilesetEndBySegment[segmentId],
            layout->gainBlinkOffTilesetEndBySegment[segmentId],
            trailMode);
        const u32 *blinkOffTrailStrip = select_blink_strip(
            layout->blinkOffTilesetTrailBySegment[segmentId],
            layout->gainBlinkOffTilesetTrailBySegment[segmentId],
            trailMode);

        /* === Break zone classification + strip selection === */
        const GaugeBreakInfo *breakInfoUsed = &breakInfo;
        u16 trailPixelsUse = trailPixelsRendered;
        u8 useBlinkOff = 0;

        if (blinkOffActive)
        {
            useBlinkOff = apply_blink_off_overrides(
                &breakInfoActual, cellFillIndex,
                blinkOffEndStrip, blinkOffTrailStrip, blinkOffBodyStrip,
                &endStrip, &trailStrip, &bodyStrip);
            if (useBlinkOff)
            {
                breakInfoUsed = &breakInfoActual;
                trailPixelsUse = trailPixelsActual;
            }
        }

        const CellBreakType cellType = classify_break_zone_cell(
            breakInfoUsed, cellFillIndex, (endStrip != NULL), (trailStrip != NULL));

        const u32 *stripToUse = bodyStrip;
        u8 desiredStripIndex = 0;

        switch (cellType)
        {
        case CELL_TYPE_END:
            stripToUse = endStrip;
            desiredStripIndex = s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
            break;

        case CELL_TYPE_TRAIL_BREAK:
            /* If next cell is full trail, force trailPx=8 for proper raccord */
            stripToUse = trailStrip;
            desiredStripIndex = breakInfoUsed->trailBreakSecondActive
                ? s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell][8]
                : s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell][breakInfoUsed->trailPxInBreakCell];
            break;

        case CELL_TYPE_TRAIL_BREAK2:
        {
            /* Same fill index as END but on TRAIL tileset */
            stripToUse = trailStrip;
            const u8 endIndex = s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
            desiredStripIndex = (endIndex == 8)
                ? s_trailTileIndexByValueTrail[0][endIndex]
                : endIndex;
            break;
        }

        case CELL_TYPE_TRAIL_FULL:
            stripToUse = trailStrip;
            desiredStripIndex = 7;
            break;

        case CELL_TYPE_VALUE_BREAK:
            stripToUse = bodyStrip;
            desiredStripIndex = breakInfoUsed->trailBreakActive
                ? breakInfoUsed->valuePxInBreakCell
                : s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
            break;

        case CELL_TYPE_REGION_FULL:
            stripToUse = bodyStrip;
            desiredStripIndex = STRIP_INDEX_FULL;
            break;

        case CELL_TYPE_REGION_EMPTY:
            stripToUse = bodyStrip;
            desiredStripIndex = STRIP_INDEX_EMPTY;
            break;

        case CELL_TYPE_REGION_TRAIL:
            stripToUse = trailStrip;
            desiredStripIndex = 7;
            break;

        case CELL_TYPE_DEFAULT:
        {
            /* Standard fill computation (no special break zone) */
            if (breakInfoUsed->endFillIndex != CACHE_INVALID_U8 &&
                cellFillIndex == breakInfoUsed->endFillIndex)
                stripToUse = endStrip;

            u8 valuePxInTile = 0;
            u8 trailPxInTile = 0;
            compute_fill_for_cell(layout, cell->cellIndex,
                                  valuePixels, trailPixelsUse,
                                  &valuePxInTile, &trailPxInTile);
            desiredStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
            break;
        }
        }
        upload_cell_if_needed(cell, stripToUse, desiredStripIndex);
    }
}

/**
 * Process dynamic mode -- Tilemap-based rendering algorithm.
 *
 * Unlike fixed mode, dynamic mode uses shared VRAM tiles and updates the
 * tilemap to point cells at the appropriate pre-loaded or streamed tile.
 * This uses less VRAM but more CPU (tilemap writes vs DMA).
 *
 * Algorithm:
 * 1. Compute break zone boundaries (breakLow..breakHigh)
 * 2. For each cell (countdown loop):
 *    a. Early-exit if trivially full or empty (tilemap-only update)
 *    b. Classify cell into visual mode (same as fixed mode)
 *    c. Select VRAM tile (pre-loaded standard or streamed partial)
 *    d. Stream partial tile data if segment/fill changed
 *    e. Update tilemap if VRAM tile assignment changed
 *
 * Pre-computed data used:
 * - layout->tilemapPosByCell: screen coordinates per cell (computed at init)
 * - dyn->cellValid: cell validity flags (avoids NULL checks in render loop)
 * - dyn->cellCurrentTileIndex: tilemap cache for change detection
 *
 * @param part                 Part to render
 * @param valuePixels          Current value fill in pixels
 * @param trailPixelsRendered  Current trail fill in pixels (after blink)
 * @param trailPixelsActual    Current trail fill in pixels (before blink, used for bridges)
 * @param blinkOffActive       1 if blink-off rendering is active this frame
 * @param blinkOnChanged       1 if blink phase toggled this frame
 * @param trailMode            Current trail mode (DAMAGE/GAIN/NONE)
 * @param trailModeChanged     1 if trail mode changed this frame
 *
 * Cost: ~50 cycles for trivial cells, ~300-500 cycles for break zone cells
 */
static void process_dynamic_mode(GaugePart *part,
                                 u16 valuePixels,
                                 u16 trailPixelsRendered,
                                 u16 trailPixelsActual,
                                 u8 blinkOffActive,
                                 u8 blinkOnChanged,
                                 u8 trailMode,
                                 u8 trailModeChanged)
{
    GaugeDynamic *dyn = &part->dyn;
    const GaugeLayout *layout = part->layout;

    /* Pre-compute tilemap attribute base (without tile index) */
    const u16 attrBase = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        layout->verticalFlip, layout->horizontalFlip, 0);

    /* --- Break zone computation ---
     * breakInfo: used for normal cell classification (trail with blink applied)
     * breakInfoActual: used for bridge visibility (trail WITHOUT blink, so bridges
     *            don't flicker on/off during blink phase) */
    GaugeBreakInfo breakInfo;
    compute_break_info(layout, valuePixels, trailPixelsRendered, trailMode, &breakInfo);
    GaugeBreakInfo breakInfoActual = breakInfo;
    if (trailPixelsActual != trailPixelsRendered)
        compute_break_info(layout, valuePixels, trailPixelsActual, trailMode, &breakInfoActual);

    /* Dynamic mode caches tile data in VRAM. When blink toggles or trail mode
     * changes, the cached standard tiles (full/empty/trail) may now come from
     * a different tileset (normal vs blink-off, damage vs gain). We must:
     * 1. Invalidate partial tile caches (forces re-upload on next use)
     * 2. Re-upload full body tiles if trail mode changed (DAMAGE<->GAIN)
     * 3. Re-upload full trail tiles with the correct tileset variant */
    const u8 layoutHasBlink = (trailMode == GAUGE_TRAIL_GAIN)
                             ? layout->hasGainBlinkOff : layout->hasBlinkOff;
    if ((blinkOnChanged && layoutHasBlink) || trailModeChanged)
    {
        reset_dynamic_blink_cache(dyn);
        if (trailModeChanged)
            reload_dynamic_full_body_tiles(dyn, layout, trailMode);
        reload_dynamic_full_trail_tiles(dyn, layout, blinkOffActive, trailMode);
    }

    /* Early-exit boundaries: cells outside [breakLow..breakHigh] are trivially
     * full or empty. Same logic as fixed mode. */
    const u8 breakLow = (breakInfo.valueBreakFillIndex != CACHE_INVALID_U8)
                       ? breakInfo.valueBreakFillIndex : breakInfo.valueFillIndex;
    const u8 breakHigh = breakInfo.trailFillIndex;

    /* Cap detection */
    const u8 capStartCellIndex = layout->cellIndexByFillIndex[0];
    const u8 capEndCellIndex = layout->cellIndexByFillIndex[layout->length - 1];
    const u8 capStartSegId = layout->segmentIdByCell[capStartCellIndex];
    const u8 capEndSegId = layout->segmentIdByCell[capEndCellIndex];
    u8 capStartEnabled = layout->capStartEnabled;
    u8 capEndEnabled = layout->capEndEnabled;
    /* Dynamic mode also requires VRAM tiles to actually be allocated */
    if (capStartEnabled && dyn->vramTileCapStart == 0) capStartEnabled = 0;
    if (capEndEnabled && dyn->vramTileCapEnd == 0) capEndEnabled = 0;

    /* Forward iteration required: dynamic mode shares partial VRAM tiles across cells.
     * With countdown, a lower-index cell could overwrite partial tile data needed by a
     * higher-index cell that was processed first. Forward order ensures the last writer
     * (highest fillIndex) wins, which matches the original design assumption.
     * NOTE: countdown is safe for fixed mode (each cell has its own VRAM tile). */
    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        /* Skip invalid cells (pre-computed in init_dynamic_tilemap) */
        if (!dyn->cellValid[cellIndex])
            continue;

        const u8 cellFillIndex = layout->fillIndexByCell[cellIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];

        /* === Cap end (always uses its tileset) === */
        if (cellIndex == capEndCellIndex && capEndEnabled && segmentId == capEndSegId)
        {
            u8 capValuePx = 0;
            u8 capTrailPx = 0;
            const u32 *capEndStrip = select_base_strip(
                layout->tilesetCapEndBySegment[segmentId],
                layout->gainTilesetCapEndBySegment[segmentId],
                trailMode);
            u16 capTrailPixels = trailPixelsRendered;

            const u32 *capEndBlinkStrip = select_blink_strip(
                layout->blinkOffTilesetCapEndBySegment[segmentId],
                layout->gainBlinkOffTilesetCapEndBySegment[segmentId],
                trailMode);

            if (blinkOffActive &&
                capEndBlinkStrip &&
                is_cell_in_trail_zone(&breakInfoActual, cellFillIndex))
            {
                capEndStrip = capEndBlinkStrip;
                capTrailPixels = trailPixelsActual;
            }

            compute_fill_for_cell(layout, cellIndex, valuePixels, capTrailPixels,
                                  &capValuePx, &capTrailPx);
            const u8 fillStripIndex = s_tileIndexByValueTrail[capValuePx][capTrailPx];
            if (dyn->cachedFillIndexCapEnd != fillStripIndex)
            {
                upload_fill_tile(capEndStrip,
                                 fillStripIndex,
                                 dyn->vramTileCapEnd,
                                 DMA);
                dyn->cachedFillIndexCapEnd = fillStripIndex;
            }
            if (dyn->cellCurrentTileIndex[cellIndex] != dyn->vramTileCapEnd)
            {
                VDP_setTileMapXY(WINDOW, attrBase | dyn->vramTileCapEnd,
                                 layout->tilemapPosByCell[cellIndex].x,
                                 layout->tilemapPosByCell[cellIndex].y);
                dyn->cellCurrentTileIndex[cellIndex] = dyn->vramTileCapEnd;
            }
            continue;
        }

        /* === Cap start (fixed border at cell 0) === */
        if (cellIndex == capStartCellIndex && capStartEnabled && segmentId == capStartSegId)
        {
            const u32 *capStartStrip = select_base_strip(
                layout->tilesetCapStartBySegment[segmentId],
                layout->gainTilesetCapStartBySegment[segmentId],
                trailMode);
            const u32 *capStartBreakStrip = select_base_strip(
                layout->tilesetCapStartBreakBySegment[segmentId],
                layout->gainTilesetCapStartBreakBySegment[segmentId],
                trailMode);
            const u32 *capStartTrailStrip = select_base_strip(
                layout->tilesetCapStartTrailBySegment[segmentId],
                layout->gainTilesetCapStartTrailBySegment[segmentId],
                trailMode);
            const u32 *capStartBlinkStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBySegment[segmentId],
                trailMode);
            const u32 *capStartBlinkBreakStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBreakBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBreakBySegment[segmentId],
                trailMode);
            const u32 *capStartBlinkTrailStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartTrailBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartTrailBySegment[segmentId],
                trailMode);

            if (capStartStrip)
            {
                CapStartResult capResult;
                if (blinkOffActive)
                {
                    CapStartResult capActual;
                    classify_cap_start_with_strips(&breakInfoActual,
                                                   cellFillIndex,
                                                   capStartStrip,
                                                   capStartBreakStrip,
                                                   capStartTrailStrip,
                                                   &capActual);

                    const u8 inTrailZone = is_cell_in_trail_zone(&breakInfoActual, cellFillIndex);
                    const u32 *blinkStrip = NULL;

                    if (inTrailZone)
                    {
                        if (capActual.usesTrail)
                            blinkStrip = capStartBlinkTrailStrip;
                        else if (capActual.usesBreak)
                            blinkStrip = capStartBlinkBreakStrip;
                        else
                            blinkStrip = capStartBlinkStrip;
                    }

                    if (blinkStrip)
                    {
                        capActual.strip = blinkStrip;
                        capResult = capActual;
                    }
                    else
                    {
                        classify_cap_start_with_strips(&breakInfo,
                                                       cellFillIndex,
                                                       capStartStrip,
                                                       capStartBreakStrip,
                                                       capStartTrailStrip,
                                                       &capResult);
                    }
                }
                else
                {
                    classify_cap_start_with_strips(&breakInfo,
                                                   cellFillIndex,
                                                   capStartStrip,
                                                   capStartBreakStrip,
                                                   capStartTrailStrip,
                                                   &capResult);
                }

                /* Stream cap start tile if strip/index/variant changed */
                if (dyn->cachedFillIndexCapStart != capResult.fillStripIndex ||
                    dyn->loadedCapStartUsesBreak != capResult.usesBreak ||
                    dyn->loadedCapStartUsesTrail != capResult.usesTrail)
                {
                    upload_fill_tile(capResult.strip, capResult.fillStripIndex,
                                     dyn->vramTileCapStart, DMA);
                    dyn->cachedFillIndexCapStart = capResult.fillStripIndex;
                    dyn->loadedCapStartUsesBreak = capResult.usesBreak;
                    dyn->loadedCapStartUsesTrail = capResult.usesTrail;
                }

                /* Update tilemap if tile assignment changed */
                if (dyn->cellCurrentTileIndex[cellIndex] != dyn->vramTileCapStart)
                {
                    VDP_setTileMapXY(WINDOW, attrBase | dyn->vramTileCapStart,
                                     layout->tilemapPosByCell[cellIndex].x,
                                     layout->tilemapPosByCell[cellIndex].y);
                    dyn->cellCurrentTileIndex[cellIndex] = dyn->vramTileCapStart;
                }
                continue;
            }
        }

        /* === Bridge/BREAK forced rendering (D/B before gauge END) ===
         * Bridge cells sit at segment boundaries. When the gauge's value/trail
         * edge is past this cell, the bridge shows a transition tile.
         * breakInfoActual uses trailPixelsActual (ignoring blink) so bridges don't
         * flicker during blink phase. */
        if (breakInfoActual.endFillIndex != CACHE_INVALID_U8)
        {
            /* Bridge end cell: last cell of a segment with a bridge tileset */
            if (layout->bridgeEndByFillIndex[cellFillIndex] &&
                breakInfoActual.endFillIndex > cellFillIndex &&
                breakInfoActual.valueFillIndex > cellFillIndex)
            {
                const u16 vramTileBridge = dyn->vramTileBridge[segmentId];
                if (vramTileBridge != 0)
                {
                    const u8 nextFillIndex = (u8)(cellFillIndex + 1);
                    const u8 bridgeInTrailZone = is_cell_in_trail_zone(&breakInfoActual, nextFillIndex);
                    const u32 *bridgeStrip = select_base_strip(
                        layout->tilesetBridgeBySegment[segmentId],
                        layout->gainTilesetBridgeBySegment[segmentId],
                        trailMode);
                    const u32 *blinkOffBridgeStrip = select_blink_strip(
                        layout->blinkOffTilesetBridgeBySegment[segmentId],
                        layout->gainBlinkOffTilesetBridgeBySegment[segmentId],
                        trailMode);
                    const u8 useBlinkOffBridge = (blinkOffActive &&
                                                  blinkOffBridgeStrip &&
                                                  bridgeInTrailZone);
                    const GaugeBreakInfo *bridgeBreakInfo = useBlinkOffBridge ? &breakInfoActual : &breakInfo;
                    const u16 bridgeTrailPixels = useBlinkOffBridge ? trailPixelsActual : trailPixelsRendered;
                    const u32 *bridgeStripUse = useBlinkOffBridge
                                                ? blinkOffBridgeStrip
                                                : bridgeStrip;
                    const u8 bridgeIdx = compute_bridge_strip_index(
                        layout, bridgeBreakInfo, cellFillIndex, valuePixels, bridgeTrailPixels);

                    if (dyn->cachedFillIndexBridge[segmentId] != bridgeIdx)
                    {
                        upload_fill_tile(bridgeStripUse,
                                         bridgeIdx, vramTileBridge, DMA);
                        dyn->cachedFillIndexBridge[segmentId] = bridgeIdx;
                    }
                    if (dyn->cellCurrentTileIndex[cellIndex] != vramTileBridge)
                    {
                        VDP_setTileMapXY(WINDOW, attrBase | vramTileBridge,
                                         layout->tilemapPosByCell[cellIndex].x,
                                         layout->tilemapPosByCell[cellIndex].y);
                        dyn->cellCurrentTileIndex[cellIndex] = vramTileBridge;
                    }
                }
                continue;
            }

            /* Forced BREAK cell: cell before a bridge that must show as fully filled */
            if (layout->bridgeBreakByFillIndex[cellFillIndex])
            {
                const u8 boundaryFillIndex = layout->bridgeBreakBoundaryByFillIndex[cellFillIndex];
                if (breakInfoActual.endFillIndex > boundaryFillIndex &&
                    breakInfoActual.valueFillIndex > boundaryFillIndex)
                {
                    const u16 vramTile = dyn->vramTileFullValue[segmentId];
                    if (dyn->cellCurrentTileIndex[cellIndex] != vramTile)
                    {
                        VDP_setTileMapXY(WINDOW, attrBase | vramTile,
                                         layout->tilemapPosByCell[cellIndex].x,
                                         layout->tilemapPosByCell[cellIndex].y);
                        dyn->cellCurrentTileIndex[cellIndex] = vramTile;
                    }
                    continue;
                }
            }
        }

        /* === Early-exit: trivial cells (FULL = F or EMPTY = V) ===
         * Standard tiles are pre-loaded in VRAM, only tilemap update needed.
         * ~70-80% of cells hit this path. */
        /* Blink-off needs per-cell classification for mixed segments. */
        if (!blinkOffActive)
        {
            if (cellFillIndex < breakLow)
            {
                const u16 vt = dyn->vramTileFullValue[segmentId];
                if (dyn->cellCurrentTileIndex[cellIndex] != vt)
                {
                    VDP_setTileMapXY(WINDOW, attrBase | vt,
                                     layout->tilemapPosByCell[cellIndex].x,
                                     layout->tilemapPosByCell[cellIndex].y);
                    dyn->cellCurrentTileIndex[cellIndex] = vt;
                }
                continue;
            }
            if (cellFillIndex > breakHigh)
            {
                const u16 vt = dyn->vramTileEmpty[segmentId];
                if (dyn->cellCurrentTileIndex[cellIndex] != vt)
                {
                    VDP_setTileMapXY(WINDOW, attrBase | vt,
                                     layout->tilemapPosByCell[cellIndex].x,
                                     layout->tilemapPosByCell[cellIndex].y);
                    dyn->cellCurrentTileIndex[cellIndex] = vt;
                }
                continue;
            }
        }

        /* === Break zone: full classification needed (END(E)/TRAIL(T)/VALUE_BREAK(B)/FULL(F)/EMPTY(V)) === */
        const GaugeBreakInfo *breakInfoUsed = &breakInfo;
        u16 trailPixelsUse = trailPixelsRendered;
        const u32 *bodyStrip = select_base_strip(layout->tilesetBySegment[segmentId],
                                                 layout->gainTilesetBySegment[segmentId],
                                                 trailMode);
        const u32 *endStrip = select_base_strip(layout->tilesetEndBySegment[segmentId],
                                                layout->gainTilesetEndBySegment[segmentId],
                                                trailMode);
        const u32 *trailStrip = select_base_strip(layout->tilesetTrailBySegment[segmentId],
                                                  layout->gainTilesetTrailBySegment[segmentId],
                                                  trailMode);
        const u32 *blinkOffEndStrip = select_blink_strip(
            layout->blinkOffTilesetEndBySegment[segmentId],
            layout->gainBlinkOffTilesetEndBySegment[segmentId],
            trailMode);
        const u32 *blinkOffTrailStrip = select_blink_strip(
            layout->blinkOffTilesetTrailBySegment[segmentId],
            layout->gainBlinkOffTilesetTrailBySegment[segmentId],
            trailMode);
        const u32 *blinkOffBodyStrip = select_blink_strip(
            layout->blinkOffTilesetBySegment[segmentId],
            layout->gainBlinkOffTilesetBySegment[segmentId],
            trailMode);
        u8 useBlinkOff = 0;

        if (blinkOffActive)
        {
            useBlinkOff = apply_blink_off_overrides(
                &breakInfoActual, cellFillIndex,
                blinkOffEndStrip, blinkOffTrailStrip, blinkOffBodyStrip,
                &endStrip, &trailStrip, &bodyStrip);
            if (useBlinkOff)
            {
                breakInfoUsed = &breakInfoActual;
                trailPixelsUse = trailPixelsActual;
            }
        }

        const u32 *trailStripUse = trailStrip ? trailStrip : bodyStrip;

        const CellBreakType cellType = classify_break_zone_cell(
            breakInfoUsed, cellFillIndex, (endStrip != NULL), (trailStrip != NULL));

        u16 vramTile = 0;
        u8 needsUpload = 0;
        u8 fillStripIndex = 0;
        const u32 *stripToUse = NULL;

        switch (cellType)
        {
        case CELL_TYPE_END:
            stripToUse = endStrip;
            fillStripIndex = s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
            vramTile = dyn->vramTilePartialEnd;
            if (dyn->loadedSegmentPartialEnd != segmentId ||
                dyn->cachedFillIndexPartialEnd != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialEnd = segmentId;
                dyn->cachedFillIndexPartialEnd = fillStripIndex;
            }
            break;

        case CELL_TYPE_TRAIL_BREAK:
            stripToUse = trailStripUse;
            fillStripIndex = breakInfoUsed->trailBreakSecondActive
                ? s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell][8]
                : s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell][breakInfoUsed->trailPxInBreakCell];
            vramTile = dyn->vramTilePartialTrail;
            if (dyn->loadedSegmentPartialTrail != segmentId ||
                dyn->cachedFillIndexPartialTrail != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialTrail = segmentId;
                dyn->cachedFillIndexPartialTrail = fillStripIndex;
            }
            break;

        case CELL_TYPE_TRAIL_BREAK2:
        {
            stripToUse = trailStripUse;
            const u8 endIndex = s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
            fillStripIndex = (endIndex == 8)
                ? s_trailTileIndexByValueTrail[0][endIndex]
                : endIndex;
            vramTile = dyn->vramTilePartialTrailSecond;
            if (dyn->loadedSegmentPartialTrailSecond != segmentId ||
                dyn->cachedFillIndexPartialTrailSecond != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialTrailSecond = segmentId;
                dyn->cachedFillIndexPartialTrailSecond = fillStripIndex;
            }
            break;
        }

        case CELL_TYPE_TRAIL_FULL:
            vramTile = dyn->vramTileFullTrail[segmentId];
            break;

        case CELL_TYPE_VALUE_BREAK:
            stripToUse = bodyStrip;
            fillStripIndex = breakInfoUsed->trailBreakActive
                ? breakInfoUsed->valuePxInBreakCell
                : s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
            vramTile = dyn->vramTilePartialValue;
            if (dyn->loadedSegmentPartialValue != segmentId ||
                dyn->cachedFillIndexPartialValue != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialValue = segmentId;
                dyn->cachedFillIndexPartialValue = fillStripIndex;
            }
            break;

        case CELL_TYPE_REGION_FULL:
            vramTile = dyn->vramTileFullValue[segmentId];
            break;

        case CELL_TYPE_REGION_EMPTY:
            vramTile = dyn->vramTileEmpty[segmentId];
            break;

        case CELL_TYPE_REGION_TRAIL:
            vramTile = dyn->vramTileFullTrail[segmentId];
            break;

        case CELL_TYPE_DEFAULT:
        {
            /* Standard fill computation: determine which partial tile to use */
            if (cellFillIndex < breakInfoUsed->valueFillIndex)
            {
                vramTile = dyn->vramTileFullValue[segmentId];
            }
            else if (cellFillIndex > breakInfoUsed->valueFillIndex && cellFillIndex < breakInfoUsed->trailFillIndex)
            {
                vramTile = dyn->vramTileFullTrail[segmentId];
            }
            else if (cellFillIndex > breakInfoUsed->trailFillIndex)
            {
                vramTile = dyn->vramTileEmpty[segmentId];
            }
            else if (cellFillIndex == breakInfoUsed->valueFillIndex && cellFillIndex == breakInfoUsed->trailFillIndex)
            {
                /* Both value and trail partial in same cell */
                u8 valuePxInTile, trailPxInTile;
                compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsUse,
                                      &valuePxInTile, &trailPxInTile);
                stripToUse = bodyStrip;
                fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
                vramTile = dyn->vramTilePartialValue;
                if (dyn->loadedSegmentPartialValue != segmentId ||
                    dyn->cachedFillIndexPartialValue != fillStripIndex)
                {
                    needsUpload = 1;
                    dyn->loadedSegmentPartialValue = segmentId;
                    dyn->cachedFillIndexPartialValue = fillStripIndex;
                }
            }
            else if (cellFillIndex == breakInfoUsed->valueFillIndex)
            {
                /* Value partial cell (trail is full=8) */
                u8 valuePxInTile, trailPxInTile;
                compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsUse,
                                      &valuePxInTile, &trailPxInTile);
                stripToUse = bodyStrip;
                fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][8];
                vramTile = dyn->vramTilePartialValue;
                if (dyn->loadedSegmentPartialValue != segmentId ||
                    dyn->cachedFillIndexPartialValue != fillStripIndex)
                {
                    needsUpload = 1;
                    dyn->loadedSegmentPartialValue = segmentId;
                    dyn->cachedFillIndexPartialValue = fillStripIndex;
                }
            }
            else  /* cellFillIndex == breakInfoUsed->trailFillIndex */
            {
                /* Trail partial cell (value=0) */
                u8 valuePxInTile, trailPxInTile;
                compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsUse,
                                      &valuePxInTile, &trailPxInTile);
                stripToUse = bodyStrip;
                fillStripIndex = s_tileIndexByValueTrail[0][trailPxInTile];
                vramTile = dyn->vramTilePartialTrail;
                if (dyn->loadedSegmentPartialTrail != segmentId ||
                    dyn->cachedFillIndexPartialTrail != fillStripIndex)
                {
                    needsUpload = 1;
                    dyn->loadedSegmentPartialTrail = segmentId;
                    dyn->cachedFillIndexPartialTrail = fillStripIndex;
                }
            }
            break;
        }
        }
        /* Upload partial tile if needed */
        if (needsUpload && stripToUse)
            upload_fill_tile(stripToUse, fillStripIndex, vramTile, DMA);

        /* Update tilemap only if tile changed (change detection optimization) */
        if (dyn->cellCurrentTileIndex[cellIndex] != vramTile)
        {
            const u16 attr = attrBase | vramTile;
            VDP_setTileMapXY(WINDOW, attr,
                             layout->tilemapPosByCell[cellIndex].x,
                             layout->tilemapPosByCell[cellIndex].y);
            dyn->cellCurrentTileIndex[cellIndex] = vramTile;
        }
    }
}

/**
 * Determine the PIP visual state for a single cell.
 *
 * Priority rules:
 * 1. Cell fully covered by value        -> PIP_STATE_VALUE
 * 2. Cell NOT covered by actual trail    -> PIP_STATE_EMPTY
 * 3. Blink-off active (damage)           -> PIP_STATE_BLINK_OFF
 * 4. Blink-off active (gain)             -> PIP_STATE_EMPTY (hidden)
 * 5. Trail is gain mode                  -> PIP_STATE_GAIN
 * 6. Default (damage trail visible)      -> PIP_STATE_LOSS
 *
 * @return One of PIP_STATE_* constants (0..4)
 */
static inline u8 select_pip_state_for_cell(const GaugeLayout *layout,
                                           u8 cellIndex,
                                           u16 valuePixels,
                                           u16 trailPixelsRendered,
                                           u16 trailPixelsActual,
                                           u8 blinkOffActive,
                                           u8 trailMode)
{
    u8 valuePxInTile;
    u8 trailRenderedPxInTile;
    compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsRendered,
                          &valuePxInTile, &trailRenderedPxInTile);
    (void)trailRenderedPxInTile;

    if (valuePxInTile == GAUGE_PIXELS_PER_TILE)
        return PIP_STATE_VALUE;

    u8 valueUnused;
    u8 trailActualPxInTile;
    compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsActual,
                          &valueUnused, &trailActualPxInTile);

    if (trailActualPxInTile != GAUGE_PIXELS_PER_TILE)
        return PIP_STATE_EMPTY;

    if (blinkOffActive)
    {
        if (trailMode == GAUGE_TRAIL_DAMAGE)
            return PIP_STATE_BLINK_OFF;
        return PIP_STATE_EMPTY; /* Gain trail: fallback to hidden trail on blink OFF. */
    }

    if (trailMode == GAUGE_TRAIL_GAIN)
        return PIP_STATE_GAIN;

    return PIP_STATE_LOSS;
}

/**
 * PIP-mode renderer: update each cell's tile from compact strip based on pip state.
 *
 * For each cell, computes its PIP state (VALUE/EMPTY/LOSS/GAIN/BLINK_OFF),
 * selects the corresponding tile from the compact strip, and uploads via DMA
 * if changed. Uses fixed VRAM (one tile per cell) regardless of VRAM mode.
 */
static void process_pip_mode(GaugePart *part,
                             u16 valuePixels,
                             u16 trailPixelsRendered,
                             u16 trailPixelsActual,
                             u8 blinkOffActive,
                             u8 trailMode)
{
    const GaugeLayout *layout = part->layout;

    for (u8 i = 0; i < part->cellCount; i++)
    {
        GaugeStreamCell *cell = &part->cells[i];
        const u8 cellIndex = cell->cellIndex;
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *pipStrip = layout->pipTilesetBySegment[segmentId];
        if (!pipStrip)
            continue;

        const u8 fillIndex = layout->fillIndexByCell[cellIndex];
        const u8 pipIndex = layout->pipIndexByFillIndex[fillIndex];
        if (pipIndex == CACHE_INVALID_U8)
            continue;

        u8 pipWidth = layout->pipWidthByPipIndex[pipIndex];
        if (pipWidth == 0)
            pipWidth = 1;

        u8 localTile = layout->pipLocalTileByFillIndex[fillIndex];
        if (localTile >= pipWidth)
            localTile = 0;

        const u8 pipState = select_pip_state_for_cell(layout, cellIndex,
                                                      valuePixels, trailPixelsRendered, trailPixelsActual,
                                                      blinkOffActive, trailMode);
        const u8 stripIndex = (u8)(pipState * pipWidth + localTile);

        upload_cell_if_needed(cell, pipStrip, stripIndex);
    }
}


/* =============================================================================
   Render/update dispatchers
   ============================================================================= */

static void render_part_fill_dynamic(GaugePart *part,
                                     u16 valuePixels,
                                     u16 trailPixelsRendered,
                                     u16 trailPixelsActual,
                                     u8 blinkOffActive,
                                     u8 blinkOnChanged,
                                     u8 trailMode,
                                     u8 trailModeChanged)
{
    process_dynamic_mode(part, valuePixels, trailPixelsRendered, trailPixelsActual,
                         blinkOffActive, blinkOnChanged, trailMode, trailModeChanged);
}

static void render_part_fill_fixed(GaugePart *part,
                                   u16 valuePixels,
                                   u16 trailPixelsRendered,
                                   u16 trailPixelsActual,
                                   u8 blinkOffActive,
                                   u8 blinkOnChanged,
                                   u8 trailMode,
                                   u8 trailModeChanged)
{
    (void)blinkOnChanged;
    (void)trailModeChanged;
    process_fixed_mode(part, valuePixels, trailPixelsRendered, trailPixelsActual,
                       blinkOffActive, trailMode);
}

static void render_part_pip_dynamic(GaugePart *part,
                                    u16 valuePixels,
                                    u16 trailPixelsRendered,
                                    u16 trailPixelsActual,
                                    u8 blinkOffActive,
                                    u8 blinkOnChanged,
                                    u8 trailMode,
                                    u8 trailModeChanged)
{
    (void)blinkOnChanged;
    (void)trailModeChanged;
    process_pip_mode(part, valuePixels, trailPixelsRendered, trailPixelsActual,
                     blinkOffActive, trailMode);
}

static void render_part_pip_fixed(GaugePart *part,
                                  u16 valuePixels,
                                  u16 trailPixelsRendered,
                                  u16 trailPixelsActual,
                                  u8 blinkOffActive,
                                  u8 blinkOnChanged,
                                  u8 trailMode,
                                  u8 trailModeChanged)
{
    (void)blinkOnChanged;
    (void)trailModeChanged;
    process_pip_mode(part, valuePixels, trailPixelsRendered, trailPixelsActual,
                     blinkOffActive, trailMode);
}

/**
 * Select the correct render handler based on value mode and VRAM mode.
 *
 * Returns one of 4 handlers: fill_dynamic, fill_fixed, pip_dynamic, pip_fixed.
 */
static GaugePartRenderHandler *resolve_part_render_handler(GaugeValueMode valueMode,
                                                           GaugeVramMode vramMode)
{
    if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        return (vramMode == GAUGE_VRAM_DYNAMIC) ? render_part_pip_dynamic
                                                : render_part_pip_fixed;
    }

    return (vramMode == GAUGE_VRAM_DYNAMIC) ? render_part_fill_dynamic
                                            : render_part_fill_fixed;
}

static void gauge_tick_and_render_fill(Gauge *gauge);
static void gauge_tick_and_render_pip(Gauge *gauge);

/** Select the tick-and-render handler: fill (continuous) or pip (discrete). */
static GaugeTickAndRenderHandler *resolve_tick_and_render_handler(GaugeValueMode valueMode)
{
    return (valueMode == GAUGE_VALUE_MODE_PIP)
        ? gauge_tick_and_render_pip
        : gauge_tick_and_render_fill;
}


/* =============================================================================
   GaugePart initialization (internal)
   ============================================================================= */

/**
 * Initialize a GaugePart with all parameters.
 */
static u8 GaugePart_initInternal(GaugePart *part,
                                 const Gauge *gauge,
                                 GaugeLayout *layout,
                                 u16 originX, u16 originY,
                                 u16 vramBase,
                                 GaugeVramMode vramMode)
{
    if (!part || !gauge || !layout || layout->length == 0)
        return 0;

    part->originX = originX;
    part->originY = originY;
    part->vramBase = vramBase;
    part->vramMode = vramMode;
    part->renderHandler = resolve_part_render_handler(gauge->valueMode, vramMode);
    part->layout = layout;
    part->cells = NULL;
    part->cellCount = 0;
    part->dyn.segmentCount = 0;
    part->dyn.cellCount = 0;

    GaugeLayout_retain(layout);

    if (gauge->valueMode == GAUGE_VALUE_MODE_PIP || vramMode == GAUGE_VRAM_FIXED)
    {
        part->cells = (GaugeStreamCell *)gauge_alloc_bytes(
            (u16)(layout->length * (u8)sizeof(GaugeStreamCell)));
        if (!part->cells)
        {
            GaugeLayout_release(layout);
            return 0;
        }
    }

    if (gauge->valueMode == GAUGE_VALUE_MODE_PIP)
    {
        /* Compact PIP renderer uses per-cell streaming for both VRAM modes. */
        write_tilemap_pip_init(part);
        return 1;
    }

    /* Initialize based on VRAM mode */
    if (part->vramMode == GAUGE_VRAM_DYNAMIC)
    {
        /* Dynamic mode initialization */
        if (!init_dynamic_vram(&part->dyn, part->layout, part->vramBase, gauge->logic.trailEnabled))
        {
            gauge_free_ptr((void **)&part->cells);
            GaugeLayout_release(layout);
            return 0;
        }
        preload_dynamic_standard_tiles(&part->dyn, part->layout, gauge->logic.trailEnabled);
        init_dynamic_tilemap(part);
    }
    else
    {
        /* Fixed mode */
        write_tilemap_fixed_init(part);
    }

    return 1;
}

static void GaugePart_releaseInternal(GaugePart *part)
{
    if (!part)
        return;

    if (part->layout)
    {
        GaugeLayout_release((GaugeLayout *)part->layout);
        part->layout = NULL;
    }

    gauge_free_ptr((void **)&part->cells);
    free_dynamic_buffers(&part->dyn);
    part->cellCount = 0;
}

static u8 ensure_part_capacity(Gauge *gauge, u8 requiredCount)
{
    if (requiredCount <= gauge->partCapacity)
        return 1;

    u8 newCapacity = (gauge->partCapacity == 0) ? 2 : gauge->partCapacity;
    while (newCapacity < requiredCount && newCapacity < GAUGE_MAX_PARTS)
        newCapacity = (u8)(newCapacity << 1);
    if (newCapacity > GAUGE_MAX_PARTS)
        newCapacity = GAUGE_MAX_PARTS;
    if (newCapacity < requiredCount)
        return 0;

    GaugePart **newParts = (GaugePart **)gauge_alloc_bytes(
        (u16)(newCapacity * (u8)sizeof(GaugePart *)));
    if (!newParts)
        return 0;

    for (u8 i = 0; i < gauge->partCount; i++)
        newParts[i] = gauge->parts[i];

    gauge_free_ptr((void **)&gauge->parts);
    gauge->parts = newParts;
    gauge->partCapacity = newCapacity;
    return 1;
}



/* =============================================================================
   Gauge public API
   ============================================================================= */

/**
 * Populate a value-to-pixels LUT for FILL mode scaling.
 * Formula: dest[v] = round(v * maxFillPixels / maxValue).
 * The +maxValue/2 provides rounding instead of truncation.
 *
 * @param dest          Destination array (must have maxValue + 1 entries)
 * @param maxValue      Maximum logical value
 * @param maxFillPixels Total pixel span
 */
static void fill_value_to_pixels_lut(u16 *dest, u16 maxValue, u16 maxFillPixels)
{
    if (maxValue == 0)
    {
        dest[0] = 0;
        return;
    }

    const u16 halfMax = maxValue >> 1;
    for (u16 v = 0; v <= maxValue; v++)
    {
        const u32 numerator = (u32)v * (u32)maxFillPixels + halfMax;
        dest[v] = (u16)(numerator / maxValue);
    }
}

/**
 * Compute total pixel span of all pips in a layout.
 *
 * Each pip contributes (pipWidth * 8) pixels. Used to validate that
 * maxFillPixels matches the layout's actual pixel capacity.
 */
static u16 compute_pip_total_pixels(const GaugeLayout *layout)
{
    u16 totalPixels = 0;
    for (u8 pipIndex = 0; pipIndex < layout->pipCount; pipIndex++)
    {
        totalPixels = (u16)(totalPixels + ((u16)layout->pipWidthByPipIndex[pipIndex] << TILE_TO_PIXEL_SHIFT));
    }
    return totalPixels;
}

/**
 * Validate PIP layout configuration (called once on first Gauge_addPart).
 *
 * Checks: pipCount > 0, all segments have compact tilesets, pipIndex mapping
 * is valid, pip widths match segment styles, maxValue == pipCount, and
 * total pixel span matches maxFillPixels. Logs errors via KLog on failure.
 *
 * @return 1 if valid, 0 if any check fails
 */
static u8 validate_pip_layout(const GaugeLogic *logic, const GaugeLayout *layout)
{
    if (layout->pipCount == 0)
    {
        KLog("Gauge PIP config error: layout pipCount is zero");
        return 0;
    }

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        if (layout->pipTilesetBySegment[segmentId] == NULL)
        {
            KLog_U1("Gauge PIP config error missing compact tileset segmentId: ", segmentId);
            return 0;
        }
    }

    for (u8 fillIndex = 0; fillIndex < layout->length; fillIndex++)
    {
        const u8 pipIndex = layout->pipIndexByFillIndex[fillIndex];
        const u8 localTile = layout->pipLocalTileByFillIndex[fillIndex];

        if (pipIndex == CACHE_INVALID_U8)
        {
            KLog_U1("Gauge PIP config error invalid pipIndex fillIndex: ", fillIndex);
            return 0;
        }

        if (localTile == 0)
        {
            const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
            const u8 segmentId = layout->segmentIdByCell[cellIndex];
            u8 styleWidth = layout->pipWidthBySegment[segmentId];
            if (styleWidth == 0)
                styleWidth = 1;

            if (layout->pipWidthByPipIndex[pipIndex] != styleWidth)
            {
                KLog_U2("Gauge PIP config error pipWidth style: ", styleWidth,
                        " pipWidth run: ", layout->pipWidthByPipIndex[pipIndex]);
                return 0;
            }
        }
    }

    if (logic->maxValue != layout->pipCount)
    {
        KLog_U2("Gauge PIP config error maxValue: ", logic->maxValue,
                " pipCount: ", layout->pipCount);
        return 0;
    }

    const u16 totalPixels = compute_pip_total_pixels(layout);
    if (totalPixels != logic->maxFillPixels)
    {
        KLog_U2("Gauge PIP config error maxFillPx: ", logic->maxFillPixels,
                " pipTotalPx: ", totalPixels);
        return 0;
    }

    return 1;
}

/**
 * Populate a value-to-pixels LUT for PIP mode from layout pip widths.
 * dest[0] = 0, dest[v] = cumulative pixel boundary for pip v.
 * Example: 4 pips of width 2 tiles each = [0, 16, 32, 48, 64].
 *
 * @param dest      Destination array (must have maxValue + 1 entries)
 * @param maxValue  Number of pips (= layout->pipCount)
 * @param layout    Layout with pip configuration
 */
static void fill_pip_value_lut(u16 *dest, u16 maxValue, const GaugeLayout *layout)
{
    dest[0] = 0;
    u16 cumulativePixels = 0;
    for (u16 value = 1; value <= maxValue; value++)
    {
        const u8 pipWidth = layout->pipWidthByPipIndex[value - 1];
        cumulativePixels = (u16)(cumulativePixels + ((u16)pipWidth << TILE_TO_PIXEL_SHIFT));
        dest[value] = cumulativePixels;
    }
}

/**
 * Initialize a Gauge from a GaugeInit config.
 *
 * Sets up the embedded GaugeLogic. maxFillPixels is derived from
 * init->layout->length * GAUGE_PIXELS_PER_TILE.
 *
 * If maxValue != maxFillPixels (FILL mode) or PIP mode, a value-to-pixels
 * LUT is built into gauge->logic.valueToPixelsData (heap allocated).
 *
 * Trail and value animation are disabled by default.
 */
void Gauge_init(Gauge *gauge, const GaugeInit *init)
{
    if (!gauge || !init || !init->layout)
        return;

    /* Gauge_init expects a fresh Gauge (or one previously released). */
    memset(gauge, 0, sizeof(*gauge));

    u16 maxValue = init->maxValue;
    const u16 maxFillPixels = (u16)(init->layout->length * GAUGE_PIXELS_PER_TILE);
    GaugeValueMode valueMode = init->valueMode;

    if (maxValue > GAUGE_LUT_CAPACITY)
    {
        KLog_U2("Gauge init maxValue: ", maxValue,
                " clamped to: ", GAUGE_LUT_CAPACITY);
        maxValue = GAUGE_LUT_CAPACITY;
    }

    if (valueMode != GAUGE_VALUE_MODE_PIP)
        valueMode = GAUGE_VALUE_MODE_FILL;
    gauge->logic.valueToPixelsData = NULL;

    /* Build value-to-pixels LUT BEFORE GaugeLogic_init so that
     * initial pixel positions are computed correctly.
     * GaugeLogic_initWithAnim does NOT overwrite valueToPixelsData[].
     *
     * - FILL mode with scaling (maxValue != maxFillPixels): interpolation LUT
     * - PIP mode: pip boundary LUT from layout
     * - FILL mode 1:1 (maxValue == maxFillPixels): no LUT, direct mapping */
    const u16 *lut = NULL;
    if (valueMode == GAUGE_VALUE_MODE_FILL && maxValue != maxFillPixels)
    {
        gauge->logic.valueToPixelsData = (u16 *)gauge_alloc_bytes(
            (u16)((maxValue + 1) * (u8)sizeof(u16)));
        if (!gauge->logic.valueToPixelsData)
        {
            KLog("Gauge init alloc failed: valueToPixelsData");
            return;
        }
        fill_value_to_pixels_lut(gauge->logic.valueToPixelsData, maxValue, maxFillPixels);
        lut = gauge->logic.valueToPixelsData;
    }
    else if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        u16 validatedPipCount = init->layout->pipCount;
        if (validatedPipCount > GAUGE_LUT_CAPACITY)
        {
            KLog_U2("Gauge PIP pipCount: ", validatedPipCount,
                    " clamped to: ", GAUGE_LUT_CAPACITY);
            validatedPipCount = GAUGE_LUT_CAPACITY;
        }
        if (maxValue != validatedPipCount)
        {
            KLog_U2("Gauge PIP maxValue: ", maxValue,
                    " adjusted to pipCount: ", validatedPipCount);
            maxValue = validatedPipCount;
        }

        gauge->logic.valueToPixelsData = (u16 *)gauge_alloc_bytes(
            (u16)((maxValue + 1) * (u8)sizeof(u16)));
        if (!gauge->logic.valueToPixelsData)
        {
            KLog("Gauge init alloc failed: PIP LUT");
            return;
        }

        fill_pip_value_lut(gauge->logic.valueToPixelsData, maxValue, init->layout);
        lut = gauge->logic.valueToPixelsData;
    }

    /* Initialize logic with the correct LUT pointer.
     * Trail disabled by default - use Gauge_setTrailAnim() to enable. */
    GaugeLogic_init(&gauge->logic, maxValue, maxFillPixels, lut,
                    0, init->initialValue);

    /* Initialize part storage (grown on demand by Gauge_addPart). */
    gauge->parts = NULL;
    gauge->partCount = 0;
    gauge->partCapacity = 0;

    /* Runtime dispatch state */
    gauge->valueMode = valueMode;
    gauge->tickAndRenderHandler = resolve_tick_and_render_handler(valueMode);

    /* VRAM allocation state */
    gauge->vramBase = init->vramBase;
    gauge->vramNextOffset = 0;
    gauge->vramMode = init->vramMode;
}

/** Configure value animation (0=instant changes, 1=animated transitions). */
void Gauge_setValueAnim(Gauge *gauge, u8 enabled, u8 shift)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->valueAnimEnabled = enabled ? 1 : 0;
    logic->valueAnimShift = (shift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : shift;
}

/** Configure trail animation (0=no trail, 1=hold+blink+shrink trail effect). */
void Gauge_setTrailAnim(Gauge *gauge, u8 enabled, u8 shift, u8 blinkShift)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->trailEnabled = enabled ? 1 : 0;
    logic->trailAnimShift = (shift == 0) ? GAUGE_DEFAULT_TRAIL_ANIM_SHIFT : shift;
    logic->blinkShift = (blinkShift == 0) ? GAUGE_DEFAULT_BLINK_SHIFT : blinkShift;

    /* If disabling trail, reset trail state to match value */
    if (!logic->trailEnabled)
    {
        logic->trailPixels = logic->valuePixels;
        logic->holdFramesRemaining = 0;
        logic->blinkFramesRemaining = 0;
    }
}

/** Override maxFillPixels (no-op if unchanged, no ordering constraint). */
void Gauge_setMaxFillPixels(Gauge *gauge, u16 maxFillPixels)
{
    if (!gauge) return;
    GaugeLogic *logic = &gauge->logic;

    /* No-op if unchanged */
    if (logic->maxFillPixels == maxFillPixels)
        return;

    logic->maxFillPixels = maxFillPixels;

    /* Rebuild LUT buffer (or clear LUT pointer if 1:1). */
    if (gauge->valueMode == GAUGE_VALUE_MODE_FILL && logic->maxValue != maxFillPixels)
    {
        if (!logic->valueToPixelsData)
        {
            logic->valueToPixelsData = (u16 *)gauge_alloc_bytes(
                (u16)((logic->maxValue + 1) * (u8)sizeof(u16)));
            if (!logic->valueToPixelsData)
            {
                KLog("Gauge setMaxFillPixels alloc failed");
                return;
            }
        }
        fill_value_to_pixels_lut(logic->valueToPixelsData, logic->maxValue, maxFillPixels);
        logic->valueToPixelsLUT = logic->valueToPixelsData;
    }
    else if (gauge->valueMode == GAUGE_VALUE_MODE_PIP && logic->valueToPixelsData)
    {
        logic->valueToPixelsLUT = logic->valueToPixelsData;
    }
    else
    {
        logic->valueToPixelsLUT = NULL;  /* 1:1 mapping */
    }

    /* Recompute pixel positions */
    logic->valueTargetPixels = value_to_pixels(logic, logic->currentValue);
    if (logic->valueTargetPixels > maxFillPixels)
        logic->valueTargetPixels = maxFillPixels;
    logic->valuePixels = logic->valueTargetPixels;
    logic->trailPixels = logic->valuePixels;

    /* Reset trail and force re-render */
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->lastValuePixels = CACHE_INVALID_U16;
    logic->lastTrailPixelsRendered = CACHE_INVALID_U16;
    logic->lastBlinkOn = CACHE_INVALID_U8;
    logic->lastTrailMode = CACHE_INVALID_U8;
    logic->needUpdate = 1;
}

u8 Gauge_addPart(Gauge *gauge,
                 GaugeLayout *layout,
                 u16 originX,
                 u16 originY)
{
    if (!gauge || !layout)
        return 0;

    const u16 vramSize = compute_vram_size_for_layout(layout,
                                                      gauge->vramMode,
                                                      gauge->logic.trailEnabled,
                                                      gauge->valueMode);
    const u16 vramBase = (u16)(gauge->vramBase + gauge->vramNextOffset);

    if (!Gauge_addPartEx(gauge, layout, originX, originY, vramBase, gauge->vramMode))
        return 0;

    gauge->vramNextOffset = (u16)(gauge->vramNextOffset + vramSize);
    return 1;
}

u8 Gauge_addPartEx(Gauge *gauge,
                   GaugeLayout *layout,
                   u16 originX,
                   u16 originY,
                   u16 vramBase,
                   GaugeVramMode vramMode)
{
    if (!gauge || !layout)
        return 0;

    if (gauge->partCount >= GAUGE_MAX_PARTS)
        return 0;

    if (gauge->valueMode == GAUGE_VALUE_MODE_PIP)
    {
        if (!validate_pip_layout(&gauge->logic, layout))
            return 0;
    }

    if (!ensure_part_capacity(gauge, (u8)(gauge->partCount + 1)))
        return 0;

    GaugePart *part = (GaugePart *)gauge_alloc_bytes((u16)sizeof(GaugePart));
    if (!part)
        return 0;

    if (!GaugePart_initInternal(part, gauge, layout, originX, originY, vramBase, vramMode))
    {
        gauge_free_ptr((void **)&part);
        return 0;
    }

    gauge->parts[gauge->partCount] = part;
    gauge->partCount++;

    /* Force next update to render */
    gauge->logic.lastValuePixels = CACHE_INVALID_U16;
    gauge->logic.needUpdate = 1;

    return 1;
}

void Gauge_release(Gauge *gauge)
{
    if (!gauge)
        return;

    for (u8 i = 0; i < gauge->partCount; i++)
    {
        GaugePart *part = (gauge->parts != NULL) ? gauge->parts[i] : NULL;
        if (!part)
            continue;

        GaugePart_releaseInternal(part);
        gauge_free_ptr((void **)&part);
        gauge->parts[i] = NULL;
    }

    gauge_free_ptr((void **)&gauge->parts);
    gauge_free_ptr((void **)&gauge->logic.valueToPixelsData);
    memset(gauge, 0, sizeof(*gauge));
}

/**
 * FILL-mode update path: tick logic + render all parts.
 * Call once per frame in the game loop.
 *
 * Execution flow:
 * 1. GaugeLogic_tick() - advance value/trail animations
 * 2. Compute render state (valuePixels, trailPixelsRendered, blinkOn)
 * 3. Change detection: compare against lastValuePixels, lastTrailPixelsRendered,
 *    lastBlinkOn, and check if value animation is still converging
 * 4. If nothing changed -> early return (zero CPU cost after initial comparison)
 * 5. If changed -> render all parts via part->renderHandler
 *
 * The change detection cache (lastValuePixels, lastTrailPixelsRendered, lastBlinkOn)
 * is initialized to CACHE_INVALID_U16/U8 to force the first render.
 * Adding a new part also invalidates the cache (Gauge_addPart sets lastValuePixels
 * to CACHE_INVALID_U16).
 *
 * When trail is disabled, trailPixelsRendered == valuePixels naturally
 * (enforced by GaugeLogic_tick), so the blink/trail paths are no-ops.
 *
 * Cost: ~20 cycles (early return) to ~2000+ cycles (full render with DMA)
 */

/* --- Shared tick-and-render helpers (fill & pip) --- */

/** Blink and trail mode state, computed once per tick. */
typedef struct {
    u8 blinkOn;
    u8 blinkOffActive;
    u8 blinkOnChanged;
    u8 trailMode;
    u8 trailModeChanged;
} BlinkState;

/**
 * Compute blink and trail mode state from logic timer state.
 * Shared between gauge_tick_and_render_fill() and gauge_tick_and_render_pip().
 */
static inline BlinkState compute_blink_state(const GaugeLogic *logic)
{
    BlinkState s;
    s.blinkOn = 1;
    if (logic->trailEnabled && logic->blinkFramesRemaining > 0)
    {
        /* Toggle blink on/off at a rate controlled by blinkShift.
         * blinkTimer increments each frame; shifting right by blinkShift
         * divides by 2^blinkShift, so bit 0 of the result toggles every
         * 2^blinkShift frames.  blinkShift=1 -> toggle every 2 frames,
         * blinkShift=2 -> every 4 frames, etc. */
        s.blinkOn = (u8)(((logic->blinkTimer >> logic->blinkShift) & 1) == 0);
    }
    s.blinkOnChanged = (logic->lastBlinkOn != s.blinkOn);
    s.trailMode = logic->trailMode;
    s.trailModeChanged = (logic->lastTrailMode != s.trailMode);
    s.blinkOffActive = (logic->trailEnabled &&
                        logic->blinkFramesRemaining > 0 &&
                        s.blinkOn == 0);
    return s;
}

/**
 * Update render cache, check for early return, and detect idle state.
 *
 * @param valueTargetForEarlyReturn  Quantized target for pip, logic->valueTargetPixels for fill.
 * @param valueForIdle               Raw valuePixels for pip idle detect, valuePixels for fill.
 * @param trailForIdle               Raw trailPixels for pip idle detect, trailPixels for fill.
 * @return 1 if nothing changed (caller should return early), 0 if render needed.
 */
static inline u8 update_render_cache_and_check(
    GaugeLogic *logic,
    u16 valuePixels, u16 trailPixelsRendered,
    const BlinkState *bs,
    u16 valueTargetForEarlyReturn,
    u16 valueForIdle, u16 trailForIdle)
{
    if (logic->lastValuePixels == valuePixels &&
        logic->lastTrailPixelsRendered == trailPixelsRendered &&
        logic->lastBlinkOn == bs->blinkOn &&
        valueTargetForEarlyReturn == valuePixels &&
        !bs->trailModeChanged)
    {
        return 1;
    }

    logic->lastValuePixels = valuePixels;
    logic->lastTrailPixelsRendered = trailPixelsRendered;
    logic->lastBlinkOn = bs->blinkOn;
    logic->lastTrailMode = bs->trailMode;

    /* Detect fully idle state: value at target, trail caught up, no timers.
     * We still render this frame, then next Gauge_update returns via needUpdate. */
    if (valueForIdle == logic->valueTargetPixels &&
        trailForIdle == valueForIdle &&
        logic->holdFramesRemaining == 0 &&
        logic->blinkFramesRemaining == 0 &&
        logic->trailMode == GAUGE_TRAIL_NONE)
    {
        logic->needUpdate = 0;
    }

    return 0;
}

static void gauge_tick_and_render_fill(Gauge *gauge)
{
    GaugeLogic *logic = &gauge->logic;

    /* Skip entirely when gauge is idle (no animations, no pending render).
     * Cost: ~8 cycles (load byte + branch) vs ~70-90 cycles for the full path. */
    if (!logic->needUpdate) return;

    /* Tick logic state machine */
    GaugeLogic_tick(logic);
    
    /* --- Compute current render state --- */
    const u16 valuePixels = logic->valuePixels;

    /* Compute trail with blink effect.
     * trailPixels = actual trail position (clamped to at least valuePixels).
     * trailPixelsRendered = what the render loop uses:
     *   - blinkOn=1 -> show trail normally (trailPixels)
     *   - blinkOn=0 -> hide trail by collapsing it to valuePixels */
    u16 trailPixels = logic->trailPixels;
    if (trailPixels < valuePixels)
        trailPixels = valuePixels;

    const BlinkState bs = compute_blink_state(logic);
    const u16 trailPixelsRendered = bs.blinkOn ? trailPixels : valuePixels;

    if (update_render_cache_and_check(logic, valuePixels, trailPixelsRendered,
                                       &bs, logic->valueTargetPixels,
                                       valuePixels, trailPixels))
        return;

    /* --- Render all parts (countdown: 68000 dbra optimization) --- */
    u8 i = gauge->partCount;
    while (i--)
    {
        GaugePart *part = gauge->parts[i];
        if (!part)
            continue;
        part->renderHandler(part, valuePixels, trailPixelsRendered, trailPixels,
                            bs.blinkOffActive, bs.blinkOnChanged,
                            bs.trailMode, bs.trailModeChanged);
    }
}

/**
 * Quantize a pixel position to the nearest PIP step boundary.
 *
 * Scans the valueToPixelsLUT to find the largest pip boundary <= pixels.
 * Example: LUT = [0, 16, 32, 48, 64], pixels=20 -> returns 16.
 *
 * @param logic   Logic with valueToPixelsLUT and maxValue
 * @param pixels  Raw pixel value to quantize
 * @return Quantized pixel value aligned to a pip boundary
 */
static inline u16 quantize_pixels_to_pip_step(const GaugeLogic *logic, u16 pixels)
{
    if (pixels >= logic->maxFillPixels)
        return logic->maxFillPixels;

    const u16 *lut = logic->valueToPixelsLUT;
    if (!lut)
    {
        /* Fallback (should not happen in strict PIP config): tile-aligned quantization. */
        return (u16)((pixels >> TILE_TO_PIXEL_SHIFT) << TILE_TO_PIXEL_SHIFT);
    }

    u16 value = 0;
    while (value < logic->maxValue && lut[value + 1] <= pixels)
        value++;

    return lut[value];
}

/**
 * PIP-mode update path: tick logic + render all parts.
 *
 * Same flow as fill mode, but pixel values are quantized to pip boundaries
 * before rendering. This ensures each pip snaps to full/empty states rather
 * than showing partial fills. Uses valueToPixelsLUT for quantization.
 */
static void gauge_tick_and_render_pip(Gauge *gauge)
{
    GaugeLogic *logic = &gauge->logic;

    /* Skip entirely when gauge is idle (no animations, no pending render). */
    if (!logic->needUpdate) return;

    /* Tick logic state machine (shared with FILL mode). */
    GaugeLogic_tick(logic);

    /* --- Compute current render state (quantized to pip steps) --- */
    const u16 valuePixelsRaw = logic->valuePixels;
    u16 trailPixelsRaw = logic->trailPixels;
    if (trailPixelsRaw < valuePixelsRaw)
        trailPixelsRaw = valuePixelsRaw;

    const BlinkState bs = compute_blink_state(logic);

    const u16 valuePixels = quantize_pixels_to_pip_step(logic, valuePixelsRaw);
    u16 trailPixels = quantize_pixels_to_pip_step(logic, trailPixelsRaw);
    if (trailPixels < valuePixels)
        trailPixels = valuePixels;

    const u16 valueTargetPixels = quantize_pixels_to_pip_step(logic, logic->valueTargetPixels);
    const u16 trailPixelsRendered = bs.blinkOn ? trailPixels : valuePixels;

    if (update_render_cache_and_check(logic, valuePixels, trailPixelsRendered,
                                       &bs, valueTargetPixels,
                                       valuePixelsRaw, trailPixelsRaw))
        return;

    /* --- Render all parts --- */
    u8 i = gauge->partCount;
    while (i--)
    {
        GaugePart *part = gauge->parts[i];
        if (!part)
            continue;
        part->renderHandler(part, valuePixels, trailPixelsRendered, trailPixels,
                            bs.blinkOffActive, bs.blinkOnChanged,
                            bs.trailMode, bs.trailModeChanged);
    }
}

/** Update gauge: tick logic + render all parts. Call once per frame. */
void Gauge_update(Gauge *gauge)
{
    if (!gauge || !gauge->tickAndRenderHandler)
        return;

    gauge->tickAndRenderHandler(gauge);
}

/** Set gauge value instantly (no trail, no animation). Resets all trail state. */
void Gauge_setValue(Gauge *gauge, u16 newValue)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->needUpdate = 1;

    logic->currentValue = (newValue > logic->maxValue) ? logic->maxValue : newValue;
    logic->valueTargetPixels = value_to_pixels(logic, logic->currentValue);

    if (logic->valueTargetPixels > logic->maxFillPixels)
        logic->valueTargetPixels = logic->maxFillPixels;

    logic->valuePixels = logic->valueTargetPixels;
    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->blinkTimer = 0;
    logic->trailMode = GAUGE_TRAIL_NONE;
}

/**
 * Decrease gauge value (damage).
 *
 * Trail holds at the previous position for holdFrames, then blinks for
 * blinkFrames, then shrinks toward the new value. If valueAnim is enabled,
 * the value itself also animates down.
 */
void Gauge_decrease(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->needUpdate = 1;

    /* Update value */
    logic->currentValue = (logic->currentValue > amount) ? (logic->currentValue - amount) : 0;
    logic->valueTargetPixels = value_to_pixels(logic, logic->currentValue);

    if (logic->valueTargetPixels > logic->maxFillPixels)
        logic->valueTargetPixels = logic->maxFillPixels;

    /* Instant value change if animation disabled */
    if (!logic->valueAnimEnabled)
        logic->valuePixels = logic->valueTargetPixels;

    /* Handle trail */
    if (!logic->trailEnabled)
    {
        logic->trailPixels = logic->valuePixels;
        logic->holdFramesRemaining = 0;
        logic->blinkFramesRemaining = 0;
        logic->trailMode = GAUGE_TRAIL_NONE;
        return;
    }

    /* Trail anchors at the currently displayed value (not the new target).
     * This creates the visual "damage gap" between trail and value.
     * Uses valuePixels (animated position) so the trail doesn't jump ahead
     * if a previous animation is still in progress.
     * If switching from GAIN mode, reset trail from the target to current. */
    const u16 previousDisplayedValuePixels = logic->valuePixels;
    if (logic->trailMode == GAUGE_TRAIL_GAIN)
    {
        logic->trailPixels = previousDisplayedValuePixels;
    }
    else if (logic->trailPixels < previousDisplayedValuePixels)
    {
        logic->trailPixels = previousDisplayedValuePixels;
    }

    /* Start hold/blink sequence */
    logic->holdFramesRemaining = holdFrames;
    logic->blinkFramesRemaining = blinkFrames;
    logic->blinkTimer = 0;
    logic->trailMode = GAUGE_TRAIL_DAMAGE;
}

/**
 * Increase gauge value (heal).
 *
 * If both valueAnim and trail are enabled, triggers a gain trail effect:
 * trail jumps to target, holds, blinks, then value catches up.
 * Otherwise, trail follows value immediately.
 */
void Gauge_increase(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->needUpdate = 1;

    /* Update value (with overflow protection) */
    if (logic->currentValue + amount > logic->maxValue)
        logic->currentValue = logic->maxValue;
    else
        logic->currentValue = (u16)(logic->currentValue + amount);

    logic->valueTargetPixels = value_to_pixels(logic, logic->currentValue);

    if (logic->valueTargetPixels > logic->maxFillPixels)
        logic->valueTargetPixels = logic->maxFillPixels;

    /* Instant value change if animation disabled */
    if (!logic->valueAnimEnabled)
        logic->valuePixels = logic->valueTargetPixels;

    if (logic->trailEnabled && logic->valueAnimEnabled)
    {
        /* Gain trail: trail leads, value catches up after hold/blink */
        logic->trailPixels = logic->valueTargetPixels;
        logic->holdFramesRemaining = holdFrames;
        logic->blinkFramesRemaining = blinkFrames;
        logic->blinkTimer = 0;
        logic->trailMode = GAUGE_TRAIL_GAIN;
    }
    else
    {
        /* Trail follows value immediately on heal */
        logic->trailPixels = logic->valuePixels;
        logic->holdFramesRemaining = 0;
        logic->blinkFramesRemaining = 0;
        logic->blinkTimer = 0;
        logic->trailMode = GAUGE_TRAIL_NONE;
    }
}


/* =============================================================================
   Utility functions
   ============================================================================= */

/**
 * Compute how many VRAM tiles are needed for a layout.
 *
 * VRAM budget depends on mode:
 * - PIP:     1 tile per cell with a compact strip
 * - Fixed:   1 tile per cell with a valid tileset
 * - Dynamic: 3 standard tiles per unique segment (empty/full/fullTrail) +
 *            partial tiles (value/trail/end/trailSecond) + bridge + cap tiles
 *
 * @return Number of VRAM tiles required
 */
static u16 compute_vram_size_for_layout(const GaugeLayout *layout,
                                        GaugeVramMode vramMode,
                                        u8 trailEnabled,
                                        GaugeValueMode valueMode)
{
    if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        /* Compact PIP mode: one tile per visible cell with a configured compact strip. */
        u16 count = 0;
        for (u8 i = 0; i < layout->length; i++)
        {
            const u8 segmentId = layout->segmentIdByCell[i];
            if (layout->pipTilesetBySegment[segmentId] != NULL)
                count++;
        }
        return count;
    }

    if (vramMode == GAUGE_VRAM_FIXED)
    {
        /* Fixed: one VRAM tile per cell with valid tileset */
        u16 count = 0;
        for (u8 i = 0; i < layout->length; i++)
        {
            const u8 segmentId = layout->segmentIdByCell[i];
            if (layout->tilesetBySegment[segmentId] || layout->gainTilesetBySegment[segmentId])
                count++;
        }
        return count;
    }

    /* Dynamic: count unique segments used + partial tiles */
    u8 segmentUsed[GAUGE_MAX_SEGMENTS] = {0};
    u8 segmentCount = 0;
    u8 hasEndTileset = 0;
    u8 bridgeCount = 0;
    u8 capStartEnabled = 0;
    u8 capEndEnabled = 0;

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        if ((layout->tilesetBySegment[segmentId] || layout->gainTilesetBySegment[segmentId]) &&
            !segmentUsed[segmentId])
        {
            segmentUsed[segmentId] = 1;
            segmentCount++;
        }
    }

    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        if (segmentUsed[segmentId] &&
            (layout->tilesetEndBySegment[segmentId] || layout->gainTilesetEndBySegment[segmentId]))
            hasEndTileset = 1;
        if (segmentUsed[segmentId] &&
            (layout->tilesetBridgeBySegment[segmentId] || layout->gainTilesetBridgeBySegment[segmentId]))
            bridgeCount++;
    }

    capStartEnabled = layout->capStartEnabled;
    capEndEnabled = layout->capEndEnabled;

    /* Layout per segment:
     * - 1 empty tile (0,0)
     * - 1 full value tile (8,8)
     * - 1 full trail tile (0,8) - only if trail enabled
     *
     * Partial tiles (scalars - 1 per GaugePart):
     * - 1 partial value tile (also used for "both" case)
     * - 1 partial trail tile (only if trail enabled)
     * - 1 partial END tile (only if any segment has END)
     * - 1 partial trail second-break tile (only if trail enabled + END)
     */
    u16 tilesPerSegment = trailEnabled ? 3 : 2;  /* empty + full value + (full trail if trail) */
    u16 partialTiles = 1;                        /* partial value */
    if (trailEnabled) partialTiles++;
    if (hasEndTileset) partialTiles++;
    if (trailEnabled && hasEndTileset) partialTiles++;
    const u16 capTiles = (u16)(capStartEnabled + capEndEnabled);

    return (u16)(segmentCount * tilesPerSegment + partialTiles + bridgeCount + capTiles);
}

/** Public API: compute VRAM tiles needed for a layout (for manual VRAM allocation). */
u16 Gauge_getVramSize(const Gauge *gauge,
                      const GaugeLayout *layout)
{
    if (!gauge || !layout)
        return 0;

    return compute_vram_size_for_layout(layout,
                                        gauge->vramMode,
                                        gauge->logic.trailEnabled,
                                        gauge->valueMode);
}





