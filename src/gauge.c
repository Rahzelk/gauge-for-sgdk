#include "gauge.h"

/* =============================================================================
   gauge.c - HUD gauge implementation for SGDK (fill + pip, fixed + dynamic)
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
   - logicTickHandler: ~50-100 cycles (mostly conditionals)
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
 * Blink full-cycle frequency: 60fps / (2 * (1 << shift)) Hz
 */
#define GAUGE_DEFAULT_VALUE_ANIM_SHIFT  4  /* Value moves at distance/16 + 1 px per frame */
#define GAUGE_DEFAULT_TRAIL_ANIM_SHIFT  4  /* Trail shrinks at distance/16 + 1 px per frame */
#define GAUGE_DEFAULT_BLINK_SHIFT       3  /* Blink toggles every 8 frames (7.5 toggles/sec @ 60fps) */

/* Gauge runtime configuration states. */
#define GAUGE_RUNTIME_OPEN       0
#define GAUGE_RUNTIME_HAS_LANES  1
#define GAUGE_RUNTIME_CLOSED     2

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
#define PIP_HALF_AXIS_HORIZONTAL 0
#define PIP_HALF_AXIS_VERTICAL   1
#define GAUGE_PIP_MAX_RENDER_TILES (GAUGE_MAX_LENGTH * 4)

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
    GAUGE_TRAIL_STATE_NONE   = 0,
    GAUGE_TRAIL_STATE_DAMAGE = 1,
    GAUGE_TRAIL_STATE_GAIN   = 2
} GaugeActiveTrailState;


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
static inline void compute_fill_for_fill_index(const GaugeLaneLayout *layout,
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
static inline void compute_fill_for_cell(const GaugeLaneLayout *layout,
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
static inline u8 compute_fill_index_and_px(const GaugeLaneLayout *layout,
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
 * For vertical gauges:   Y varies upward from the bottom origin, X stays at origin.
 *
 * @param orient  GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL
 * @param originX Tilemap X origin of the gauge
 * @param originY Tilemap Y origin of the gauge (bottom tile for vertical)
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
        *outY = (u16)(originY - cell);
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

   Multi-lane gauges: if value or trail edges fall outside this layout's
   pixel range [fillOffset .. fillOffset + length*8], the corresponding
   break features are disabled for this lane.
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
static u16 compute_vram_size_for_layout(const GaugeLaneLayout *layout,
                                        GaugeVramMode vramMode,
                                        u8 trailEnabled,
                                        GaugeValueMode valueMode);
static inline u8 select_pip_state_for_cell(const GaugeLaneLayout *layout,
                                           u8 cellIndex,
                                           u16 valuePixels,
                                           u16 trailPixelsRendered,
                                           u16 trailPixelsActual,
                                           u8 blinkOffActive,
                                           u8 trailMode);

/**
 * Compute all break/transition zone information for a gauge lane.
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
static inline void compute_break_info(const GaugeLaneLayout *layout,
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

    /* Detect if break cells belong to this layout (multi-lane gauges) */
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
     * 5) no END in this lane
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
static inline void classify_cap_start(const GaugeLaneLayout *layout,
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
static inline u8 compute_bridge_strip_index(const GaugeLaneLayout *layout,
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
static inline void detect_caps_enabled(const GaugeLaneLayout *layout,
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
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
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
    return (trailMode == GAUGE_TRAIL_STATE_GAIN) ? gainBlinkStrip : normalBlinkStrip;
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
 * Called once per trail mode change to cache the result in GaugeLaneLayout.
 *
 * @param layout    Layout to inspect
 * @param trailMode GAUGE_TRAIL_STATE_DAMAGE or GAUGE_TRAIL_STATE_GAIN
 * @return 1 if any segment has blink-off tilesets, 0 otherwise
 */
static inline u8 layout_has_blink_off_mode(const GaugeLaneLayout *layout, u8 trailMode)
{
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
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


/* =============================================================================
   Unified cell decision types
   =============================================================================

   These types and helpers factor the cell rendering DECISION (which strip and
   tile index to use) out of process_fixed_mode() and process_dynamic_mode().

   The decision is mode-agnostic:
   - Fixed mode:   uses (strip, fillStripIndex) to DMA a tile per cell.
   - Dynamic mode: uses (type) to route to the correct shared VRAM slot,
                   then (strip, fillStripIndex) to DMA partial tiles.

   This eliminates ~320 lines of duplicated classification logic.
   ============================================================================= */

/**
 * Break zone cell type -- classifies a cell within the break zone.
 *
 * Used by compute_cell_decision() to determine which tileset and index to use.
 *
 *   |FULL|VALUE_BREAK|TRAIL_FULL|TRAIL_BREAK|TRAIL_BREAK2|END|EMPTY|
 *    <-- value -->|<---------- trail ---------->|
 */
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
 * Cell decision type -- encodes which VRAM slot dynamic mode should use.
 * Fixed mode ignores this (uniform output for all types).
 */
typedef enum
{
    CELL_DECISION_CAP_END,          /* Cap end tile (dedicated VRAM per lane) */
    CELL_DECISION_CAP_START,        /* Cap start tile (dedicated VRAM per lane) */
    CELL_DECISION_BRIDGE,           /* Bridge tile (dedicated VRAM per segment) */
    CELL_DECISION_STANDARD_FULL,    /* Fully filled body tile (pre-loaded per segment) */
    CELL_DECISION_STANDARD_EMPTY,   /* Empty body tile (pre-loaded per segment) */
    CELL_DECISION_STANDARD_TRAIL,   /* Full trail tile (pre-loaded per segment) */
    CELL_DECISION_PARTIAL_END,      /* Partial END tile (streamed on demand) */
    CELL_DECISION_PARTIAL_TRAIL,    /* Partial trail tile (streamed on demand) */
    CELL_DECISION_PARTIAL_TRAIL2,   /* Second trail break tile (streamed on demand) */
    CELL_DECISION_PARTIAL_VALUE     /* Partial value tile (streamed on demand) */
} CellDecisionType;

/**
 * Cell rendering decision result.
 *
 * Produced by compute_cell_decision() for each cell.
 * Contains all information needed by both fixed and dynamic renderers.
 */
typedef struct
{
    const u32 *strip;           /* Selected ROM strip (NULL = skip rendering) */
    u8 fillStripIndex;          /* Tile index within the strip (0..44 or 0..63) */
    CellDecisionType type;      /* VRAM slot type for dynamic mode routing */
    u8 capStartUsesBreak;       /* 1 if cap start uses break variant (for dyn cache) */
    u8 capStartUsesTrail;       /* 1 if cap start uses trail variant (for dyn cache) */
    u8 useBlinkVariant;         /* 1 when decision strip comes from blink-off variant */
} CellDecision;

/* Debug trace helpers (used by gauge_tick_and_render_fill/pip). */
#if GAUGE_ENABLE_TRACE
typedef struct
{
    u8 active;
    u8 laneIndex;
} GaugeTraceContext;

typedef struct
{
    u8 used;
    u8 segmentId;
    CellDecisionType baseLaneType;
    u8 baseLaneIdx;
    u8 terminalOverrideApplied;
    CellDecision decision;
} GaugeTraceCell;

static GaugeTraceContext s_traceContext = {0, 0};

static const char *decision_type_to_text(CellDecisionType type)
{
    switch (type)
    {
    case CELL_DECISION_CAP_END: return "CAP_END";
    case CELL_DECISION_CAP_START: return "CAP_START";
    case CELL_DECISION_BRIDGE: return "BRIDGE";
    case CELL_DECISION_STANDARD_FULL: return "STANDARD_FULL";
    case CELL_DECISION_STANDARD_EMPTY: return "STANDARD_EMPTY";
    case CELL_DECISION_STANDARD_TRAIL: return "STANDARD_TRAIL";
    case CELL_DECISION_PARTIAL_END: return "PARTIAL_END";
    case CELL_DECISION_PARTIAL_TRAIL: return "PARTIAL_TRAIL";
    case CELL_DECISION_PARTIAL_TRAIL2: return "PARTIAL_TRAIL2";
    case CELL_DECISION_PARTIAL_VALUE: return "PARTIAL_VALUE";
    default: return "UNKNOWN";
    }
}

static const char *decision_class_to_text(CellDecisionType type,
                                          u8 capStartUsesBreak,
                                          u8 capStartUsesTrail)
{
    switch (type)
    {
    case CELL_DECISION_CAP_END:
        return "CAP_END";
    case CELL_DECISION_CAP_START:
        if (capStartUsesTrail)
            return "CAP_START_TRAIL";
        if (capStartUsesBreak)
            return "CAP_START_BREAK";
        return "CAP_START";
    case CELL_DECISION_BRIDGE:
        return "BRIDGE";
    case CELL_DECISION_PARTIAL_END:
        return "END";
    case CELL_DECISION_STANDARD_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL2:
        return "TRAIL";
    case CELL_DECISION_STANDARD_FULL:
    case CELL_DECISION_STANDARD_EMPTY:
    case CELL_DECISION_PARTIAL_VALUE:
    default:
        return "BODY";
    }
}

static const char *trail_mode_to_text(u8 trailMode)
{
    switch (trailMode)
    {
    case GAUGE_TRAIL_STATE_DAMAGE: return "DAMAGE";
    case GAUGE_TRAIL_STATE_GAIN: return "GAIN";
    case GAUGE_TRAIL_STATE_NONE:
    default:
        return "NONE";
    }
}

static const char *pip_state_to_text(u8 pipState)
{
    switch (pipState)
    {
    case PIP_STATE_VALUE: return "VALUE";
    case PIP_STATE_LOSS: return "LOSS";
    case PIP_STATE_GAIN: return "GAIN";
    case PIP_STATE_BLINK_OFF: return "BLINK_OFF";
    case PIP_STATE_EMPTY:
    default:
        return "EMPTY";
    }
}

static const char *render_state_to_text(u8 trailMode,
                                        u8 blinkOffActive,
                                        u16 valuePixels,
                                        u16 trailPixelsRendered)
{
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
        return "GAIN";
    if (blinkOffActive)
        return "BLINK_OFF";
    if (trailPixelsRendered > valuePixels)
        return "TRAIL";
    return "NORMAL";
}

static void trace_reset_context(void)
{
    s_traceContext.active = 0;
    s_traceContext.laneIndex = 0;
}

static void trace_set_lane_index(u8 laneIndex)
{
    s_traceContext.laneIndex = laneIndex;
}

static void trace_emit_frame_line(u16 valuePixels,
                                  u16 trailPixelsRendered,
                                  u16 trailPixelsActual,
                                  u8 trailMode,
                                  u8 blinkOffActive)
{
    if (!s_traceContext.active)
        return;

    kprintf(
        "GAUGE_TRACE FRAME valuePixels=%u trailRendered=%u trailActual=%u trailMode=%s state=%s\n",
        (unsigned int)valuePixels,
        (unsigned int)trailPixelsRendered,
        (unsigned int)trailPixelsActual,
        trail_mode_to_text(trailMode),
        render_state_to_text(trailMode, blinkOffActive, valuePixels, trailPixelsRendered)
    );
}

static void trace_emit_cell_line(u8 cellIndex,
                                 u8 segmentId,
                                 CellDecisionType baseLaneType,
                                 u8 baseLaneIdx,
                                 u8 terminalOverrideApplied,
                                 const CellDecision *decision)
{
    if (!s_traceContext.active || !decision)
        return;

    kprintf(
        "GAUGE_TRACE CELL lane=%u cell=%u seg=%u class=%s type=%s strip=0x%08lX idx=%u ovr=%s baseLaneType=%s baseLaneIdx=%u\n",
        (unsigned int)s_traceContext.laneIndex,
        (unsigned int)cellIndex,
        (unsigned int)segmentId,
        decision_class_to_text(decision->type,
                               decision->capStartUsesBreak,
                               decision->capStartUsesTrail),
        decision_type_to_text(decision->type),
        (unsigned long)(u32)decision->strip,
        (unsigned int)decision->fillStripIndex,
        terminalOverrideApplied ? "TERM_END" : "NONE",
        decision_type_to_text(baseLaneType),
        (unsigned int)baseLaneIdx
    );
}

static void trace_emit_pip_cell_line(u8 cellIndex,
                                     u8 segmentId,
                                     u8 renderIndex,
                                     u8 dynamicMode,
                                     const u32 *pipStrip,
                                     u8 stripIndex,
                                     u16 tileIndex,
                                     u8 requestedState,
                                     u8 resolvedState,
                                     u8 stateCount,
                                     u8 changed)
{
    if (!s_traceContext.active)
        return;

    kprintf(
        "GAUGE_TRACE CELL lane=%u cell=%u seg=%u class=PIP type=%s strip=0x%08lX idx=%u req=%s res=%s tile=%u render=%u changed=%u states=%u\n",
        (unsigned int)s_traceContext.laneIndex,
        (unsigned int)cellIndex,
        (unsigned int)segmentId,
        dynamicMode ? "DYNAMIC" : "FIXED",
        (unsigned long)(u32)pipStrip,
        (unsigned int)stripIndex,
        pip_state_to_text(requestedState),
        pip_state_to_text(resolvedState),
        (unsigned int)tileIndex,
        (unsigned int)renderIndex,
        (unsigned int)changed,
        (unsigned int)stateCount
    );
}

static void trace_begin_frame(const Gauge *gauge,
                              u16 valuePixels,
                              u16 trailPixelsRendered,
                              u16 trailPixelsActual,
                              u8 trailMode,
                              u8 blinkOffActive)
{
    trace_reset_context();

    if (!gauge || !gauge->debugMode)
        return;

    s_traceContext.active = 1;
    trace_emit_frame_line(valuePixels, trailPixelsRendered, trailPixelsActual,
                          trailMode, blinkOffActive);
}

static void trace_end_frame(void)
{
    trace_reset_context();
}

static void trace_clear_recorded_cells(GaugeTraceCell *traceByCell, u8 cellCount)
{
    for (u8 cellIndex = 0; cellIndex < cellCount; cellIndex++)
        traceByCell[cellIndex].used = 0;
}

static void trace_record_cell(GaugeTraceCell *traceByCell,
                              u8 cellIndex,
                              u8 segmentId,
                              CellDecisionType baseLaneType,
                              u8 baseLaneIdx,
                              u8 terminalOverrideApplied,
                              const CellDecision *decision)
{
    if (!traceByCell || !decision || cellIndex >= GAUGE_MAX_LENGTH)
        return;

    traceByCell[cellIndex].used = 1;
    traceByCell[cellIndex].segmentId = segmentId;
    traceByCell[cellIndex].baseLaneType = baseLaneType;
    traceByCell[cellIndex].baseLaneIdx = baseLaneIdx;
    traceByCell[cellIndex].terminalOverrideApplied = terminalOverrideApplied;
    traceByCell[cellIndex].decision = *decision;
}

static void trace_emit_recorded_cells(const GaugeLaneLayout *layout,
                                      const GaugeTraceCell *traceByCell)
{
    if (!layout || !traceByCell)
        return;

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        if (!traceByCell[cellIndex].used)
            continue;

        trace_emit_cell_line(cellIndex,
                             traceByCell[cellIndex].segmentId,
                             traceByCell[cellIndex].baseLaneType,
                             traceByCell[cellIndex].baseLaneIdx,
                             traceByCell[cellIndex].terminalOverrideApplied,
                             &traceByCell[cellIndex].decision);
    }
}
#endif

static Gauge *s_activeGaugeForRender = NULL;

/**
 * Fill loop context -- loop-invariant state shared between fixed and dynamic.
 *
 * Computed once before the cell loop by init_fill_loop_context().
 * Contains break zone info, cap detection, and rendering parameters.
 */
typedef struct
{
    GaugeBreakInfo breakInfo;         /* Break zone with rendered trail (blink applied) */
    GaugeBreakInfo breakInfoActual;   /* Break zone with actual trail (for bridges) */
    u16 valuePixels;
    u16 trailPixelsRendered;
    u16 trailPixelsActual;
    u8 breakLow;                      /* Early-exit lower bound (fill index) */
    u8 breakHigh;                     /* Early-exit upper bound (fill index) */
    u8 capStartCellIndex;
    u8 capEndCellIndex;
    u8 capStartSegId;
    u8 capEndSegId;
    u8 capStartEnabled;               /* Dynamic mode may override to 0 if VRAM absent */
    u8 capEndEnabled;                 /* Dynamic mode may override to 0 if VRAM absent */
    u8 blinkOffActive;
    u8 trailMode;
} FillLoopContext;


/**
 * Initialize fill loop context with loop-invariant state.
 *
 * Computes break zones, early-exit boundaries, and cap detection.
 * Called once before the cell loop in both fixed and dynamic modes.
 *
 * @param layout              Layout configuration
 * @param valuePixels         Current value fill in pixels
 * @param trailPixelsRendered Trail pixels after blink
 * @param trailPixelsActual   Trail pixels before blink (for bridges)
 * @param blinkOffActive      1 if blink-off rendering is active
 * @param trailMode           GAUGE_TRAIL_STATE_DAMAGE/GAIN/NONE
 * @param ctx                 [out] Initialized loop context
 */
static inline void init_fill_loop_context(const GaugeLaneLayout *layout,
                                          u16 valuePixels,
                                          u16 trailPixelsRendered,
                                          u16 trailPixelsActual,
                                          u8 blinkOffActive,
                                          u8 trailMode,
                                          FillLoopContext *ctx)
{
    ctx->valuePixels = valuePixels;
    ctx->trailPixelsRendered = trailPixelsRendered;
    ctx->trailPixelsActual = trailPixelsActual;
    ctx->blinkOffActive = blinkOffActive;
    ctx->trailMode = trailMode;

    /* Break zone computation:
     * breakInfo:       uses rendered trail (blink applied) for normal classification
     * breakInfoActual: uses actual trail (ignoring blink) for bridge visibility */
    compute_break_info(layout, valuePixels, trailPixelsRendered, trailMode,
                       &ctx->breakInfo);
    ctx->breakInfoActual = ctx->breakInfo;
    if (trailPixelsActual != trailPixelsRendered)
        compute_break_info(layout, valuePixels, trailPixelsActual, trailMode,
                           &ctx->breakInfoActual);

    /* Early-exit boundaries: cells outside [breakLow..breakHigh] are trivially
     * full or empty (~70-80% of cells). Skips full classification. */
    ctx->breakLow = (ctx->breakInfo.valueBreakFillIndex != CACHE_INVALID_U8)
                   ? ctx->breakInfo.valueBreakFillIndex
                   : ctx->breakInfo.valueFillIndex;
    ctx->breakHigh = ctx->breakInfo.trailFillIndex;

    /* Cap detection */
    ctx->capStartCellIndex = layout->cellIndexByFillIndex[0];
    ctx->capEndCellIndex = layout->cellIndexByFillIndex[layout->length - 1];
    ctx->capStartSegId = layout->segmentIdByCell[ctx->capStartCellIndex];
    ctx->capEndSegId = layout->segmentIdByCell[ctx->capEndCellIndex];
    ctx->capStartEnabled = layout->capStartEnabled;
    ctx->capEndEnabled = layout->capEndEnabled;
}


/**
 * Compute the rendering decision for a single cell.
 *
 * This is the unified decision function shared by process_fixed_mode() and
 * process_dynamic_mode(). It determines:
 * - Which ROM strip to use (body, end, trail, bridge, cap, blink-off variant)
 * - Which tile index within that strip
 * - Which cell type (for dynamic mode VRAM slot routing)
 *
 * The caller provides bodyStrip as a parameter because its source differs
 * between modes:
 * - Fixed:   layout->tilesetBySegment[segmentId] (per-cell cache removed)
 * - Dynamic: layout->tilesetBySegment[segmentId]
 *
 * All other strips (end, trail, bridge, caps, blink variants) are looked up
 * from the layout inside this function.
 *
 * @param layout           Layout configuration
 * @param ctx              Loop-invariant context (from init_fill_loop_context)
 * @param cellIndex        Cell index in layout
 * @param cellFillIndex    Fill index of the cell (from fillIndexByCell)
 * @param segmentId        Segment ID of the cell
 * @param bodyStrip        Pre-computed body strip (mode-specific source)
 * @param out              [out] Rendering decision
 */
static inline void compute_cell_decision(const GaugeLaneLayout *layout,
                                         const FillLoopContext *ctx,
                                         u8 cellIndex,
                                         u8 cellFillIndex,
                                         u8 segmentId,
                                         const u32 *bodyStrip,
                                         CellDecision *out)
{
    out->capStartUsesBreak = 0;
    out->capStartUsesTrail = 0;
    out->useBlinkVariant = 0;

    /* Blink-off body strip lookup is only needed when blink-off is active. */
    const u32 *blinkOffBodyStrip = NULL;
    if (ctx->blinkOffActive)
    {
        blinkOffBodyStrip = select_blink_strip(
            layout->blinkOffTilesetBySegment[segmentId],
            layout->gainBlinkOffTilesetBySegment[segmentId],
            ctx->trailMode);
    }

    /* === Cap end (always uses its dedicated tileset) === */
    if (cellIndex == ctx->capEndCellIndex &&
        ctx->capEndEnabled &&
        segmentId == ctx->capEndSegId)
    {
        const u32 *capEndStrip = select_base_strip(
            layout->tilesetCapEndBySegment[segmentId],
            layout->gainTilesetCapEndBySegment[segmentId],
            ctx->trailMode);
        u16 capTrailPixels = ctx->trailPixelsRendered;

        const u32 *capEndBlinkStrip = select_blink_strip(
            layout->blinkOffTilesetCapEndBySegment[segmentId],
            layout->gainBlinkOffTilesetCapEndBySegment[segmentId],
            ctx->trailMode);

        u8 capUsesBlink = 0;
        if (ctx->blinkOffActive &&
            capEndBlinkStrip &&
            is_cell_in_trail_zone(&ctx->breakInfoActual, cellFillIndex))
        {
            capEndStrip = capEndBlinkStrip;
            capTrailPixels = ctx->trailPixelsActual;
            capUsesBlink = 1;
        }

        u8 capValuePx = 0;
        u8 capTrailPx = 0;
        compute_fill_for_cell(layout, cellIndex, ctx->valuePixels, capTrailPixels,
                              &capValuePx, &capTrailPx);

        out->type = CELL_DECISION_CAP_END;
        out->strip = capEndStrip;
        out->fillStripIndex = s_tileIndexByValueTrail[capValuePx][capTrailPx];
        out->useBlinkVariant = capUsesBlink;
        return;
    }

    /* === Cap start (fixed border at cell 0) === */
    if (cellIndex == ctx->capStartCellIndex &&
        ctx->capStartEnabled &&
        segmentId == ctx->capStartSegId)
    {
        const u32 *capStartStrip = select_base_strip(
            layout->tilesetCapStartBySegment[segmentId],
            layout->gainTilesetCapStartBySegment[segmentId],
            ctx->trailMode);

        if (capStartStrip)
        {
            const u32 *capStartBreakStrip = select_base_strip(
                layout->tilesetCapStartBreakBySegment[segmentId],
                layout->gainTilesetCapStartBreakBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartTrailStrip = select_base_strip(
                layout->tilesetCapStartTrailBySegment[segmentId],
                layout->gainTilesetCapStartTrailBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartBlinkStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartBlinkBreakStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBreakBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBreakBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartBlinkTrailStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartTrailBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartTrailBySegment[segmentId],
                ctx->trailMode);

            CapStartResult capResult;
            u8 capUsesBlink = 0;
            if (ctx->blinkOffActive)
            {
                CapStartResult capActual;
                classify_cap_start_with_strips(&ctx->breakInfoActual,
                                               cellFillIndex,
                                               capStartStrip,
                                               capStartBreakStrip,
                                               capStartTrailStrip,
                                               &capActual);

                const u8 inTrailZone = is_cell_in_trail_zone(&ctx->breakInfoActual,
                                                              cellFillIndex);
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
                    capUsesBlink = 1;
                }
                else
                {
                    classify_cap_start_with_strips(&ctx->breakInfo,
                                                   cellFillIndex,
                                                   capStartStrip,
                                                   capStartBreakStrip,
                                                   capStartTrailStrip,
                                                   &capResult);
                }
            }
            else
            {
                classify_cap_start_with_strips(&ctx->breakInfo,
                                               cellFillIndex,
                                               capStartStrip,
                                               capStartBreakStrip,
                                               capStartTrailStrip,
                                               &capResult);
            }

            out->type = CELL_DECISION_CAP_START;
            out->strip = capResult.strip;
            out->fillStripIndex = capResult.fillStripIndex;
            out->capStartUsesBreak = capResult.usesBreak;
            out->capStartUsesTrail = capResult.usesTrail;
            out->useBlinkVariant = capUsesBlink;
            return;
        }
        /* capStartStrip == NULL: fall through to normal cell handling */
    }

    /* === Bridge/BREAK forced rendering (D/B before gauge END) ===
     * Bridge cells sit at segment boundaries. When the gauge's value/trail
     * edge is past this cell, the bridge shows a transition tile.
     * breakInfoActual uses trailPixelsActual (ignoring blink) so bridges don't
     * flicker during blink phase. */
    if (ctx->breakInfoActual.endFillIndex != CACHE_INVALID_U8)
    {
        /* Bridge end cell: last cell of a segment with a bridge tileset */
        if (layout->bridgeEndByFillIndex[cellFillIndex] &&
            ctx->breakInfoActual.endFillIndex > cellFillIndex &&
            ctx->breakInfoActual.valueFillIndex > cellFillIndex)
        {
            const u8 nextFillIndex = (u8)(cellFillIndex + 1);
            const u8 bridgeInTrailZone = is_cell_in_trail_zone(
                &ctx->breakInfoActual, nextFillIndex);

            const u32 *bridgeStrip = select_base_strip(
                layout->tilesetBridgeBySegment[segmentId],
                layout->gainTilesetBridgeBySegment[segmentId],
                ctx->trailMode);

            if (bridgeStrip)
            {
                const u32 *blinkOffBridgeStrip = select_blink_strip(
                    layout->blinkOffTilesetBridgeBySegment[segmentId],
                    layout->gainBlinkOffTilesetBridgeBySegment[segmentId],
                    ctx->trailMode);

                const u8 useBlinkOffBridge = (ctx->blinkOffActive &&
                                              blinkOffBridgeStrip &&
                                              bridgeInTrailZone);

                const GaugeBreakInfo *bridgeBreakInfo = useBlinkOffBridge
                    ? &ctx->breakInfoActual : &ctx->breakInfo;
                const u16 bridgeTrailPixels = useBlinkOffBridge
                    ? ctx->trailPixelsActual : ctx->trailPixelsRendered;
                const u32 *bridgeStripUse = useBlinkOffBridge
                    ? blinkOffBridgeStrip : bridgeStrip;
                const u8 bridgeIdx = compute_bridge_strip_index(
                    layout, bridgeBreakInfo, cellFillIndex,
                    ctx->valuePixels, bridgeTrailPixels);

                out->type = CELL_DECISION_BRIDGE;
                out->strip = bridgeStripUse;
                out->fillStripIndex = bridgeIdx;
                out->useBlinkVariant = useBlinkOffBridge;
            }
            else
            {
                /* No bridge tileset: fall back to body strip, fully filled.
                 * Fixed mode renders this; dynamic mode skips via vramTileBridge==0. */
                const u32 *fallbackStrip = bodyStrip;
                if (ctx->blinkOffActive && blinkOffBodyStrip && bridgeInTrailZone)
                    fallbackStrip = blinkOffBodyStrip;

                out->type = CELL_DECISION_BRIDGE;
                out->strip = fallbackStrip;
                out->fillStripIndex = STRIP_INDEX_FULL;
                out->useBlinkVariant = (fallbackStrip == blinkOffBodyStrip) ? 1 : 0;
            }
            return;
        }

        /* Forced BREAK cell: cell before a bridge that must show as fully filled */
        if (layout->bridgeBreakByFillIndex[cellFillIndex])
        {
            const u8 boundaryFillIndex = layout->bridgeBreakBoundaryByFillIndex[cellFillIndex];
            if (ctx->breakInfoActual.endFillIndex > boundaryFillIndex &&
                ctx->breakInfoActual.valueFillIndex > boundaryFillIndex)
            {
                const u32 *breakStripUse = bodyStrip;
                if (ctx->blinkOffActive && blinkOffBodyStrip &&
                    is_cell_in_trail_zone(&ctx->breakInfoActual, cellFillIndex))
                {
                    breakStripUse = blinkOffBodyStrip;
                }
                out->type = CELL_DECISION_STANDARD_FULL;
                out->strip = breakStripUse;
                out->fillStripIndex = STRIP_INDEX_FULL;
                out->useBlinkVariant = (breakStripUse == blinkOffBodyStrip) ? 1 : 0;
                return;
            }
        }
    }

    /* === Early-exit: trivial cells (FULL or EMPTY) ===
     * ~70-80% of cells hit this path, skipping the full classification below.
     * Blink-off needs per-cell classification for mixed segments. */
    if (!ctx->blinkOffActive)
    {
        if (cellFillIndex < ctx->breakLow)
        {
            out->type = CELL_DECISION_STANDARD_FULL;
            out->strip = bodyStrip;
            out->fillStripIndex = STRIP_INDEX_FULL;
            return;
        }
        if (cellFillIndex > ctx->breakHigh)
        {
            out->type = CELL_DECISION_STANDARD_EMPTY;
            out->strip = bodyStrip;
            out->fillStripIndex = STRIP_INDEX_EMPTY;
            return;
        }
    }

    /* === Break zone: full classification needed ===
     * Lazy strip computation: only break-zone cells (~20-30%) need these.
     * bodyStrip is provided by the caller; blink-off body strip is selected here. */
    const u32 *endStrip = select_base_strip(
        layout->tilesetEndBySegment[segmentId],
        layout->gainTilesetEndBySegment[segmentId],
        ctx->trailMode);
    const u32 *trailStrip = select_base_strip(
        layout->tilesetTrailBySegment[segmentId],
        layout->gainTilesetTrailBySegment[segmentId],
        ctx->trailMode);
    const u32 *blinkOffEndStrip = select_blink_strip(
        layout->blinkOffTilesetEndBySegment[segmentId],
        layout->gainBlinkOffTilesetEndBySegment[segmentId],
        ctx->trailMode);
    const u32 *blinkOffTrailStrip = select_blink_strip(
        layout->blinkOffTilesetTrailBySegment[segmentId],
        layout->gainBlinkOffTilesetTrailBySegment[segmentId],
        ctx->trailMode);

    /* Mutable copies for blink-off override */
    const u32 *bodyStripUse = bodyStrip;
    const u32 *endStripUse = endStrip;
    const u32 *trailStripUse = trailStrip;

    const GaugeBreakInfo *breakInfoUsed = &ctx->breakInfo;
    u16 trailPixelsUse = ctx->trailPixelsRendered;
    u8 useBlinkOff = 0;

    if (ctx->blinkOffActive)
    {
        useBlinkOff = apply_blink_off_overrides(
            &ctx->breakInfoActual, cellFillIndex,
            blinkOffEndStrip, blinkOffTrailStrip, blinkOffBodyStrip,
            &endStripUse, &trailStripUse, &bodyStripUse);
        if (useBlinkOff)
        {
            breakInfoUsed = &ctx->breakInfoActual;
            trailPixelsUse = ctx->trailPixelsActual;
        }
    }

    /* Trail strip with body fallback (defensive, matches dynamic mode) */
    const u32 *trailStripFinal = trailStripUse ? trailStripUse : bodyStripUse;
    /* NOTE: full-trail index depends on the strip type:
     * - TRAIL strip uses s_trailTileIndexByValueTrail[0][8] (index 7)
     * - BODY strip uses STRIP_INDEX_FULL_TRAIL (index 8)
     * This keeps FIXED and DYNAMIC behavior consistent when no trail strip exists. */
    const u8 fullTrailIndex = (trailStripUse != NULL)
        ? s_trailTileIndexByValueTrail[0][8]
        : STRIP_INDEX_FULL_TRAIL;

    const CellBreakType cellType = classify_break_zone_cell(
        breakInfoUsed, cellFillIndex,
        (endStripUse != NULL), (trailStripUse != NULL));

    switch (cellType)
    {
    case CELL_TYPE_END:
        out->type = CELL_DECISION_PARTIAL_END;
        out->strip = endStripUse;
        out->fillStripIndex = s_tileIndexByValueTrail
            [breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_TRAIL_BREAK:
        out->type = CELL_DECISION_PARTIAL_TRAIL;
        out->strip = trailStripFinal;
        out->fillStripIndex = breakInfoUsed->trailBreakSecondActive
            ? s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell][8]
            : s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell]
                                          [breakInfoUsed->trailPxInBreakCell];
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_TRAIL_BREAK2:
    {
        out->type = CELL_DECISION_PARTIAL_TRAIL2;
        out->strip = trailStripFinal;
        const u8 endIndex = s_tileIndexByValueTrail
            [breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
        out->fillStripIndex = (endIndex == 8)
            ? s_trailTileIndexByValueTrail[0][endIndex]
            : endIndex;
        out->useBlinkVariant = useBlinkOff;
        return;
    }

    case CELL_TYPE_TRAIL_FULL:
        out->type = CELL_DECISION_STANDARD_TRAIL;
        out->strip = trailStripFinal;
        out->fillStripIndex = fullTrailIndex;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_VALUE_BREAK:
        out->type = CELL_DECISION_PARTIAL_VALUE;
        out->strip = bodyStripUse;
        out->fillStripIndex = breakInfoUsed->trailBreakActive
            ? breakInfoUsed->valuePxInBreakCell
            : s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile]
                                     [breakInfoUsed->endTrailPxInTile];
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_REGION_FULL:
        out->type = CELL_DECISION_STANDARD_FULL;
        out->strip = bodyStripUse;
        out->fillStripIndex = STRIP_INDEX_FULL;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_REGION_EMPTY:
        out->type = CELL_DECISION_STANDARD_EMPTY;
        out->strip = bodyStripUse;
        out->fillStripIndex = STRIP_INDEX_EMPTY;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_REGION_TRAIL:
        out->type = CELL_DECISION_STANDARD_TRAIL;
        out->strip = trailStripFinal;
        out->fillStripIndex = fullTrailIndex;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_DEFAULT:
    {
        /* Sub-routing by cell position relative to value/trail edges.
         * Standard cells use shortcuts; partial cells need fill computation. */
        const u32 *defaultStrip = bodyStripUse;

        /* endStrip override: if this cell is the END cell, use endStrip */
        if (breakInfoUsed->endFillIndex != CACHE_INVALID_U8 &&
            cellFillIndex == breakInfoUsed->endFillIndex &&
            endStripUse)
        {
            defaultStrip = endStripUse;
        }

        if (cellFillIndex < breakInfoUsed->valueFillIndex)
        {
            out->type = CELL_DECISION_STANDARD_FULL;
            out->strip = defaultStrip;
            out->fillStripIndex = STRIP_INDEX_FULL;
            out->useBlinkVariant = useBlinkOff;
        }
        else if (cellFillIndex > breakInfoUsed->valueFillIndex &&
                 cellFillIndex < breakInfoUsed->trailFillIndex)
        {
            out->type = CELL_DECISION_STANDARD_TRAIL;
            out->strip = trailStripFinal;
            out->fillStripIndex = fullTrailIndex;
            out->useBlinkVariant = useBlinkOff;
        }
        else if (cellFillIndex > breakInfoUsed->trailFillIndex)
        {
            out->type = CELL_DECISION_STANDARD_EMPTY;
            out->strip = defaultStrip;
            out->fillStripIndex = STRIP_INDEX_EMPTY;
            out->useBlinkVariant = useBlinkOff;
        }
        else
        {
            /* Partial cell: compute exact pixel fill */
            u8 valuePxInTile = 0;
            u8 trailPxInTile = 0;
            compute_fill_for_cell(layout, cellIndex,
                                  ctx->valuePixels, trailPixelsUse,
                                  &valuePxInTile, &trailPxInTile);

            if (cellFillIndex == breakInfoUsed->valueFillIndex &&
                cellFillIndex == breakInfoUsed->trailFillIndex)
            {
                /* Both value and trail edges in same cell */
                out->type = CELL_DECISION_PARTIAL_VALUE;
                out->strip = defaultStrip;
                out->fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
                out->useBlinkVariant = useBlinkOff;
            }
            else if (cellFillIndex == breakInfoUsed->valueFillIndex)
            {
                /* Value edge cell (trail is full = 8) */
                out->type = CELL_DECISION_PARTIAL_VALUE;
                out->strip = defaultStrip;
                out->fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
                out->useBlinkVariant = useBlinkOff;
            }
            else
            {
                /* Trail edge cell (value is 0) */
                out->type = CELL_DECISION_PARTIAL_TRAIL;
                out->strip = defaultStrip;
                out->fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
                out->useBlinkVariant = useBlinkOff;
            }
        }
        return;
    }
    }

    /* Fallback (should never be reached) */
    out->type = CELL_DECISION_STANDARD_EMPTY;
    out->strip = bodyStripUse;
    out->fillStripIndex = STRIP_INDEX_EMPTY;
    out->useBlinkVariant = useBlinkOff;
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

/* Layout lazy-allocation helpers (defined later in GaugeLaneLayout section). */
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
                                                          u8 segmentCount);
static u8 layout_sync_optional_segment_flags_by_usage(u8 hasAny,
                                                       u8 **segmentFlags,
                                                       u8 segmentCount,
                                                       const u8 *defaultView,
                                                       u8 defaultValue);
static void layout_copy_segment_tilesets(const u32 **destinationTilesets,
                                         const u32 * const *sourceTilesets,
                                         u8 segmentCount);

/* GaugeLaneLayout helpers used only by the module build/runtime plumbing. */
void GaugeLaneLayout_initEx(GaugeLaneLayout *layout,
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
                        u8 horizontalFlip);
void GaugeLaneLayout_setFillOffset(GaugeLaneLayout *layout, u16 fillOffsetPixels);
void GaugeLaneLayout_setFillForward(GaugeLaneLayout *layout);
void GaugeLaneLayout_setFillReverse(GaugeLaneLayout *layout);
void GaugeLaneLayout_retain(GaugeLaneLayout *layout);
void GaugeLaneLayout_release(GaugeLaneLayout *layout);

/* Internal gauge-lane builders used by Gauge_build(). */
u8 Gauge_addLaneEx(Gauge *gauge,
                   GaugeLaneLayout *layout,
                   u16 originX,
                   u16 originY,
                   u16 vramBase,
                   GaugeVramMode vramMode);

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
 * Called by GaugeLaneLayout_setFillForward/Reverse after fill order is set.
 */
static void build_bridge_luts(GaugeLaneLayout *layout)
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

/**
 * Build PIP lookup tables.
 *
 * PIP boundaries are defined by contiguous runs of segment IDs in fill order.
 * For a given segment style, one logical pip spans pipWidthBySegment[segmentId] cells.
 * A run can therefore contain multiple pips with the same style.
 *
 * Also builds a physical render LUT for multi-height PIP:
 * renderIndex -> (fillIndex,row), where row is [0..segmentHeight-1].
 */
static inline void compute_pip_source_mapping(u8 stripCoverage,
                                              u8 halfAxis,
                                              u8 pipWidth,
                                              u8 pipHeight,
                                              u8 sourceWidth,
                                              u8 sourceHeight,
                                              u8 localCol,
                                              u8 localRow,
                                              u8 *outSourceCol,
                                              u8 *outSourceRow,
                                              u8 *outExtraHFlip,
                                              u8 *outExtraVFlip)
{
    u8 sourceCol = localCol;
    u8 sourceRow = localRow;
    u8 extraHFlip = 0;
    u8 extraVFlip = 0;

    if (pipWidth == 0)
        pipWidth = 1;
    if (pipHeight == 0)
        pipHeight = 1;
    if (sourceWidth == 0)
        sourceWidth = 1;
    if (sourceHeight == 0)
        sourceHeight = 1;
    if (localCol >= pipWidth)
        localCol = 0;
    if (localRow >= pipHeight)
        localRow = 0;

    if (stripCoverage == GAUGE_STRIP_COVERAGE_HALF)
    {
        if (halfAxis == PIP_HALF_AXIS_VERTICAL)
        {
            /* Right side mirrored from left side via HFLIP. */
            if (localCol < sourceWidth)
            {
                sourceCol = localCol;
            }
            else
            {
                sourceCol = (u8)(pipWidth - 1 - localCol);
                extraHFlip = 1;
            }
            sourceRow = localRow;
        }
        else
        {
            /* Bottom side mirrored from top side via VFLIP. */
            sourceCol = localCol;
            if (localRow < sourceHeight)
            {
                sourceRow = localRow;
            }
            else
            {
                sourceRow = (u8)(pipHeight - 1 - localRow);
                extraVFlip = 1;
            }
        }
    }
    else if (stripCoverage == GAUGE_STRIP_COVERAGE_QUARTER)
    {
        /* Reconstruct right half with HFLIP and bottom half with VFLIP. */
        if (localCol < sourceWidth)
        {
            sourceCol = localCol;
        }
        else
        {
            sourceCol = (u8)(pipWidth - 1 - localCol);
            extraHFlip = 1;
        }

        if (localRow < sourceHeight)
        {
            sourceRow = localRow;
        }
        else
        {
            sourceRow = (u8)(pipHeight - 1 - localRow);
            extraVFlip = 1;
        }
    }
    else
    {
        /* FULL coverage: direct lookup. */
        sourceCol = localCol;
        sourceRow = localRow;
    }

    if (sourceCol >= sourceWidth)
        sourceCol = 0;
    if (sourceRow >= sourceHeight)
        sourceRow = 0;

    *outSourceCol = sourceCol;
    *outSourceRow = sourceRow;
    *outExtraHFlip = extraHFlip;
    *outExtraVFlip = extraVFlip;
}

static void build_pip_luts(GaugeLaneLayout *layout)
{
    const u8 hasPipStyles =
        segment_tileset_array_has_any(layout->pipTilesetBySegment, layout->segmentCount);
    u16 computedRenderCount = (u16)layout->length * 4u;
    if (computedRenderCount > GAUGE_PIP_MAX_RENDER_TILES)
        computedRenderCount = GAUGE_PIP_MAX_RENDER_TILES;
    const u8 maxRenderCount = (u8)computedRenderCount;

    if (!hasPipStyles)
    {
        layout->pipCount = 0;
        layout->pipRenderCount = 0;
        layout_free_optional_ptr((void **)&layout->pipIndexByFillIndex, s_invalidCellIndexes, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipLocalTileByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipWidthByPipIndex, s_oneCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderFillIndexByRenderIndex, s_invalidCellIndexes, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderRowByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderSourceColByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderSourceRowByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderExtraHFlipByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderExtraVFlipByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout->pipIndexByFillIndex = (u8 *)s_invalidCellIndexes;
        layout->pipLocalTileByFillIndex = (u8 *)s_zeroCellFlags;
        layout->pipWidthByPipIndex = (u8 *)s_oneCellFlags;
        layout->pipRenderFillIndexByRenderIndex = (u8 *)s_invalidCellIndexes;
        layout->pipRenderRowByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderSourceColByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderSourceRowByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderExtraHFlipByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderExtraVFlipByRenderIndex = (u8 *)s_zeroCellFlags;
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
                                         1) ||
        !layout_ensure_cell_flag_storage(&layout->pipRenderFillIndexByRenderIndex,
                                         maxRenderCount,
                                         s_invalidCellIndexes,
                                         CACHE_INVALID_U8) ||
        !layout_ensure_cell_flag_storage(&layout->pipRenderRowByRenderIndex,
                                         maxRenderCount,
                                         s_zeroCellFlags,
                                         0) ||
        !layout_ensure_cell_flag_storage(&layout->pipRenderSourceColByRenderIndex,
                                         maxRenderCount,
                                         s_zeroCellFlags,
                                         0) ||
        !layout_ensure_cell_flag_storage(&layout->pipRenderSourceRowByRenderIndex,
                                         maxRenderCount,
                                         s_zeroCellFlags,
                                         0) ||
        !layout_ensure_cell_flag_storage(&layout->pipRenderExtraHFlipByRenderIndex,
                                         maxRenderCount,
                                         s_zeroCellFlags,
                                         0) ||
        !layout_ensure_cell_flag_storage(&layout->pipRenderExtraVFlipByRenderIndex,
                                         maxRenderCount,
                                         s_zeroCellFlags,
                                         0))
    {
        layout->pipCount = 0;
        layout->pipRenderCount = 0;
        layout_free_optional_ptr((void **)&layout->pipIndexByFillIndex, s_invalidCellIndexes, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipLocalTileByFillIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipWidthByPipIndex, s_oneCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderFillIndexByRenderIndex, s_invalidCellIndexes, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderRowByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderSourceColByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderSourceRowByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderExtraHFlipByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout_free_optional_ptr((void **)&layout->pipRenderExtraVFlipByRenderIndex, s_zeroCellFlags, NULL, NULL);
        layout->pipIndexByFillIndex = (u8 *)s_invalidCellIndexes;
        layout->pipLocalTileByFillIndex = (u8 *)s_zeroCellFlags;
        layout->pipWidthByPipIndex = (u8 *)s_oneCellFlags;
        layout->pipRenderFillIndexByRenderIndex = (u8 *)s_invalidCellIndexes;
        layout->pipRenderRowByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderSourceColByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderSourceRowByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderExtraHFlipByRenderIndex = (u8 *)s_zeroCellFlags;
        layout->pipRenderExtraVFlipByRenderIndex = (u8 *)s_zeroCellFlags;
        return;
    }

    layout->pipCount = 0;
    layout->pipRenderCount = 0;

    for (u8 i = 0; i < layout->length; i++)
    {
        layout->pipIndexByFillIndex[i] = CACHE_INVALID_U8;
        layout->pipLocalTileByFillIndex[i] = 0;
        layout->pipWidthByPipIndex[i] = 1;
    }
    for (u8 i = 0; i < maxRenderCount; i++)
    {
        layout->pipRenderFillIndexByRenderIndex[i] = CACHE_INVALID_U8;
        layout->pipRenderRowByRenderIndex[i] = 0;
        layout->pipRenderSourceColByRenderIndex[i] = 0;
        layout->pipRenderSourceRowByRenderIndex[i] = 0;
        layout->pipRenderExtraHFlipByRenderIndex[i] = 0;
        layout->pipRenderExtraVFlipByRenderIndex[i] = 0;
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

    /* Build physical render LUT: each fillIndex is duplicated by segment height. */
    for (u8 i = 0; i < layout->length && layout->pipRenderCount < maxRenderCount; i++)
    {
        const u8 fillCellIndex = layout->cellIndexByFillIndex[i];
        const u8 segmentId = layout->segmentIdByCell[fillCellIndex];
        const u8 pipIndex = layout->pipIndexByFillIndex[i];
        u8 pipWidth = 1;
        if (pipIndex != CACHE_INVALID_U8)
            pipWidth = layout->pipWidthByPipIndex[pipIndex];
        if (pipWidth == 0)
            pipWidth = 1;

        u8 pipHeight = layout->pipHeightBySegment[segmentId];
        if (pipHeight == 0)
            pipHeight = 1;
        if (pipHeight > 4)
            pipHeight = 4;

        u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
        if (sourceWidth == 0)
            sourceWidth = pipWidth;
        u8 sourceHeight = layout->pipSourceHeightBySegment[segmentId];
        if (sourceHeight == 0)
            sourceHeight = pipHeight;

        const u8 stripCoverage = layout->pipStripCoverageBySegment[segmentId];
        const u8 halfAxis = layout->pipHalfAxisBySegment[segmentId];
        u8 localTile = layout->pipLocalTileByFillIndex[i];
        if (localTile >= pipWidth)
            localTile = 0;

        for (u8 row = 0; row < pipHeight && layout->pipRenderCount < maxRenderCount; row++)
        {
            u8 sourceCol = localTile;
            u8 sourceRow = row;
            u8 extraHFlip = 0;
            u8 extraVFlip = 0;
            compute_pip_source_mapping(stripCoverage,
                                       halfAxis,
                                       pipWidth,
                                       pipHeight,
                                       sourceWidth,
                                       sourceHeight,
                                       localTile,
                                       row,
                                       &sourceCol,
                                       &sourceRow,
                                       &extraHFlip,
                                       &extraVFlip);

            const u8 renderIndex = layout->pipRenderCount;
            layout->pipRenderFillIndexByRenderIndex[renderIndex] = i;
            layout->pipRenderRowByRenderIndex[renderIndex] = row;
            layout->pipRenderSourceColByRenderIndex[renderIndex] = sourceCol;
            layout->pipRenderSourceRowByRenderIndex[renderIndex] = sourceRow;
            layout->pipRenderExtraHFlipByRenderIndex[renderIndex] = extraHFlip;
            layout->pipRenderExtraVFlipByRenderIndex[renderIndex] = extraVFlip;
            layout->pipRenderCount++;
        }
    }

}


/* =============================================================================
   GaugeLaneLayout implementation
   ============================================================================= */

static void layout_zero_optional_flags(GaugeLaneLayout *layout)
{
    layout->hasBlinkOff = 0;
    layout->hasGainBlinkOff = 0;
    layout->capStartEnabled = 0;
    layout->capEndEnabled = 0;
}

static void layout_set_optional_views_to_defaults(GaugeLaneLayout *layout)
{
    /* Optional fields point to shared sentinel views until explicitly enabled.
     * This keeps zero-feature layouts RAM-light while preserving branch-free reads. */
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
    layout->pipHeightBySegment = (u8 *)s_oneSegmentFlags;
    layout->pipOffsetBySegment = (u8 *)s_zeroSegmentFlags;
    layout->pipStateCountBySegment = (u8 *)s_zeroSegmentFlags;
    layout->pipStripCoverageBySegment = (u8 *)s_zeroSegmentFlags;
    layout->pipHalfAxisBySegment = (u8 *)s_zeroSegmentFlags;
    layout->pipSourceWidthBySegment = (u8 *)s_oneSegmentFlags;
    layout->pipSourceHeightBySegment = (u8 *)s_oneSegmentFlags;

    layout->pipIndexByFillIndex = (u8 *)s_invalidCellIndexes;
    layout->pipLocalTileByFillIndex = (u8 *)s_zeroCellFlags;
    layout->pipWidthByPipIndex = (u8 *)s_oneCellFlags;
    layout->pipRenderCount = 0;
    layout->pipRenderFillIndexByRenderIndex = (u8 *)s_invalidCellIndexes;
    layout->pipRenderRowByRenderIndex = (u8 *)s_zeroCellFlags;
    layout->pipRenderSourceColByRenderIndex = (u8 *)s_zeroCellFlags;
    layout->pipRenderSourceRowByRenderIndex = (u8 *)s_zeroCellFlags;
    layout->pipRenderExtraHFlipByRenderIndex = (u8 *)s_zeroCellFlags;
    layout->pipRenderExtraVFlipByRenderIndex = (u8 *)s_zeroCellFlags;

    layout->bridgeEndByFillIndex = (u8 *)s_zeroCellFlags;
    layout->bridgeBreakByFillIndex = (u8 *)s_zeroCellFlags;
    layout->bridgeBreakBoundaryByFillIndex = (u8 *)s_zeroCellFlags;
}

static void layout_free_optional_ptr(void **ptr,
                                     const void *defaultViewA,
                                     const void *defaultViewB,
                                     const void *defaultViewC)
{
    /* Free only real heap-backed buffers. Shared sentinel views are never freed. */
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
                                                          u8 segmentCount)
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
        *segmentTilesetsBySegment = (const u32 **)s_nullSegmentTilesets;
        return 0;
    }

    return 1;
}

static u8 layout_sync_optional_segment_flags_by_usage(u8 hasAny,
                                                       u8 **segmentFlags,
                                                       u8 segmentCount,
                                                       const u8 *defaultView,
                                                       u8 defaultValue)
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

typedef enum
{
    LAYOUT_TILESET_SLOT_BODY = 0,
    LAYOUT_TILESET_SLOT_END,
    LAYOUT_TILESET_SLOT_TRAIL,
    LAYOUT_TILESET_SLOT_BRIDGE,
    LAYOUT_TILESET_SLOT_CAP_START,
    LAYOUT_TILESET_SLOT_CAP_END,
    LAYOUT_TILESET_SLOT_CAP_START_BREAK,
    LAYOUT_TILESET_SLOT_CAP_START_TRAIL,
    LAYOUT_TILESET_SLOT_COUNT
} LayoutTilesetSlot;

typedef enum
{
    LAYOUT_STYLE_CONTEXT_BASE = 0,
    LAYOUT_STYLE_CONTEXT_GAIN,
    LAYOUT_STYLE_CONTEXT_BLINK,
    LAYOUT_STYLE_CONTEXT_GAIN_BLINK
} LayoutStyleContext;

typedef u16 LayoutTilesetSlotMask;

#define LAYOUT_TILESET_SLOT_MASK(slot) ((LayoutTilesetSlotMask)(1u << (slot)))
#define LAYOUT_TILESET_SLOT_MASK_ALL ((LayoutTilesetSlotMask)((1u << LAYOUT_TILESET_SLOT_COUNT) - 1u))

typedef struct
{
    const u32 ***targetBySlot[LAYOUT_TILESET_SLOT_COUNT];
} LayoutContextTargetView;

static void build_layout_context_target_view(GaugeLaneLayout *layout,
                                             LayoutStyleContext context,
                                             LayoutContextTargetView *outView)
{
    if (!layout || !outView)
        return;

    memset(outView, 0, sizeof(*outView));

    switch (context)
    {
    case LAYOUT_STYLE_CONTEXT_BASE:
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BODY] = &layout->tilesetBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_END] = &layout->tilesetEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_TRAIL] = &layout->tilesetTrailBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BRIDGE] = &layout->tilesetBridgeBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START] = &layout->tilesetCapStartBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_END] = &layout->tilesetCapEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_BREAK] = &layout->tilesetCapStartBreakBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_TRAIL] = &layout->tilesetCapStartTrailBySegment;
        break;

    case LAYOUT_STYLE_CONTEXT_GAIN:
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BODY] = &layout->gainTilesetBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_END] = &layout->gainTilesetEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_TRAIL] = &layout->gainTilesetTrailBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BRIDGE] = &layout->gainTilesetBridgeBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START] = &layout->gainTilesetCapStartBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_END] = &layout->gainTilesetCapEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_BREAK] = &layout->gainTilesetCapStartBreakBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_TRAIL] = &layout->gainTilesetCapStartTrailBySegment;
        break;

    case LAYOUT_STYLE_CONTEXT_BLINK:
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BODY] = &layout->blinkOffTilesetBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_END] = &layout->blinkOffTilesetEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_TRAIL] = &layout->blinkOffTilesetTrailBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BRIDGE] = &layout->blinkOffTilesetBridgeBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START] = &layout->blinkOffTilesetCapStartBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_END] = &layout->blinkOffTilesetCapEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_BREAK] = &layout->blinkOffTilesetCapStartBreakBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_TRAIL] = &layout->blinkOffTilesetCapStartTrailBySegment;
        break;

    case LAYOUT_STYLE_CONTEXT_GAIN_BLINK:
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BODY] = &layout->gainBlinkOffTilesetBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_END] = &layout->gainBlinkOffTilesetEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_TRAIL] = &layout->gainBlinkOffTilesetTrailBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_BRIDGE] = &layout->gainBlinkOffTilesetBridgeBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START] = &layout->gainBlinkOffTilesetCapStartBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_END] = &layout->gainBlinkOffTilesetCapEndBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_BREAK] = &layout->gainBlinkOffTilesetCapStartBreakBySegment;
        outView->targetBySlot[LAYOUT_TILESET_SLOT_CAP_START_TRAIL] = &layout->gainBlinkOffTilesetCapStartTrailBySegment;
        break;
    }
}

static void layout_free_buffers(GaugeLaneLayout *layout)
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
    layout_free_optional_ptr((void **)&layout->pipHeightBySegment, s_oneSegmentFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipOffsetBySegment, s_zeroSegmentFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipStateCountBySegment, s_zeroSegmentFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipStripCoverageBySegment, s_zeroSegmentFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipHalfAxisBySegment, s_zeroSegmentFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipSourceWidthBySegment, s_oneSegmentFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipSourceHeightBySegment, s_oneSegmentFlags, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->pipIndexByFillIndex, s_invalidCellIndexes, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipLocalTileByFillIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipWidthByPipIndex, s_oneCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipRenderFillIndexByRenderIndex, s_invalidCellIndexes, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipRenderRowByRenderIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipRenderSourceColByRenderIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipRenderSourceRowByRenderIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipRenderExtraHFlipByRenderIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->pipRenderExtraVFlipByRenderIndex, s_zeroCellFlags, NULL, NULL);

    layout_free_optional_ptr((void **)&layout->bridgeEndByFillIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->bridgeBreakByFillIndex, s_zeroCellFlags, NULL, NULL);
    layout_free_optional_ptr((void **)&layout->bridgeBreakBoundaryByFillIndex, s_zeroCellFlags, NULL, NULL);

    layout->length = 0;
    layout->segmentCount = 0;
    layout->pipCount = 0;
    layout->pipRenderCount = 0;
    layout_set_optional_views_to_defaults(layout);
    layout_zero_optional_flags(layout);
}

static u8 layout_alloc_buffers(GaugeLaneLayout *layout, u8 length, u8 segmentCount)
{
    const u16 cellBytes = (u16)length;
    const u16 segPtrBytes = (u16)(segmentCount * (u8)sizeof(const u32 *));

    /* Mandatory geometry and BODY tileset bindings only.
     * Optional features are allocated lazily by the layout build path. */
    layout->segmentIdByCell = (u8 *)gauge_alloc_bytes(cellBytes);
    layout->fillIndexByCell = (u8 *)gauge_alloc_bytes(cellBytes);
    layout->cellIndexByFillIndex = (u8 *)gauge_alloc_bytes(cellBytes);
    layout->tilemapPosByCell = (Vect2D_u16 *)gauge_alloc_bytes((u16)(length * (u8)sizeof(Vect2D_u16)));

    layout->tilesetBySegment = (const u32 **)gauge_alloc_bytes(segPtrBytes);

    if (!layout->segmentIdByCell || !layout->fillIndexByCell || !layout->cellIndexByFillIndex ||
        !layout->tilemapPosByCell || !layout->tilesetBySegment)
    {
        layout_free_buffers(layout);
        return 0;
    }

    layout->length = length;
    layout->segmentCount = segmentCount;
    layout->pipCount = 0;
    layout->pipRenderCount = 0;
    layout_set_optional_views_to_defaults(layout);

    return 1;
}

void GaugeLaneLayout_initEx(GaugeLaneLayout *layout,
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
        return;
    layout_free_buffers(layout);
    if (!layout_alloc_buffers(layout, length, segmentCount))
        return;

    layout->fillOffset = 0;
    layout->endOverrideEnabled = 1;
    layout->refCount = 0;

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(endTilesets, layout->segmentCount),
        &layout->tilesetEndBySegment,
        layout->segmentCount);

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(trailTilesets, layout->segmentCount),
        &layout->tilesetTrailBySegment,
        layout->segmentCount);

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(bridgeTilesets, layout->segmentCount),
        &layout->tilesetBridgeBySegment,
        layout->segmentCount);

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
        GaugeLaneLayout_setFillReverse(layout);
    else
        GaugeLaneLayout_setFillForward(layout);

    /* Set visual properties */
    layout->orientation = orientation;
    layout->palette = palette;
    layout->priority = priority ? 1 : 0;
    layout->verticalFlip = verticalFlip ? 1 : 0;
    layout->horizontalFlip = horizontalFlip ? 1 : 0;

    /* Direct layout builders refresh these cached flags after optional tilesets are wired. */
    layout_zero_optional_flags(layout);
}

/** Set fill direction to forward: cell 0 fills first (left-to-right / bottom-to-top). */
void GaugeLaneLayout_setFillForward(GaugeLaneLayout *layout)
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

/** Set pixel offset for fill computation (used for multi-lane gauge windows). */
void GaugeLaneLayout_setFillOffset(GaugeLaneLayout *layout, u16 fillOffsetPixels)
{
    if (!layout)
        return;

    layout->fillOffset = fillOffsetPixels;
}

/** Set fill direction to reverse: last cell fills first (right-to-left / top-to-bottom). */
void GaugeLaneLayout_setFillReverse(GaugeLaneLayout *layout)
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

void GaugeLaneLayout_retain(GaugeLaneLayout *layout)
{
    if (!layout)
        return;
    layout->refCount++;
}

void GaugeLaneLayout_release(GaugeLaneLayout *layout)
{
    if (!layout)
        return;

    if (layout->refCount > 0)
        layout->refCount--;

    if (layout->refCount == 0)
        layout_free_buffers(layout);
}

/* =============================================================================
   Direct layout build helpers
   ============================================================================= */

/**
 * Usage flags for one tileset context (base/gain/blinkOff/gainBlinkOff).
 * Each flag is 1 if any segment provides that tileset, 0 otherwise.
 */
typedef struct
{
    u8 body;
    u8 end;
    u8 trail;
    u8 bridge;
    u8 capStart;
    u8 capEnd;
    u8 capStartBreak;
    u8 capStartTrail;
} SkinSetUsageFlags;

static inline u8 usage_flag_for_slot(const SkinSetUsageFlags *usageFlags,
                                     LayoutTilesetSlot slot)
{
    if (!usageFlags)
        return 0;

    switch (slot)
    {
    case LAYOUT_TILESET_SLOT_BODY:
        return usageFlags->body;
    case LAYOUT_TILESET_SLOT_END:
        return usageFlags->end;
    case LAYOUT_TILESET_SLOT_TRAIL:
        return usageFlags->trail;
    case LAYOUT_TILESET_SLOT_BRIDGE:
        return usageFlags->bridge;
    case LAYOUT_TILESET_SLOT_CAP_START:
        return usageFlags->capStart;
    case LAYOUT_TILESET_SLOT_CAP_END:
        return usageFlags->capEnd;
    case LAYOUT_TILESET_SLOT_CAP_START_BREAK:
        return usageFlags->capStartBreak;
    case LAYOUT_TILESET_SLOT_CAP_START_TRAIL:
        return usageFlags->capStartTrail;
    default:
        return 0;
    }
}

static void sync_layout_context_slots_from_usage(GaugeLaneLayout *layout,
                                                 LayoutStyleContext context,
                                                 const SkinSetUsageFlags *usageFlags,
                                                 LayoutTilesetSlotMask slotMask)
{
    if (!layout || !usageFlags)
        return;

    LayoutContextTargetView targetView;
    build_layout_context_target_view(layout, context, &targetView);

    for (u8 slot = 0; slot < LAYOUT_TILESET_SLOT_COUNT; slot++)
    {
        const LayoutTilesetSlot slotId = (LayoutTilesetSlot)slot;
        if ((slotMask & LAYOUT_TILESET_SLOT_MASK(slotId)) == 0)
            continue;

        if (context == LAYOUT_STYLE_CONTEXT_BASE && slotId == LAYOUT_TILESET_SLOT_BODY)
            continue;

        const u32 ***targetField = targetView.targetBySlot[slotId];
        if (!targetField)
            continue;

        layout_sync_optional_segment_tilesets_by_usage(
            usage_flag_for_slot(usageFlags, slotId),
            targetField,
            layout->segmentCount);
    }
}

/**
 * Scan helper: OR-combine a SkinSet's non-NULL pointers into usage flags.
 */
/**
 * Sync base tileset allocations (end/trail/bridge/caps + capEnd flags).
 *
 * base.body is always required and handled by GaugeLaneLayout_initEx, so
 * only the 7 optional tilesets + capEnd flag array are synced here.
 */
static void sync_base_allocations(GaugeLaneLayout *layout,
                                  const SkinSetUsageFlags *f,
                                  u8 hasCapEndFlags)
{
    const u8 sc = layout->segmentCount;

    sync_layout_context_slots_from_usage(layout,
                                         LAYOUT_STYLE_CONTEXT_BASE,
                                         f,
                                         LAYOUT_TILESET_SLOT_MASK_ALL);

    layout_sync_optional_segment_flags_by_usage(hasCapEndFlags,
        &layout->capEndBySegment, sc, s_zeroSegmentFlags, 0);
}

/**
 * Populate base tilesets from segment styles into layout arrays.
 *
 * body is always assigned. Optional arrays are only written when allocated
 * (checked via != s_nullSegmentTilesets / s_zeroSegmentFlags sentinel).
 */
/**
 * Compute all derived layout state after tilesets are populated.
 *
 * Caches blink-off presence flags, detects cap configuration,
 * builds bridge LUTs and pip LUTs.
 */
static void finalize_layout_derived_state(GaugeLaneLayout *layout)
{
    layout->hasBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_STATE_DAMAGE);
    layout->hasGainBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_STATE_GAIN);
    detect_caps_enabled(layout, &layout->capStartEnabled, &layout->capEndEnabled);
    build_bridge_luts(layout);
    build_pip_luts(layout);
}


/* =============================================================================
   GaugeLogic implementation (internal -- not exposed in gauge.h)
   ============================================================================= */

/* Forward declaration: GaugeLogic_init calls GaugeLogic_initWithAnim */
static void GaugeLogic_initWithAnim(GaugeLogic *logic,
                                    u16 maxValue,
                                    u16 maxFillPixels,
                                    const u16 *valueToPixelsLUT,
                                    u16 initialValue,
                                    u8 valueAnimEnabled,
                                    u8 valueAnimShift,
                                    u8 trailAnimShift,
                                    u8 blinkShift);
static void GaugeLogic_tick_follow_mode(GaugeLogic *logic);
static void GaugeLogic_tick_non_follow_mode(GaugeLogic *logic);
static GaugeLogicTickHandler *resolve_logic_tick_handler(GaugeTrailMode mode);

static void GaugeLogic_init(GaugeLogic *logic,
                     u16 maxValue,
                     u16 maxFillPixels,
                     const u16 *valueToPixelsLUT,
                     u16 initialValue)
{
    GaugeLogic_initWithAnim(logic, maxValue, maxFillPixels, valueToPixelsLUT,
                            initialValue,
                            0,  /* valueAnimEnabled = instant */
                            GAUGE_DEFAULT_VALUE_ANIM_SHIFT,
                            GAUGE_DEFAULT_TRAIL_ANIM_SHIFT,
                            GAUGE_DEFAULT_BLINK_SHIFT);
}

static void GaugeLogic_initWithAnim(GaugeLogic *logic,
                                    u16 maxValue,
                                    u16 maxFillPixels,
                                    const u16 *valueToPixelsLUT,
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
    logic->gainAnimShift = (valueAnimShift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : valueAnimShift;
    logic->gainBlinkShift = (blinkShift == 0) ? GAUGE_DEFAULT_BLINK_SHIFT : blinkShift;

    logic->trailEnabled = 0;
    logic->configuredTrailMode = GAUGE_TRAIL_MODE_DISABLED;
    logic->configuredGainMode = GAUGE_GAIN_MODE_DISABLED;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    logic->lastActiveTrailState = GAUGE_TRAIL_STATE_NONE;
    logic->criticalValue = 0;
    logic->modeUsesCriticalThreshold = 0;
    logic->modeUsesValueBlink = 0;
    logic->modeKeepsStaticTrail = 0;

    /* Initialize render cache to force first render */
    logic->lastValuePixels = CACHE_INVALID_U16;
    logic->lastTrailPixelsRendered = CACHE_INVALID_U16;
    logic->lastBlinkOn = CACHE_INVALID_U8;
    logic->needUpdate = 1;

    /* Disabled by default; configured later via Gauge_setTrailMode in OPEN state. */
    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
}

/* Trail mode helpers (configuration/runtime). */
static inline u8 is_critical_mode_active(const GaugeLogic *logic)
{
    return (logic->currentValue <= logic->criticalValue) ? 1 : 0;
}

static inline void set_mode_flags(GaugeLogic *logic, GaugeTrailMode mode)
{
    logic->modeUsesCriticalThreshold =
        (mode == GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK ||
         mode == GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK ||
         mode == GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK) ? 1 : 0;
    logic->modeUsesValueBlink =
        (mode == GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK) ? 1 : 0;
    logic->modeKeepsStaticTrail =
        (mode == GAUGE_TRAIL_MODE_STATIC_TRAIL ||
         mode == GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK) ? 1 : 0;
}

static inline void apply_non_follow_trail_mode_state(GaugeLogic *logic, u8 advanceBlinkTimer)
{
    const u8 criticalModeActive =
        logic->modeUsesCriticalThreshold ? is_critical_mode_active(logic) : 0;

    logic->holdFramesRemaining = 0;

    switch ((GaugeTrailMode)logic->configuredTrailMode)
    {
        case GAUGE_TRAIL_MODE_DISABLED:
            logic->trailPixels = logic->valuePixels;
            logic->blinkFramesRemaining = 0;
            logic->blinkTimer = 0;
            logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
            break;

        case GAUGE_TRAIL_MODE_STATIC_TRAIL:
            logic->trailPixels = logic->maxFillPixels;
            logic->blinkFramesRemaining = 0;
            logic->blinkTimer = 0;
            logic->activeTrailState = (logic->trailPixels > logic->valuePixels)
                ? GAUGE_TRAIL_STATE_DAMAGE
                : GAUGE_TRAIL_STATE_NONE;
            break;

        case GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK:
            logic->trailPixels = logic->maxFillPixels;
            logic->activeTrailState = (logic->trailPixels > logic->valuePixels)
                ? GAUGE_TRAIL_STATE_DAMAGE
                : GAUGE_TRAIL_STATE_NONE;
            if (criticalModeActive)
            {
                logic->blinkFramesRemaining = 1; /* continuous blink gate */
                if (advanceBlinkTimer)
                    logic->blinkTimer++;
                else
                    logic->blinkTimer = 0;
            }
            else
            {
                logic->blinkFramesRemaining = 0;
                logic->blinkTimer = 0;
            }
            break;

        case GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK:
            if (criticalModeActive)
            {
                logic->trailPixels = logic->maxFillPixels;
                logic->blinkFramesRemaining = 1; /* continuous blink gate */
                if (advanceBlinkTimer)
                    logic->blinkTimer++;
                else
                    logic->blinkTimer = 0;
                logic->activeTrailState = (logic->trailPixels > logic->valuePixels)
                    ? GAUGE_TRAIL_STATE_DAMAGE
                    : GAUGE_TRAIL_STATE_NONE;
            }
            else
            {
                logic->trailPixels = logic->valuePixels;
                logic->blinkFramesRemaining = 0;
                logic->blinkTimer = 0;
                logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
            }
            break;

        case GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK:
            logic->trailPixels = logic->valuePixels;
            logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
            if (criticalModeActive)
            {
                logic->blinkFramesRemaining = 1; /* continuous blink gate */
                if (advanceBlinkTimer)
                    logic->blinkTimer++;
                else
                    logic->blinkTimer = 0;
            }
            else
            {
                logic->blinkFramesRemaining = 0;
                logic->blinkTimer = 0;
            }
            break;

        case GAUGE_TRAIL_MODE_FOLLOW:
        default:
            break;
    }
}

static inline void tick_value_animation(GaugeLogic *logic, u8 freezeIncreasing)
{
    const u8 valueAnimShift =
        (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN &&
         logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW)
        ? logic->gainAnimShift
        : logic->valueAnimShift;

    if (!logic->valueAnimEnabled || logic->valuePixels == logic->valueTargetPixels)
        return;

    if (logic->valuePixels < logic->valueTargetPixels)
    {
        if (freezeIncreasing)
            return;

        const u16 diff = (u16)(logic->valueTargetPixels - logic->valuePixels);
        const u16 step = CALC_ANIM_STEP(diff, valueAnimShift);
        logic->valuePixels = (u16)(logic->valuePixels + step);

        if (logic->valuePixels > logic->valueTargetPixels)
            logic->valuePixels = logic->valueTargetPixels;
        if (logic->valuePixels > logic->maxFillPixels)
            logic->valuePixels = logic->maxFillPixels;
    }
    else
    {
        const u16 diff = (u16)(logic->valuePixels - logic->valueTargetPixels);
        const u16 step = CALC_ANIM_STEP(diff, valueAnimShift);
        s16 result = (s16)logic->valuePixels - (s16)step;
        logic->valuePixels = (u16)(result & ~(result >> 15));

        if (logic->valuePixels < logic->valueTargetPixels)
            logic->valuePixels = logic->valueTargetPixels;
    }
}

static inline u8 tick_gain_follow_state(GaugeLogic *logic)
{
    if (logic->configuredGainMode != GAUGE_GAIN_MODE_FOLLOW ||
        logic->activeTrailState != GAUGE_TRAIL_STATE_GAIN)
    {
        return 0;
    }

    logic->trailPixels = logic->valueTargetPixels;

    /* Hold phase: trail stays at target. */
    if (logic->holdFramesRemaining > 0)
    {
        logic->holdFramesRemaining--;
        return 1;
    }

    /* Blink phase: trail stays at target. */
    if (logic->blinkFramesRemaining > 0)
    {
        logic->blinkTimer++;
        logic->blinkFramesRemaining--;
        if (logic->blinkFramesRemaining == 0)
            logic->blinkTimer = 0;
        return 1;
    }

    /* Wait until animated value catches the gain trail. */
    if (logic->valuePixels < logic->valueTargetPixels)
        return 1;

    logic->trailPixels = logic->valuePixels;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    return 0;
}

static void GaugeLogic_tick_non_follow_mode(GaugeLogic *logic)
{
    const u8 gainMode = (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN);
    const u8 freezeIncreasing =
        (gainMode && (logic->holdFramesRemaining > 0 || logic->blinkFramesRemaining > 0)) ? 1 : 0;

    tick_value_animation(logic, freezeIncreasing);

    if (tick_gain_follow_state(logic))
        return;

    apply_non_follow_trail_mode_state(logic, 1);
}

static void GaugeLogic_tick_follow_mode(GaugeLogic *logic)
{
    const u8 gainMode = (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN);
    const u8 freezeIncreasing =
        (gainMode && (logic->holdFramesRemaining > 0 || logic->blinkFramesRemaining > 0)) ? 1 : 0;

    tick_value_animation(logic, freezeIncreasing);

    if (gainMode && tick_gain_follow_state(logic))
        return;

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
        if (logic->activeTrailState == GAUGE_TRAIL_STATE_DAMAGE)
            logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    }
}

static GaugeLogicTickHandler *resolve_logic_tick_handler(GaugeTrailMode mode)
{
    if (mode == GAUGE_TRAIL_MODE_FOLLOW)
        return GaugeLogic_tick_follow_mode;
    return GaugeLogic_tick_non_follow_mode;
}

static inline u8 trail_mode_enables_trail(GaugeTrailMode mode)
{
    return (mode != GAUGE_TRAIL_MODE_DISABLED) ? 1 : 0;
}

static inline u8 gain_mode_uses_trail(GaugeGainMode mode)
{
    return (mode == GAUGE_GAIN_MODE_FOLLOW) ? 1 : 0;
}

static inline u8 compute_trail_enabled(GaugeTrailMode trailMode, GaugeGainMode gainMode)
{
    return (trail_mode_enables_trail(trailMode) || gain_mode_uses_trail(gainMode)) ? 1 : 0;
}

static inline void reset_follow_mode_state(GaugeLogic *logic)
{
    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->blinkTimer = 0;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
}

static inline void apply_configured_trail_mode_state(GaugeLogic *logic, u8 advanceBlinkTimer)
{
    if ((GaugeTrailMode)logic->configuredTrailMode == GAUGE_TRAIL_MODE_FOLLOW)
    {
        reset_follow_mode_state(logic);
        return;
    }

    apply_non_follow_trail_mode_state(logic, advanceBlinkTimer);
}

static inline void invalidate_render_cache(GaugeLogic *logic)
{
    logic->lastValuePixels = CACHE_INVALID_U16;
    logic->lastTrailPixelsRendered = CACHE_INVALID_U16;
    logic->lastBlinkOn = CACHE_INVALID_U8;
    logic->lastActiveTrailState = CACHE_INVALID_U8;
    logic->needUpdate = 1;
}


/* =============================================================================
   GaugeLaneInstance internals
   ============================================================================= */

/* =============================================================================
   Dynamic VRAM mode implementation
   ============================================================================= */

static void free_dynamic_buffers(GaugeDynamic *dyn)
{
    if (!dyn)
        return;
    gauge_free_ptr((void **)&dyn->vramTileEmpty);
    gauge_free_ptr((void **)&dyn->vramTileFullValue);
    gauge_free_ptr((void **)&dyn->vramTileFullTrail);
    gauge_free_ptr((void **)&dyn->vramTileBridge);
    gauge_free_ptr((void **)&dyn->vramTilePipBase);
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
    dyn->vramTilePipBase = (u16 *)gauge_alloc_bytes((u16)(segmentCount * (u8)sizeof(u16)));
    dyn->cachedFillIndexBridge = (u8 *)gauge_alloc_bytes(segmentCount);
    dyn->cellCurrentTileIndex = (u16 *)gauge_alloc_bytes((u16)(cellCount * (u8)sizeof(u16)));
    dyn->cellValid = (u8 *)gauge_alloc_bytes(cellCount);
    if (!dyn->vramTileEmpty || !dyn->vramTileFullValue || !dyn->vramTileFullTrail ||
        !dyn->vramTileBridge || !dyn->vramTilePipBase || !dyn->cachedFillIndexBridge ||
        !dyn->cellCurrentTileIndex || !dyn->cellValid)
    {
        free_dynamic_buffers(dyn);
        return 0;
    }
    dyn->segmentCount = segmentCount;
    dyn->cellCount = cellCount;
    return 1;
}

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
 *   Partial tiles (1 per GaugeLaneInstance, shared across all segments):
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
static u8 init_dynamic_vram(GaugeDynamic *dyn, const GaugeLaneLayout *layout, u16 vramBase, u8 trailEnabled)
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
        dyn->vramTilePipBase[i] = 0;
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

    /* Partial tiles (scalars - 1 per GaugeLaneInstance, streamed on demand) */
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

    /* Cap tiles (1 per lane if enabled) */
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
static void preload_dynamic_standard_tiles(GaugeDynamic *dyn, const GaugeLaneLayout *layout, u8 trailEnabled)
{
    const u8 emptyIndex = STRIP_INDEX_EMPTY;
    const u8 fullValueIndex = STRIP_INDEX_FULL;
    const u8 fullTrailIndexBody = STRIP_INDEX_FULL_TRAIL;
    const u8 fullTrailIndexTrail = s_trailTileIndexByValueTrail[0][8]; /* value=0, trail=8 */

    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        const u32 *bodyStrip = select_base_strip(layout->tilesetBySegment[segmentId],
                                                 layout->gainTilesetBySegment[segmentId],
                                                 GAUGE_TRAIL_STATE_DAMAGE);
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
                                                      GAUGE_TRAIL_STATE_DAMAGE);
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
                                                   GAUGE_TRAIL_STATE_DAMAGE);
        if (capEndStrip)
            upload_fill_tile(capEndStrip, emptyIndex, dyn->vramTileCapEnd, DMA_QUEUE);
    }
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
                                            const GaugeLaneLayout *layout,
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
                                           const GaugeLaneLayout *layout,
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
 * @param lane  GaugeLaneInstance to initialize (must have layout and dyn set)
 */
static void init_dynamic_tilemap(GaugeLaneInstance *lane)
{
    const GaugeLaneLayout *layout = lane->layout;
    GaugeDynamic *dyn = &lane->dyn;

    /* Pre-calculate tilemap positions and cell validity for each cell */
    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        compute_tile_xy(layout->orientation, lane->originX, lane->originY, cellIndex,
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
 * Compute tile position for one physical PIP render tile.
 *
 * Base position comes from fillIndex timeline.
 * Then a positive transverse shift is applied:
 * - horizontal gauge: Y += segmentOffsetTiles + row
 * - vertical gauge:   X += segmentOffsetTiles + row
 */
static inline void compute_pip_render_xy(const GaugeLaneLayout *layout,
                                         u16 originX,
                                         u16 originY,
                                         u8 fillIndex,
                                         u8 row,
                                         u16 *outX,
                                         u16 *outY)
{
    const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
    const u8 segmentId = layout->segmentIdByCell[cellIndex];
    const u16 transverseShift = (u16)layout->pipOffsetBySegment[segmentId] + row;

    compute_tile_xy(layout->orientation, originX, originY, cellIndex, outX, outY);
    if (layout->orientation == GAUGE_ORIENT_HORIZONTAL)
        *outY = (u16)(*outY + transverseShift);
    else
        *outX = (u16)(*outX + transverseShift);
}

/**
 * Compute compact PIP strip index for SGDK row-major tilesets.
 *
 * Packing order is row -> state -> col:
 *   index = row * (sourceWidth * pipStateCount) + pipState * sourceWidth + sourceCol
 */
static inline u8 compute_pip_strip_index(u8 sourceWidth,
                                         u8 pipStateCount,
                                         u8 pipState,
                                         u8 row,
                                         u8 sourceCol)
{
    const u16 rowStride = (u16)sourceWidth * pipStateCount;
    const u16 index = (u16)row * rowStride + (u16)pipState * sourceWidth + sourceCol;
    return (u8)index;
}

/**
 * Initialize dynamic PIP VRAM layout with shared slots per segment/state.
 *
 * Slot mapping per segment:
 *   base + state * (sourceWidth * sourceHeight) + sourceRow * sourceWidth + sourceCol
 * where state is derived from the compact PIP strip state order.
 *
 * Runtime then updates only tilemap indices (no per-cell tile streaming).
 */
static u8 init_dynamic_pip_vram(GaugeLaneInstance *lane)
{
    const GaugeLaneLayout *layout = lane->layout;
    GaugeDynamic *dyn = &lane->dyn;
    u16 nextVram = lane->vramBase;
    const u8 renderCount = layout->pipRenderCount;

    if (!alloc_dynamic_buffers(dyn, layout->segmentCount, renderCount))
        return 0;

    for (u8 segmentId = 0; segmentId < dyn->segmentCount; segmentId++)
    {
        dyn->vramTileEmpty[segmentId] = 0;
        dyn->vramTileFullValue[segmentId] = 0;
        dyn->vramTileFullTrail[segmentId] = 0;
        dyn->vramTileBridge[segmentId] = 0;
        dyn->vramTilePipBase[segmentId] = 0;
        dyn->cachedFillIndexBridge[segmentId] = CACHE_INVALID_U8;
    }

    dyn->vramTileCapStart = 0;
    dyn->vramTileCapEnd = 0;
    dyn->vramTilePartialValue = 0;
    dyn->vramTilePartialTrail = 0;
    dyn->vramTilePartialEnd = 0;
    dyn->vramTilePartialTrailSecond = 0;
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

    for (u8 renderIndex = 0; renderIndex < dyn->cellCount; renderIndex++)
    {
        dyn->cellCurrentTileIndex[renderIndex] = CACHE_INVALID_U16;
        dyn->cellValid[renderIndex] = 0;
    }

    /* Preload all states for each segment using the compact strip layout. */
    for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
    {
        const u32 *pipStrip = layout->pipTilesetBySegment[segmentId];
        if (!pipStrip)
            continue;

        u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
        if (sourceWidth == 0)
            sourceWidth = 1;
        u8 sourceHeight = layout->pipSourceHeightBySegment[segmentId];
        if (sourceHeight == 0)
            sourceHeight = 1;
        if (sourceHeight > 4)
            sourceHeight = 4;

        u8 pipStateCount = layout->pipStateCountBySegment[segmentId];
        if (pipStateCount < 2)
            continue;

        dyn->vramTilePipBase[segmentId] = nextVram;

        for (u8 state = 0; state < pipStateCount; state++)
        {
            for (u8 row = 0; row < sourceHeight; row++)
            {
                for (u8 sourceCol = 0; sourceCol < sourceWidth; sourceCol++)
                {
                    const u8 stripIndex = compute_pip_strip_index(sourceWidth,
                                                                   pipStateCount,
                                                                   state,
                                                                   row,
                                                                   sourceCol);
                    upload_fill_tile(pipStrip, stripIndex, nextVram, DMA_QUEUE);
                    nextVram++;
                }
            }
        }
    }

    /* Initialize tilemap with EMPTY state for each render tile. */
    for (u8 renderIndex = 0; renderIndex < renderCount; renderIndex++)
    {
        const u8 fillIndex = layout->pipRenderFillIndexByRenderIndex[renderIndex];
        if (fillIndex == CACHE_INVALID_U8)
            continue;

        const u8 renderRow = layout->pipRenderRowByRenderIndex[renderIndex];
        const u8 sourceCol = layout->pipRenderSourceColByRenderIndex[renderIndex];
        const u8 sourceRow = layout->pipRenderSourceRowByRenderIndex[renderIndex];
        const u8 extraHFlip = layout->pipRenderExtraHFlipByRenderIndex[renderIndex];
        const u8 extraVFlip = layout->pipRenderExtraVFlipByRenderIndex[renderIndex];
        const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        if (!layout->pipTilesetBySegment[segmentId] ||
            layout->pipStateCountBySegment[segmentId] < 2)
            continue;

        const u16 pipBaseTile = dyn->vramTilePipBase[segmentId];
        u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
        if (sourceWidth == 0)
            sourceWidth = 1;

        const u16 vramTile = (u16)(pipBaseTile + sourceRow * sourceWidth + sourceCol);
        const u16 attr = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        (u8)(layout->verticalFlip ^ extraVFlip),
                                        (u8)(layout->horizontalFlip ^ extraHFlip),
                                        vramTile);

        u16 x = 0;
        u16 y = 0;
        compute_pip_render_xy(layout, lane->originX, lane->originY, fillIndex, renderRow, &x, &y);
        VDP_setTileMapXY(WINDOW, attr, x, y);

        dyn->cellCurrentTileIndex[renderIndex] = vramTile;
        dyn->cellValid[renderIndex] = 1;
    }

    return 1;
}

/**
 * Initialize tilemap for PIP mode (one VRAM tile per rendered PIP tile).
 *
 * Allocates one VRAM tile per render tile, uploads the EMPTY state from each segment's
 * compact strip, and writes the initial tilemap. Also sets up GaugeStreamCell
 * entries for per-cell DMA updates at runtime.
 *
 * @param lane  GaugeLaneInstance to initialize (must have layout set)
 */
static void write_tilemap_pip_init(GaugeLaneInstance *lane)
{
    const GaugeLaneLayout *layout = lane->layout;
    lane->cellCount = 0;

    for (u8 renderIndex = 0; renderIndex < layout->pipRenderCount; renderIndex++)
    {
        const u8 fillIndex = layout->pipRenderFillIndexByRenderIndex[renderIndex];
        if (fillIndex == CACHE_INVALID_U8)
            continue;
        const u8 renderRow = layout->pipRenderRowByRenderIndex[renderIndex];
        const u8 sourceCol = layout->pipRenderSourceColByRenderIndex[renderIndex];
        const u8 sourceRow = layout->pipRenderSourceRowByRenderIndex[renderIndex];
        const u8 extraHFlip = layout->pipRenderExtraHFlipByRenderIndex[renderIndex];
        const u8 extraVFlip = layout->pipRenderExtraVFlipByRenderIndex[renderIndex];
        const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *pipStrip = layout->pipTilesetBySegment[segmentId];
        if (!pipStrip)
            continue;
        const u8 pipStateCount = layout->pipStateCountBySegment[segmentId];
        if (pipStateCount < 2)
            continue;

        if (lane->cellCount >= layout->pipRenderCount)
            break;

        const u16 vramTile = (u16)(lane->vramBase + lane->cellCount);
        const u16 attr = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        (u8)(layout->verticalFlip ^ extraVFlip),
                                        (u8)(layout->horizontalFlip ^ extraHFlip),
                                        vramTile);

        u16 x = 0;
        u16 y = 0;
        compute_pip_render_xy(layout, lane->originX, lane->originY, fillIndex, renderRow, &x, &y);
        VDP_setTileMapXY(WINDOW, attr, x, y);

        u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
        if (sourceWidth == 0)
            sourceWidth = 1;
        const u8 stripIndex = compute_pip_strip_index(sourceWidth,
                                                       pipStateCount,
                                                       PIP_STATE_EMPTY,
                                                       sourceRow,
                                                       sourceCol);

        /* Preload EMPTY tile for this local position. */
        upload_fill_tile(pipStrip, stripIndex, vramTile, DMA_QUEUE);

        lane->cells[lane->cellCount].vramTileIndex = vramTile;
        lane->cells[lane->cellCount].cachedStrip = pipStrip;
        lane->cells[lane->cellCount].cachedFillIndex = stripIndex;
        lane->cells[lane->cellCount].cellIndex = cellIndex;
        lane->cells[lane->cellCount].pipFillIndex = fillIndex;
        lane->cells[lane->cellCount].pipRow = renderRow;
        lane->cells[lane->cellCount].pipRenderIndex = renderIndex;
        lane->cellCount++;
    }
}

/**
 * Initialize tilemap for fixed VRAM mode (one VRAM tile per cell, fill mode).
 *
 * Allocates one VRAM tile per valid cell, pre-caches tileset strip pointers
 * (body/end/trail/bridge) per cell to avoid per-frame segment lookups, uploads
 * initial empty tiles, and writes the tilemap on the WINDOW plane.
 *
 * @param lane  GaugeLaneInstance to initialize (must have layout and vramBase set)
 */
static void write_tilemap_fixed_init(GaugeLaneInstance *lane)
{
    const GaugeLaneLayout *layout = lane->layout;
    lane->cellCount = 0;

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *bodyStrip = layout->tilesetBySegment[segmentId];
        const u32 *gainBodyStrip = layout->gainTilesetBySegment[segmentId];

        if (!bodyStrip && !gainBodyStrip)
            continue;

        if (lane->cellCount >= layout->length)
            break;

        const u16 vramTile = (u16)(lane->vramBase + lane->cellCount);
        const u16 attr = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        layout->verticalFlip, layout->horizontalFlip, vramTile);

        u16 x, y;
        compute_tile_xy(layout->orientation, lane->originX, lane->originY, cellIndex, &x, &y);

        VDP_setTileMapXY(WINDOW, attr, x, y);

        lane->cells[lane->cellCount].vramTileIndex = vramTile;
        lane->cells[lane->cellCount].cachedStrip = NULL;
        lane->cells[lane->cellCount].cachedFillIndex = CACHE_INVALID_U8;
        lane->cells[lane->cellCount].cellIndex = cellIndex;
        lane->cells[lane->cellCount].pipFillIndex = CACHE_INVALID_U8;
        lane->cells[lane->cellCount].pipRow = 0;
        lane->cells[lane->cellCount].pipRenderIndex = CACHE_INVALID_U8;
        lane->cellCount++;

    } 
}

static inline GaugeLaneInstance *gauge_get_baseLane_instance(const Gauge *gauge)
{
    if (!gauge || gauge->laneCount == 0 || gauge->baseLaneIndex >= gauge->laneCount)
        return NULL;
    return gauge->lanes[gauge->baseLaneIndex];
}

static void gauge_build_baseLane_fill_decisions(Gauge *gauge,
                                              u16 valuePixels,
                                              u16 trailPixelsRendered,
                                              u16 trailPixelsActual,
                                              u8 blinkOffActive,
                                              u8 trailMode)
{
    GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
    if (!baseLane)
        return;

    const GaugeLaneLayout *baseLaneLayout = baseLane->layout;
    FillLoopContext ctx;
    init_fill_loop_context(baseLaneLayout, valuePixels, trailPixelsRendered,
                           trailPixelsActual, blinkOffActive, trailMode, &ctx);

    for (u8 i = 0; i < GAUGE_MAX_LENGTH; i++)
    {
        gauge->baseLaneDecisionTypeByFillIndex[i] = (u8)CELL_DECISION_STANDARD_EMPTY;
        gauge->baseLaneDecisionIdxByFillIndex[i] = STRIP_INDEX_EMPTY;
        gauge->baseLaneDecisionCapStartBreakByFillIndex[i] = 0;
        gauge->baseLaneDecisionCapStartTrailByFillIndex[i] = 0;
        gauge->baseLaneDecisionUseBlinkVariantByFillIndex[i] = 0;
    }

    for (u8 cellIndex = 0; cellIndex < baseLaneLayout->length; cellIndex++)
    {
        const u8 fillIndex = baseLaneLayout->fillIndexByCell[cellIndex];
        const u8 segmentId = baseLaneLayout->segmentIdByCell[cellIndex];
        const u32 *bodyStrip = select_base_strip(
            baseLaneLayout->tilesetBySegment[segmentId],
            baseLaneLayout->gainTilesetBySegment[segmentId],
            trailMode);

        CellDecision decision;
        compute_cell_decision(baseLaneLayout, &ctx, cellIndex, fillIndex,
                              segmentId, bodyStrip, &decision);

        gauge->baseLaneDecisionTypeByFillIndex[fillIndex] = (u8)decision.type;
        gauge->baseLaneDecisionIdxByFillIndex[fillIndex] = decision.fillStripIndex;
        gauge->baseLaneDecisionCapStartBreakByFillIndex[fillIndex] = decision.capStartUsesBreak;
        gauge->baseLaneDecisionCapStartTrailByFillIndex[fillIndex] = decision.capStartUsesTrail;
        gauge->baseLaneDecisionUseBlinkVariantByFillIndex[fillIndex] = decision.useBlinkVariant;
    }
}

typedef struct
{
    CellDecisionType type;
    const u32 *strip;
    u8 fillStripIndex;
    u8 capStartUsesBreak;
    u8 capStartUsesTrail;
    u8 terminalOverrideApplied;
} LocalResolvedDecision;

typedef struct
{
    u8 valid;
    const u32 *body;
    const u32 *end;
    const u32 *trail;
    const u32 *bridge;
    const u32 *capStart;
    const u32 *capStartBreak;
    const u32 *capStartTrail;
    const u32 *capEnd;
} LocalStripSet;

typedef struct
{
    LocalStripSet normalBySegment[GAUGE_MAX_SEGMENTS];
    LocalStripSet blinkBySegment[GAUGE_MAX_SEGMENTS];
} LocalStripCache;

static inline void build_local_strip_set(const GaugeLaneLayout *layout,
                                         u8 segmentId,
                                         u8 trailMode,
                                         u8 useBlinkVariant,
                                         LocalStripSet *set)
{
    set->body = select_base_strip(layout->tilesetBySegment[segmentId],
                                  layout->gainTilesetBySegment[segmentId],
                                  trailMode);
    set->end = select_base_strip(layout->tilesetEndBySegment[segmentId],
                                 layout->gainTilesetEndBySegment[segmentId],
                                 trailMode);
    set->trail = select_base_strip(layout->tilesetTrailBySegment[segmentId],
                                   layout->gainTilesetTrailBySegment[segmentId],
                                   trailMode);
    set->bridge = select_base_strip(layout->tilesetBridgeBySegment[segmentId],
                                    layout->gainTilesetBridgeBySegment[segmentId],
                                    trailMode);
    set->capStart = select_base_strip(layout->tilesetCapStartBySegment[segmentId],
                                      layout->gainTilesetCapStartBySegment[segmentId],
                                      trailMode);
    set->capStartBreak = select_base_strip(layout->tilesetCapStartBreakBySegment[segmentId],
                                           layout->gainTilesetCapStartBreakBySegment[segmentId],
                                           trailMode);
    set->capStartTrail = select_base_strip(layout->tilesetCapStartTrailBySegment[segmentId],
                                           layout->gainTilesetCapStartTrailBySegment[segmentId],
                                           trailMode);
    set->capEnd = select_base_strip(layout->tilesetCapEndBySegment[segmentId],
                                    layout->gainTilesetCapEndBySegment[segmentId],
                                    trailMode);

    if (useBlinkVariant)
    {
        const u32 *blinkBody = select_blink_strip(layout->blinkOffTilesetBySegment[segmentId],
                                                  layout->gainBlinkOffTilesetBySegment[segmentId],
                                                  trailMode);
        const u32 *blinkEnd = select_blink_strip(layout->blinkOffTilesetEndBySegment[segmentId],
                                                 layout->gainBlinkOffTilesetEndBySegment[segmentId],
                                                 trailMode);
        const u32 *blinkTrail = select_blink_strip(layout->blinkOffTilesetTrailBySegment[segmentId],
                                                   layout->gainBlinkOffTilesetTrailBySegment[segmentId],
                                                   trailMode);
        const u32 *blinkBridge = select_blink_strip(layout->blinkOffTilesetBridgeBySegment[segmentId],
                                                    layout->gainBlinkOffTilesetBridgeBySegment[segmentId],
                                                    trailMode);
        const u32 *blinkCapStart = select_blink_strip(layout->blinkOffTilesetCapStartBySegment[segmentId],
                                                      layout->gainBlinkOffTilesetCapStartBySegment[segmentId],
                                                      trailMode);
        const u32 *blinkCapStartBreak = select_blink_strip(layout->blinkOffTilesetCapStartBreakBySegment[segmentId],
                                                           layout->gainBlinkOffTilesetCapStartBreakBySegment[segmentId],
                                                           trailMode);
        const u32 *blinkCapStartTrail = select_blink_strip(layout->blinkOffTilesetCapStartTrailBySegment[segmentId],
                                                           layout->gainBlinkOffTilesetCapStartTrailBySegment[segmentId],
                                                           trailMode);
        const u32 *blinkCapEnd = select_blink_strip(layout->blinkOffTilesetCapEndBySegment[segmentId],
                                                    layout->gainBlinkOffTilesetCapEndBySegment[segmentId],
                                                    trailMode);

        if (blinkBody) set->body = blinkBody;
        if (blinkEnd) set->end = blinkEnd;
        if (blinkTrail) set->trail = blinkTrail;
        if (blinkBridge) set->bridge = blinkBridge;
        if (blinkCapStart) set->capStart = blinkCapStart;
        if (blinkCapStartBreak) set->capStartBreak = blinkCapStartBreak;
        if (blinkCapStartTrail) set->capStartTrail = blinkCapStartTrail;
        if (blinkCapEnd) set->capEnd = blinkCapEnd;
    }

    set->valid = 1;
}

static inline const LocalStripSet *get_local_strip_set(const GaugeLaneLayout *layout,
                                                       u8 segmentId,
                                                       u8 trailMode,
                                                       u8 useBlinkVariant,
                                                       LocalStripCache *cache)
{
    LocalStripSet *set = useBlinkVariant
        ? &cache->blinkBySegment[segmentId]
        : &cache->normalBySegment[segmentId];

    if (!set->valid)
        build_local_strip_set(layout, segmentId, trailMode, useBlinkVariant, set);

    return set;
}

static inline const u32 *resolve_local_strip_from_baseLane_type(const LocalStripSet *stripSet,
                                                              CellDecisionType type,
                                                              u8 capStartUsesBreak,
                                                              u8 capStartUsesTrail)
{
    const u32 *bodyStrip = stripSet->body;
    const u32 *endStrip = stripSet->end;
    const u32 *trailStrip = stripSet->trail;
    const u32 *bridgeStrip = stripSet->bridge;
    const u32 *capStartStrip = stripSet->capStart;
    const u32 *capStartBreakStrip = stripSet->capStartBreak;
    const u32 *capStartTrailStrip = stripSet->capStartTrail;
    const u32 *capEndStrip = stripSet->capEnd;

    switch (type)
    {
    case CELL_DECISION_CAP_END:
        return capEndStrip ? capEndStrip : (endStrip ? endStrip : bodyStrip);
    case CELL_DECISION_CAP_START:
        if (capStartUsesTrail)
            return capStartTrailStrip ? capStartTrailStrip : (capStartStrip ? capStartStrip : bodyStrip);
        if (capStartUsesBreak)
            return capStartBreakStrip ? capStartBreakStrip : (capStartStrip ? capStartStrip : bodyStrip);
        return capStartStrip ? capStartStrip : bodyStrip;
    case CELL_DECISION_BRIDGE:
        return bridgeStrip ? bridgeStrip : bodyStrip;
    case CELL_DECISION_PARTIAL_END:
        return endStrip ? endStrip : bodyStrip;
    case CELL_DECISION_STANDARD_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL2:
        return trailStrip ? trailStrip : bodyStrip;
    case CELL_DECISION_STANDARD_FULL:
    case CELL_DECISION_STANDARD_EMPTY:
    case CELL_DECISION_PARTIAL_VALUE:
    default:
        return bodyStrip;
    }
}

static inline const u32 *resolve_local_body_strip(const LocalStripSet *stripSet)
{
    return stripSet->body;
}

static inline u8 compute_linkedLane_terminal_end_index(const GaugeLaneLayout *layout,
                                                      u16 currentValuePixels,
                                                      u16 trailPixelsRendered,
                                                      u8 *outStripIndex)
{
    if (!layout || layout->length == 0)
        return 0;

    const u8 terminalFillIndex = (u8)(layout->length - 1);
    const u8 terminalCellIndex = layout->cellIndexByFillIndex[terminalFillIndex];
    u8 valuePxInTerminalCell = 0;
    u8 trailPxInTerminalCell = 0;

    compute_fill_for_cell(layout, terminalCellIndex,
                          currentValuePixels, trailPixelsRendered,
                          &valuePxInTerminalCell, &trailPxInTerminalCell);

    if (trailPxInTerminalCell == 0)
        return 0;

    if (outStripIndex)
    {
        *outStripIndex = s_tileIndexByValueTrail[valuePxInTerminalCell]
                                                [trailPxInTerminalCell];
    }

    return 1;
}

static inline u8 compute_linkedLane_terminal_override(const GaugeLaneLayout *layout,
                                                     u16 currentValuePixels,
                                                     u16 trailPixelsRendered,
                                                     u8 *forcedStripIndex)
{
    if (!layout || !layout->endOverrideEnabled || layout->length == 0)
        return 0;

    /* For shortened linked lanes, keep a stable local END shape on the
     * terminal cell based on what is actually rendered this frame. Using the
     * visible trail extent avoids showing a fake END during blink-off frames
     * where the trail is intentionally hidden. */

    return compute_linkedLane_terminal_end_index(
        layout, currentValuePixels, trailPixelsRendered, forcedStripIndex);
}

typedef struct
{
    u8 terminalOverrideActive;
    u8 terminalForcedIndex;
} LinkedLaneTerminalOverrideContext;

static inline void prepare_linkedLane_terminal_override(const Gauge *gauge,
                                                       GaugeLaneInstance *lane,
                                                       const GaugeLaneLayout *layout,
                                                       u8 isLinkedLane,
                                                       u16 valuePixels,
                                                       u16 trailPixelsRendered,
                                                       LinkedLaneTerminalOverrideContext *ctx)
{
    if (!ctx)
        return;

    ctx->terminalOverrideActive = 0;
    ctx->terminalForcedIndex = STRIP_INDEX_FULL;

    if (!gauge || !lane || !layout || !isLinkedLane ||
        !layout->endOverrideEnabled || layout->length == 0)
    {
        return;
    }

    ctx->terminalOverrideActive = compute_linkedLane_terminal_override(
        layout, valuePixels, trailPixelsRendered, &ctx->terminalForcedIndex);
}

static inline void project_baseLane_decision_to_local(const LocalStripSet *stripSet,
                                                     CellDecisionType baseLaneType,
                                                     u8 baseLaneIdx,
                                                     u8 capStartUsesBreak,
                                                     u8 capStartUsesTrail,
                                                     LocalResolvedDecision *resolved)
{
    resolved->type = baseLaneType;
    resolved->fillStripIndex = baseLaneIdx;
    resolved->capStartUsesBreak = capStartUsesBreak;
    resolved->capStartUsesTrail = capStartUsesTrail;
    resolved->terminalOverrideApplied = 0;
    resolved->strip = resolve_local_strip_from_baseLane_type(
        stripSet, baseLaneType, capStartUsesBreak, capStartUsesTrail);
}

static inline void apply_linkedLane_terminal_override_to_local(const LocalStripSet *stripSet,
                                                              u8 isTerminalCell,
                                                              u8 terminalOverrideActive,
                                                              u8 terminalForcedIndex,
                                                              LocalResolvedDecision *resolved)
{
    /* A linked lane terminal override only makes sense when the local skin
     * provides a dedicated END strip. Otherwise, forcing PARTIAL_END would
     * fall back to BODY and can overwrite the shared dynamic PARTIAL_VALUE
     * tile with a trail/end index. */
    if (!terminalOverrideActive || !isTerminalCell || !stripSet ||
        !stripSet->end || !resolved)
        return;

    resolved->type = CELL_DECISION_PARTIAL_END;
    resolved->fillStripIndex = terminalForcedIndex;
    resolved->capStartUsesBreak = 0;
    resolved->capStartUsesTrail = 0;
    resolved->terminalOverrideApplied = 1;
    resolved->strip = resolve_local_strip_from_baseLane_type(
        stripSet, CELL_DECISION_PARTIAL_END, 0, 0);
}

static inline void apply_dynamic_constraints_to_local(const GaugeLaneInstance *lane,
                                                      u8 segmentId,
                                                      const u32 *fallbackBody,
                                                      const u32 *fallbackTrail,
                                                      CellDecisionType baseLaneType,
                                                      u8 isLinkedLane,
                                                      u8 isTerminalCell,
                                                      u8 terminalOverrideActive,
                                                      LocalResolvedDecision *resolved)
{
    if (lane->vramMode != GAUGE_VRAM_DYNAMIC)
        return;

    if (resolved->type == CELL_DECISION_BRIDGE &&
        lane->dyn.vramTileBridge &&
        lane->dyn.vramTileBridge[segmentId] == 0)
    {
        resolved->type = CELL_DECISION_STANDARD_FULL;
        resolved->fillStripIndex = STRIP_INDEX_FULL;
        resolved->strip = fallbackBody;
    }
    else if (resolved->type == CELL_DECISION_CAP_START && lane->dyn.vramTileCapStart == 0)
    {
        resolved->type = CELL_DECISION_PARTIAL_VALUE;
        resolved->strip = fallbackBody;
    }
    else if (resolved->type == CELL_DECISION_CAP_END && lane->dyn.vramTileCapEnd == 0)
    {
        resolved->type = CELL_DECISION_PARTIAL_END;
    }

    /* Dynamic mode only has one PARTIAL_END VRAM slot.
     * If terminal override is active on a linkedLane, avoid a second
     * PARTIAL_END decision on non-terminal cells (would overwrite shared slot). */
    if (terminalOverrideActive &&
        isLinkedLane &&
        !isTerminalCell &&
        baseLaneType == CELL_DECISION_PARTIAL_END)
    {
        resolved->type = CELL_DECISION_PARTIAL_VALUE;
        resolved->strip = fallbackBody;
    }

    if (resolved->type == CELL_DECISION_PARTIAL_END && lane->dyn.vramTilePartialEnd == 0)
    {
        resolved->type = CELL_DECISION_PARTIAL_VALUE;
        resolved->strip = fallbackBody;
    }

    if (resolved->type == CELL_DECISION_PARTIAL_TRAIL2 &&
        lane->dyn.vramTilePartialTrailSecond == 0)
    {
        if (lane->dyn.vramTilePartialTrail != 0)
        {
            resolved->type = CELL_DECISION_PARTIAL_TRAIL;
            resolved->strip = fallbackTrail ? fallbackTrail : fallbackBody;
        }
        else
        {
            resolved->type = CELL_DECISION_PARTIAL_VALUE;
            resolved->strip = fallbackBody;
        }
    }

    if (resolved->type == CELL_DECISION_PARTIAL_TRAIL &&
        lane->dyn.vramTilePartialTrail == 0)
    {
        resolved->type = CELL_DECISION_PARTIAL_VALUE;
        resolved->strip = fallbackBody;
    }

    if (resolved->type == CELL_DECISION_STANDARD_TRAIL &&
        lane->dyn.vramTileFullTrail &&
        lane->dyn.vramTileFullTrail[segmentId] == 0)
    {
        if (lane->dyn.vramTilePartialTrail != 0)
        {
            resolved->type = CELL_DECISION_PARTIAL_TRAIL;
            resolved->strip = fallbackTrail ? fallbackTrail : fallbackBody;
            resolved->fillStripIndex = (resolved->strip == fallbackTrail &&
                                        fallbackTrail != fallbackBody)
                ? s_trailTileIndexByValueTrail[0][8]
                : STRIP_INDEX_FULL_TRAIL;
        }
        else
        {
            resolved->type = CELL_DECISION_PARTIAL_VALUE;
            resolved->strip = fallbackBody;
            resolved->fillStripIndex = STRIP_INDEX_FULL_TRAIL;
        }
    }
}

static inline void clamp_local_decision_index(const u32 *bodyStrip,
                                              const u32 *trailStrip,
                                              LocalResolvedDecision *resolved)
{
    if (resolved->fillStripIndex > 63)
        resolved->fillStripIndex = 63;

    const u8 allow64 = (resolved->strip == trailStrip && trailStrip != bodyStrip) ? 1 : 0;
    if (!allow64 && resolved->fillStripIndex > 44)
        resolved->fillStripIndex = 44;
}

static void resolve_local_decision_for_lane(const GaugeLaneInstance *lane,
                                            u8 cellFillIndex,
                                            u8 segmentId,
                                            CellDecisionType baseLaneType,
                                            u8 baseLaneIdx,
                                            u8 capStartUsesBreak,
                                            u8 capStartUsesTrail,
                                            u8 isLinkedLane,
                                            u8 terminalOverrideActive,
                                            u8 terminalForcedIndex,
                                            u8 useBlinkVariant,
                                            u8 trailMode,
                                            LocalStripCache *stripCache,
                                            LocalResolvedDecision *resolved)
{
    const GaugeLaneLayout *layout = lane->layout;
    const LocalStripSet *stripSet = get_local_strip_set(
        layout, segmentId, trailMode, useBlinkVariant, stripCache);
    const u32 *fallbackBody = resolve_local_body_strip(stripSet);
    const u32 *fallbackTrail = resolve_local_strip_from_baseLane_type(
        stripSet, CELL_DECISION_PARTIAL_TRAIL, 0, 0);

    if (!fallbackBody)
    {
        fallbackBody = select_base_strip(layout->tilesetBySegment[segmentId],
                                         layout->gainTilesetBySegment[segmentId],
                                         trailMode);
    }

    const u8 terminalFillIndex = (u8)(layout->length - 1);
    const u8 isTerminalCell = (cellFillIndex == terminalFillIndex) ? 1 : 0;
    project_baseLane_decision_to_local(stripSet, baseLaneType, baseLaneIdx,
                                     capStartUsesBreak, capStartUsesTrail, resolved);
    apply_linkedLane_terminal_override_to_local(stripSet, isTerminalCell,
                                                terminalOverrideActive, terminalForcedIndex,
                                                resolved);

    if (!resolved->strip)
    {
        resolved->strip = fallbackBody;
        resolved->type = CELL_DECISION_PARTIAL_VALUE;
    }

    apply_dynamic_constraints_to_local(lane, segmentId, fallbackBody, fallbackTrail,
                                       baseLaneType, isLinkedLane, isTerminalCell,
                                       terminalOverrideActive, resolved);
    clamp_local_decision_index(fallbackBody, fallbackTrail, resolved);
}

static void gauge_build_baseLane_pip_states(Gauge *gauge,
                                          u16 valuePixels,
                                          u16 trailPixelsRendered,
                                          u16 trailPixelsActual,
                                          u8 blinkOffActive,
                                          u8 trailMode)
{
    GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
    if (!baseLane)
        return;

    const GaugeLaneLayout *baseLaneLayout = baseLane->layout;
    for (u8 i = 0; i < GAUGE_MAX_LENGTH; i++)
        gauge->baseLanePipStateByFillIndex[i] = PIP_STATE_EMPTY;

    for (u8 fillIndex = 0; fillIndex < baseLaneLayout->length; fillIndex++)
    {
        const u8 cellIndex = baseLaneLayout->cellIndexByFillIndex[fillIndex];
        gauge->baseLanePipStateByFillIndex[fillIndex] =
            select_pip_state_for_cell(baseLaneLayout, cellIndex,
                                      valuePixels, trailPixelsRendered, trailPixelsActual,
                                      blinkOffActive, trailMode);
    }
}

/**
 * Process fixed mode: stream tiles for all cells via DMA.
 *
 * Each cell has its own dedicated VRAM tile. When the gauge value changes,
 * only the affected cells get new tile data DMA'd from ROM strips.
 * Change detection per cell (cachedFillIndex + cachedStrip) avoids
 * redundant DMA transfers.
 *
 * Cell classification is computed once on the baseLane for the frame.
 * This function only projects local cells to baseLane fill indices, resolves
 * the local strip fallback, then uploads the selected tile if needed.
 *
 * @param lane                 Lane to render
 * @param valuePixels          Current value fill in pixels
 * @param trailPixelsRendered  Current trail fill in pixels (after blink)
 * @param trailPixelsActual    Current trail fill in pixels (before blink)
 * @param blinkOffActive       1 if blink-off rendering is active this frame
 * @param trailMode            Current trail mode (DAMAGE/GAIN/NONE)
 *
 * Cost: ~200 cycles per cell (DMA setup dominant), ~50 cycles for trivial cells
 */
static void process_fixed_mode(GaugeLaneInstance *lane,
                               u16 valuePixels,
                               u16 trailPixelsRendered,
                               u16 trailPixelsActual,
                               u8 blinkOffActive,
                               u8 trailMode)
{
    (void)trailPixelsActual;
    (void)blinkOffActive;
    const GaugeLaneLayout *layout = lane->layout;
    Gauge *gauge = s_activeGaugeForRender;
    if (!gauge)
        return;
    const u8 isLinkedLane = (lane != gauge_get_baseLane_instance(gauge)) ? 1 : 0;
    LinkedLaneTerminalOverrideContext terminalCtx;
    prepare_linkedLane_terminal_override(gauge, lane, layout, isLinkedLane,
                                        valuePixels, trailPixelsRendered, &terminalCtx);
    LocalStripCache stripCache = {0};

#if GAUGE_ENABLE_TRACE
    const u8 traceEnabled = s_traceContext.active;
    GaugeTraceCell traceByCell[GAUGE_MAX_LENGTH];

    if (traceEnabled)
        trace_clear_recorded_cells(traceByCell, layout->length);
#endif

    /* Countdown loop: 68000 zero-flag test is free after decrement (dbra) */
    u8 i = lane->cellCount;
    while (i--)
    {
        GaugeStreamCell *cell = &lane->cells[i];
        const u8 cellIndex = cell->cellIndex;
        const u8 cellFillIndex = layout->fillIndexByCell[cellIndex];
        const u8 mappedBaseLaneFillIndex = lane->baseLaneFillIndexByCell[cellIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const CellDecisionType baseLaneType =
            (CellDecisionType)gauge->baseLaneDecisionTypeByFillIndex[mappedBaseLaneFillIndex];
        const u8 baseLaneIdx = gauge->baseLaneDecisionIdxByFillIndex[mappedBaseLaneFillIndex];
        const u8 capStartUsesBreak =
            gauge->baseLaneDecisionCapStartBreakByFillIndex[mappedBaseLaneFillIndex];
        const u8 capStartUsesTrail =
            gauge->baseLaneDecisionCapStartTrailByFillIndex[mappedBaseLaneFillIndex];
        const u8 useBlinkVariant =
            gauge->baseLaneDecisionUseBlinkVariantByFillIndex[mappedBaseLaneFillIndex];
        LocalResolvedDecision resolved;
        resolve_local_decision_for_lane(lane, cellFillIndex, segmentId, baseLaneType, baseLaneIdx,
                                        capStartUsesBreak, capStartUsesTrail,
                                        isLinkedLane,
                                        terminalCtx.terminalOverrideActive,
                                        terminalCtx.terminalForcedIndex,
                                        useBlinkVariant, trailMode,
                                        &stripCache, &resolved);

        CellDecision decision;
        decision.type = resolved.type;
        decision.strip = resolved.strip;
        decision.fillStripIndex = resolved.fillStripIndex;
        decision.capStartUsesBreak = resolved.capStartUsesBreak;
        decision.capStartUsesTrail = resolved.capStartUsesTrail;
        decision.useBlinkVariant = useBlinkVariant;

#if GAUGE_ENABLE_TRACE
        if (traceEnabled)
            trace_record_cell(traceByCell, cellIndex, segmentId, baseLaneType, baseLaneIdx,
                              resolved.terminalOverrideApplied, &decision);
#endif

        upload_cell_if_needed(cell, decision.strip, decision.fillStripIndex);
    }

#if GAUGE_ENABLE_TRACE
    if (traceEnabled)
        trace_emit_recorded_cells(layout, traceByCell);
#endif
}

/**
 * Process dynamic mode -- Tilemap-based rendering algorithm.
 *
 * Uses baseLane-cached strip decision metadata (type/index + flags), resolves
 * local strips for this lane, then routes to the appropriate dynamic VRAM slot.
 * Standard tiles are pre-loaded; partial/cap/bridge tiles are streamed on demand.
 *
 * @param lane               Lane to process
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
static void process_dynamic_mode(GaugeLaneInstance *lane,
                                 u16 valuePixels,
                                 u16 trailPixelsRendered,
                                 u16 trailPixelsActual,
                                 u8 blinkOffActive,
                                 u8 blinkOnChanged,
                                 u8 trailMode,
                                 u8 trailModeChanged)
{
    (void)trailPixelsActual;
    GaugeDynamic *dyn = &lane->dyn;
    const GaugeLaneLayout *layout = lane->layout;
    Gauge *gauge = s_activeGaugeForRender;
    if (!gauge)
        return;
    const u8 isLinkedLane = (lane != gauge_get_baseLane_instance(gauge)) ? 1 : 0;
    LinkedLaneTerminalOverrideContext terminalCtx;
    prepare_linkedLane_terminal_override(gauge, lane, layout, isLinkedLane,
                                        valuePixels, trailPixelsRendered, &terminalCtx);
    LocalStripCache stripCache = {0};

    /* Pre-compute tilemap attribute base (without tile index) */
    const u16 attrBase = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        layout->verticalFlip, layout->horizontalFlip, 0);

    /* Dynamic mode caches tile data in VRAM. When blink toggles or trail mode
     * changes, the cached standard tiles (full/empty/trail) may now come from
     * a different tileset (normal vs blink-off, damage vs gain). We must:
     * 1. Invalidate partial tile caches (forces re-upload on next use)
     * 2. Re-upload full body tiles if trail mode changed (DAMAGE<->GAIN)
     * 3. Re-upload full trail tiles with the correct tileset variant */
    const u8 layoutHasBlink = (trailMode == GAUGE_TRAIL_STATE_GAIN)
                             ? layout->hasGainBlinkOff : layout->hasBlinkOff;
    if ((blinkOnChanged && layoutHasBlink) || trailModeChanged)
    {
        reset_dynamic_blink_cache(dyn);
        if (trailModeChanged)
            reload_dynamic_full_body_tiles(dyn, layout, trailMode);
        reload_dynamic_full_trail_tiles(dyn, layout, blinkOffActive, trailMode);
    }

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
        const u8 mappedBaseLaneFillIndex = lane->baseLaneFillIndexByCell[cellIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const CellDecisionType baseLaneType =
            (CellDecisionType)gauge->baseLaneDecisionTypeByFillIndex[mappedBaseLaneFillIndex];
        const u8 baseLaneIdx = gauge->baseLaneDecisionIdxByFillIndex[mappedBaseLaneFillIndex];
        const u8 capStartUsesBreak =
            gauge->baseLaneDecisionCapStartBreakByFillIndex[mappedBaseLaneFillIndex];
        const u8 capStartUsesTrail =
            gauge->baseLaneDecisionCapStartTrailByFillIndex[mappedBaseLaneFillIndex];
        const u8 useBlinkVariant =
            gauge->baseLaneDecisionUseBlinkVariantByFillIndex[mappedBaseLaneFillIndex];
        LocalResolvedDecision resolved;
        resolve_local_decision_for_lane(lane, cellFillIndex, segmentId, baseLaneType, baseLaneIdx,
                                        capStartUsesBreak, capStartUsesTrail,
                                        isLinkedLane,
                                        terminalCtx.terminalOverrideActive,
                                        terminalCtx.terminalForcedIndex,
                                        useBlinkVariant, trailMode,
                                        &stripCache, &resolved);

        CellDecision decision;
        decision.type = resolved.type;
        decision.strip = resolved.strip;
        decision.fillStripIndex = resolved.fillStripIndex;
        decision.capStartUsesBreak = resolved.capStartUsesBreak;
        decision.capStartUsesTrail = resolved.capStartUsesTrail;
        decision.useBlinkVariant = useBlinkVariant;

#if GAUGE_ENABLE_TRACE
        if (s_traceContext.active)
            trace_emit_cell_line(cellIndex, segmentId, baseLaneType, baseLaneIdx,
                                 resolved.terminalOverrideApplied, &decision);
#endif

        /* === VRAM routing: map decision type to VRAM slot + cache === */
        u16 vramTile = 0;
        u8 needsUpload = 0;

        switch (decision.type)
        {
        case CELL_DECISION_CAP_END:
            vramTile = dyn->vramTileCapEnd;
            if (dyn->cachedFillIndexCapEnd != decision.fillStripIndex)
            {
                needsUpload = 1;
                dyn->cachedFillIndexCapEnd = decision.fillStripIndex;
            }
            break;

        case CELL_DECISION_CAP_START:
            vramTile = dyn->vramTileCapStart;
            if (dyn->cachedFillIndexCapStart != decision.fillStripIndex ||
                dyn->loadedCapStartUsesBreak != decision.capStartUsesBreak ||
                dyn->loadedCapStartUsesTrail != decision.capStartUsesTrail)
            {
                needsUpload = 1;
                dyn->cachedFillIndexCapStart = decision.fillStripIndex;
                dyn->loadedCapStartUsesBreak = decision.capStartUsesBreak;
                dyn->loadedCapStartUsesTrail = decision.capStartUsesTrail;
            }
            break;

        case CELL_DECISION_BRIDGE:
        {
            const u16 vramTileBridge = dyn->vramTileBridge[segmentId];
            if (vramTileBridge == 0)
                continue;  /* No VRAM bridge allocated -> skip */
            vramTile = vramTileBridge;
            if (dyn->cachedFillIndexBridge[segmentId] != decision.fillStripIndex)
            {
                needsUpload = 1;
                dyn->cachedFillIndexBridge[segmentId] = decision.fillStripIndex;
            }
            break;
        }

        case CELL_DECISION_STANDARD_FULL:
            vramTile = dyn->vramTileFullValue[segmentId];
            break;

        case CELL_DECISION_STANDARD_EMPTY:
            vramTile = dyn->vramTileEmpty[segmentId];
            break;

        case CELL_DECISION_STANDARD_TRAIL:
            vramTile = dyn->vramTileFullTrail[segmentId];
            break;

        case CELL_DECISION_PARTIAL_END:
            vramTile = dyn->vramTilePartialEnd;
            if (dyn->loadedSegmentPartialEnd != segmentId ||
                dyn->cachedFillIndexPartialEnd != decision.fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialEnd = segmentId;
                dyn->cachedFillIndexPartialEnd = decision.fillStripIndex;
            }
            break;

        case CELL_DECISION_PARTIAL_TRAIL:
            vramTile = dyn->vramTilePartialTrail;
            if (dyn->loadedSegmentPartialTrail != segmentId ||
                dyn->cachedFillIndexPartialTrail != decision.fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialTrail = segmentId;
                dyn->cachedFillIndexPartialTrail = decision.fillStripIndex;
            }
            break;

        case CELL_DECISION_PARTIAL_TRAIL2:
            vramTile = dyn->vramTilePartialTrailSecond;
            if (dyn->loadedSegmentPartialTrailSecond != segmentId ||
                dyn->cachedFillIndexPartialTrailSecond != decision.fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialTrailSecond = segmentId;
                dyn->cachedFillIndexPartialTrailSecond = decision.fillStripIndex;
            }
            break;

        case CELL_DECISION_PARTIAL_VALUE:
            vramTile = dyn->vramTilePartialValue;
            if (dyn->loadedSegmentPartialValue != segmentId ||
                dyn->cachedFillIndexPartialValue != decision.fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialValue = segmentId;
                dyn->cachedFillIndexPartialValue = decision.fillStripIndex;
            }
            break;
        }

        /* Upload partial tile if needed */
        if (needsUpload && decision.strip)
            upload_fill_tile(decision.strip, decision.fillStripIndex, vramTile, DMA);

        /* Update tilemap only if tile changed (change detection optimization) */
        if (dyn->cellCurrentTileIndex[cellIndex] != vramTile)
        {
            VDP_setTileMapXY(WINDOW, attrBase | vramTile,
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
 * 1. Cell fully covered by visible value -> PIP_STATE_VALUE
 * 2. Cell fully covered by visible trail -> PIP_STATE_LOSS / PIP_STATE_GAIN
 * 3. Hidden follow trail in blink-off    -> PIP_STATE_BLINK_OFF / PIP_STATE_EMPTY
 * 4. Default                             -> PIP_STATE_EMPTY
 *
 * @return One of PIP_STATE_* constants (0..4)
 */
static inline u8 select_pip_state_for_cell(const GaugeLaneLayout *layout,
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

    if (valuePxInTile == GAUGE_PIXELS_PER_TILE)
        return PIP_STATE_VALUE;

    if (trailRenderedPxInTile == GAUGE_PIXELS_PER_TILE)
    {
        if (trailMode == GAUGE_TRAIL_STATE_GAIN)
            return PIP_STATE_GAIN;
        if (trailMode == GAUGE_TRAIL_STATE_DAMAGE)
            return PIP_STATE_LOSS;
        return PIP_STATE_EMPTY;
    }

    u8 valueUnused;
    u8 trailActualPxInTile;
    compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsActual,
                          &valueUnused, &trailActualPxInTile);

    if (trailActualPxInTile != GAUGE_PIXELS_PER_TILE)
        return PIP_STATE_EMPTY;

    if (blinkOffActive)
    {
        if (trailMode == GAUGE_TRAIL_STATE_GAIN)
            return PIP_STATE_EMPTY;
        return PIP_STATE_BLINK_OFF;
    }
    
    return PIP_STATE_EMPTY;
}

static inline u8 resolve_available_pip_state(u8 requestedState,
                                             u8 stateCount,
                                             u8 trailMode)
{
    if (stateCount < 2)
        return PIP_STATE_EMPTY;

    /* Gain mode contract:
     * - if GAIN state is missing, render gain zone as VALUE (no blinking)
     * - never use BLINK_OFF for gain mode in PIP
     */
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
    {
        if (stateCount <= PIP_STATE_GAIN)
        {
            switch (requestedState)
            {
            case PIP_STATE_GAIN:
            case PIP_STATE_LOSS:
            case PIP_STATE_BLINK_OFF:
                return PIP_STATE_VALUE;
            default:
                break;
            }
        }
        else if (requestedState == PIP_STATE_BLINK_OFF)
        {
            return PIP_STATE_EMPTY;
        }
    }

    switch (requestedState)
    {
    case PIP_STATE_EMPTY:
        return PIP_STATE_EMPTY;
    case PIP_STATE_VALUE:
        return PIP_STATE_VALUE;
    case PIP_STATE_LOSS:
        return (stateCount > PIP_STATE_LOSS) ? PIP_STATE_LOSS : PIP_STATE_EMPTY;
    case PIP_STATE_GAIN:
        if (stateCount > PIP_STATE_GAIN)
            return PIP_STATE_GAIN;
        if (stateCount > PIP_STATE_LOSS)
            return PIP_STATE_LOSS;
        return PIP_STATE_VALUE;
    case PIP_STATE_BLINK_OFF:
        if (stateCount > PIP_STATE_BLINK_OFF)
            return PIP_STATE_BLINK_OFF;
        if (stateCount > PIP_STATE_LOSS)
            return PIP_STATE_LOSS;
        return PIP_STATE_EMPTY;
    default:
        return PIP_STATE_EMPTY;
    }
}

/**
 * PIP-mode renderer: update each cell's tile from compact strip based on pip state.
 *
 * For each cell, computes its PIP state (VALUE/EMPTY/LOSS/GAIN/BLINK_OFF),
 * then selects the corresponding tile from the compact strip.
 *
 * - Fixed mode: per-cell streaming (upload on demand).
 * - Dynamic mode: preloaded shared slots per segment/state/local tile;
 *   runtime mostly updates tilemap indices only.
 */
static void process_pip_mode(GaugeLaneInstance *lane,
                             u16 valuePixels,
                             u16 trailPixelsRendered,
                             u16 trailPixelsActual,
                             u8 blinkOffActive,
                             u8 trailMode)
{
    (void)valuePixels;
    (void)trailPixelsRendered;
    (void)trailPixelsActual;
    (void)blinkOffActive;
    (void)trailMode;
    const GaugeLaneLayout *layout = lane->layout;
    Gauge *gauge = s_activeGaugeForRender;
    if (!gauge)
        return;

    if (lane->vramMode == GAUGE_VRAM_DYNAMIC)
    {
        GaugeDynamic *dyn = &lane->dyn;

        for (u8 renderIndex = 0; renderIndex < layout->pipRenderCount; renderIndex++)
        {
            if (!dyn->cellValid[renderIndex])
                continue;

            const u8 fillIndex = layout->pipRenderFillIndexByRenderIndex[renderIndex];
            if (fillIndex == CACHE_INVALID_U8)
                continue;
            const u8 renderRow = layout->pipRenderRowByRenderIndex[renderIndex];
            const u8 sourceCol = layout->pipRenderSourceColByRenderIndex[renderIndex];
            const u8 sourceRow = layout->pipRenderSourceRowByRenderIndex[renderIndex];
            const u8 extraHFlip = layout->pipRenderExtraHFlipByRenderIndex[renderIndex];
            const u8 extraVFlip = layout->pipRenderExtraVFlipByRenderIndex[renderIndex];
            const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
            const u8 segmentId = layout->segmentIdByCell[cellIndex];
            const u16 pipBaseTile = dyn->vramTilePipBase[segmentId];
            const u8 pipStateCount = layout->pipStateCountBySegment[segmentId];
            if (pipStateCount < 2)
                continue;

            u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
            if (sourceWidth == 0)
                sourceWidth = 1;
            u8 sourceHeight = layout->pipSourceHeightBySegment[segmentId];
            if (sourceHeight == 0)
                sourceHeight = 1;

            const u8 mappedBaseLaneFillIndex = lane->baseLaneFillIndexByCell[cellIndex];
            const u8 requestedState = gauge->baseLanePipStateByFillIndex[mappedBaseLaneFillIndex];
            const u8 resolvedState = resolve_available_pip_state(requestedState,
                                                                 pipStateCount,
                                                                 trailMode);
            const u8 stripIndex = compute_pip_strip_index(sourceWidth,
                                                          pipStateCount,
                                                          resolvedState,
                                                          sourceRow,
                                                          sourceCol);
            const u16 desiredTile = (u16)(pipBaseTile +
                                          ((u16)resolvedState * (sourceWidth * sourceHeight)) +
                                          (sourceRow * sourceWidth) +
                                          sourceCol);
            const u8 changed = (dyn->cellCurrentTileIndex[renderIndex] != desiredTile);

            if (changed)
            {
                u16 x = 0;
                u16 y = 0;
                compute_pip_render_xy(layout, lane->originX, lane->originY, fillIndex, renderRow, &x, &y);
                const u16 attr = TILE_ATTR_FULL(layout->palette, layout->priority,
                                                (u8)(layout->verticalFlip ^ extraVFlip),
                                                (u8)(layout->horizontalFlip ^ extraHFlip),
                                                desiredTile);
                VDP_setTileMapXY(WINDOW, attr,
                                 x,
                                 y);
                dyn->cellCurrentTileIndex[renderIndex] = desiredTile;
            }

#if GAUGE_ENABLE_TRACE
            trace_emit_pip_cell_line(cellIndex,
                                     segmentId,
                                     renderIndex,
                                     1,
                                     layout->pipTilesetBySegment[segmentId],
                                     stripIndex,
                                     desiredTile,
                                     requestedState,
                                     resolvedState,
                                     pipStateCount,
                                     changed);
#endif
        }

        return;
    }

    for (u8 i = 0; i < lane->cellCount; i++)
    {
        GaugeStreamCell *cell = &lane->cells[i];
        const u8 cellIndex = cell->cellIndex;
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *pipStrip = layout->pipTilesetBySegment[segmentId];
        if (!pipStrip)
            continue;

        const u8 fillIndex = cell->pipFillIndex;
        if (fillIndex == CACHE_INVALID_U8)
            continue;
        const u8 renderIndex = cell->pipRenderIndex;
        if (renderIndex == CACHE_INVALID_U8)
            continue;

        const u8 sourceCol = layout->pipRenderSourceColByRenderIndex[renderIndex];
        const u8 sourceRow = layout->pipRenderSourceRowByRenderIndex[renderIndex];
        u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
        if (sourceWidth == 0)
            sourceWidth = 1;

        const u8 mappedBaseLaneFillIndex = lane->baseLaneFillIndexByCell[cellIndex];
        const u8 requestedState = gauge->baseLanePipStateByFillIndex[mappedBaseLaneFillIndex];
        const u8 stateCount = layout->pipStateCountBySegment[segmentId];
        const u8 pipState = resolve_available_pip_state(requestedState,
                                                        stateCount,
                                                        trailMode);
        const u8 stripIndex = compute_pip_strip_index(sourceWidth,
                                                       stateCount,
                                                       pipState,
                                                       sourceRow,
                                                       sourceCol);
        const u8 changed =
            (cell->cachedFillIndex != stripIndex) ||
            (cell->cachedStrip != pipStrip);

        upload_cell_if_needed(cell, pipStrip, stripIndex);

#if GAUGE_ENABLE_TRACE
        trace_emit_pip_cell_line(cellIndex,
                                 segmentId,
                                 renderIndex,
                                 0,
                                 pipStrip,
                                 stripIndex,
                                 cell->vramTileIndex,
                                 requestedState,
                                 pipState,
                                 stateCount,
                                 changed);
#endif
    }
}


/* =============================================================================
   Render/update dispatchers
   ============================================================================= */

static void render_lane_fill_dynamic(GaugeLaneInstance *lane,
                                     u16 valuePixels,
                                     u16 trailPixelsRendered,
                                     u16 trailPixelsActual,
                                     u8 blinkOffActive,
                                     u8 blinkOnChanged,
                                     u8 trailMode,
                                     u8 trailModeChanged)
{
    process_dynamic_mode(lane, valuePixels, trailPixelsRendered, trailPixelsActual,
                         blinkOffActive, blinkOnChanged, trailMode, trailModeChanged);
}

static void render_lane_fill_fixed(GaugeLaneInstance *lane,
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
    process_fixed_mode(lane, valuePixels, trailPixelsRendered, trailPixelsActual,
                       blinkOffActive, trailMode);
}

static void render_lane_pip(GaugeLaneInstance *lane,
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
    process_pip_mode(lane, valuePixels, trailPixelsRendered, trailPixelsActual,
                     blinkOffActive, trailMode);
}

/**
 * Select the correct render handler based on value mode and VRAM mode.
 *
 * Returns one of 3 handlers: fill_dynamic, fill_fixed, or pip (shared).
 */
static GaugeLaneRenderHandler *resolve_lane_render_handler(GaugeValueMode valueMode,
                                                           GaugeVramMode vramMode)
{
    if (valueMode == GAUGE_VALUE_MODE_PIP)
        return render_lane_pip;

    return (vramMode == GAUGE_VRAM_DYNAMIC) ? render_lane_fill_dynamic
                                            : render_lane_fill_fixed;
}

static void gauge_tick_and_render_fill(Gauge *gauge);
static void gauge_tick_and_render_pip(Gauge *gauge);
static void gauge_tick_and_render_bootstrap(Gauge *gauge);

/** Select the tick-and-render handler: fill (continuous) or pip (discrete). */
static GaugeTickAndRenderHandler *resolve_tick_and_render_handler(GaugeValueMode valueMode)
{
    return (valueMode == GAUGE_VALUE_MODE_PIP)
        ? gauge_tick_and_render_pip
        : gauge_tick_and_render_fill;
}


/* =============================================================================
   GaugeLaneInstance initialization (internal)
   ============================================================================= */

/**
 * Initialize a GaugeLaneInstance with all parameters.
 */
static u8 GaugeLaneInstance_initInternal(GaugeLaneInstance *lane,
                                 const Gauge *gauge,
                                 GaugeLaneLayout *layout,
                                 u16 originX, u16 originY,
                                 u16 vramBase,
                                 GaugeVramMode vramMode)
{
    if (!lane || !gauge || !layout || layout->length == 0)
        return 0;

    lane->originX = originX;
    lane->originY = originY;
    lane->vramBase = vramBase;
    lane->vramMode = vramMode;
    lane->renderHandler = resolve_lane_render_handler(gauge->valueMode, vramMode);
    lane->layout = layout;
    lane->cells = NULL;
    lane->cellCount = 0;
    lane->dyn.segmentCount = 0;
    lane->dyn.cellCount = 0;

    GaugeLaneLayout_retain(layout);

    if (vramMode == GAUGE_VRAM_FIXED)
    {
        u8 streamCellCapacity = layout->length;
        if (gauge->valueMode == GAUGE_VALUE_MODE_PIP && layout->pipRenderCount > streamCellCapacity)
            streamCellCapacity = layout->pipRenderCount;

        lane->cells = (GaugeStreamCell *)gauge_alloc_bytes(
            (u16)(streamCellCapacity * (u8)sizeof(GaugeStreamCell)));
        if (!lane->cells)
        {
            GaugeLaneLayout_release(layout);
            return 0;
        }
    }

    if (gauge->valueMode == GAUGE_VALUE_MODE_PIP)
    {
        if (lane->vramMode == GAUGE_VRAM_DYNAMIC)
        {
            if (!init_dynamic_pip_vram(lane))
            {
                gauge_free_ptr((void **)&lane->cells);
                GaugeLaneLayout_release(layout);
                return 0;
            }
        }
        else
        {
            write_tilemap_pip_init(lane);
        }
        return 1;
    }

    /* Initialize based on VRAM mode */
    if (lane->vramMode == GAUGE_VRAM_DYNAMIC)
    {
        /* Dynamic mode initialization */
        if (!init_dynamic_vram(&lane->dyn, lane->layout, lane->vramBase, gauge->logic.trailEnabled))
        {
            gauge_free_ptr((void **)&lane->cells);
            GaugeLaneLayout_release(layout);
            return 0;
        }
        preload_dynamic_standard_tiles(&lane->dyn, lane->layout, gauge->logic.trailEnabled);
        init_dynamic_tilemap(lane);
    }
    else
    {
        /* Fixed mode */
        write_tilemap_fixed_init(lane);
    }

    return 1;
}

static void GaugeLaneInstance_releaseInternal(GaugeLaneInstance *lane)
{
    if (!lane)
        return;

    if (lane->layout)
    {
        GaugeLaneLayout_release((GaugeLaneLayout *)lane->layout);
        lane->layout = NULL;
    }

    gauge_free_ptr((void **)&lane->cells);
    free_dynamic_buffers(&lane->dyn);
    lane->cellCount = 0;
}

static u8 ensure_lane_capacity(Gauge *gauge, u8 requiredCount)
{
    if (requiredCount <= gauge->laneCapacity)
        return 1;

    u8 newCapacity = (gauge->laneCapacity == 0) ? 2 : gauge->laneCapacity;
    while (newCapacity < requiredCount && newCapacity < GAUGE_MAX_LANES)
        newCapacity = (u8)(newCapacity << 1);
    if (newCapacity > GAUGE_MAX_LANES)
        newCapacity = GAUGE_MAX_LANES;
    if (newCapacity < requiredCount)
        return 0;

    GaugeLaneInstance **newLanes = (GaugeLaneInstance **)gauge_alloc_bytes(
        (u16)(newCapacity * (u8)sizeof(GaugeLaneInstance *)));
    if (!newLanes)
        return 0;

    for (u8 i = 0; i < gauge->laneCount; i++)
        newLanes[i] = gauge->lanes[i];

    gauge_free_ptr((void **)&gauge->lanes);
    gauge->lanes = newLanes;
    gauge->laneCapacity = newCapacity;
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
static u16 compute_pip_total_pixels(const GaugeLaneLayout *layout)
{
    u16 totalPixels = 0;
    for (u8 pipIndex = 0; pipIndex < layout->pipCount; pipIndex++)
    {
        totalPixels = (u16)(totalPixels + ((u16)layout->pipWidthByPipIndex[pipIndex] << TILE_TO_PIXEL_SHIFT));
    }
    return totalPixels;
}

/**
 * Validate PIP layout configuration (called once on first Gauge_addLane).
 *
 * Checks: pipCount > 0, all segments have compact tilesets, pipIndex mapping
 * is valid, pip widths match segment styles, maxValue == pipCount, and
 * total pixel span matches maxFillPixels.
 *
 * @return 1 if valid, 0 if any check fails
 */
static u8 validate_pip_layout(const GaugeLogic *logic, const GaugeLaneLayout *layout)
{
    if (layout->pipCount == 0)
        return 0;

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        if (layout->pipTilesetBySegment[segmentId] == NULL)
            return 0;
        if (layout->pipStateCountBySegment[segmentId] < 2)
            return 0;
        u8 pipHeight = layout->pipHeightBySegment[segmentId];
        if (pipHeight == 0)
            pipHeight = 1;
        if (pipHeight > 4)
            return 0;
    }

    for (u8 fillIndex = 0; fillIndex < layout->length; fillIndex++)
    {
        const u8 pipIndex = layout->pipIndexByFillIndex[fillIndex];
        const u8 localTile = layout->pipLocalTileByFillIndex[fillIndex];

        if (pipIndex == CACHE_INVALID_U8)
            return 0;

        if (localTile == 0)
        {
            const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
            const u8 segmentId = layout->segmentIdByCell[cellIndex];
            u8 styleWidth = layout->pipWidthBySegment[segmentId];
            if (styleWidth == 0)
                styleWidth = 1;

            if (layout->pipWidthByPipIndex[pipIndex] != styleWidth)
                return 0;
        }
    }

    if (logic->maxValue != layout->pipCount)
        return 0;

    const u16 totalPixels = compute_pip_total_pixels(layout);
    if (totalPixels != logic->maxFillPixels)
        return 0;

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
static void fill_pip_value_lut(u16 *dest, u16 maxValue, const GaugeLaneLayout *layout)
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

/* Internal init payload kept private (public API uses GaugeDefinition). */
typedef struct
{
    u16 maxValue;
    u16 initialValue;
    const GaugeLaneLayout *layout;
    u16 vramBase;
    GaugeVramMode vramMode;
    GaugeValueMode valueMode;
} GaugeInit;

/**
 * Initialize a Gauge from a GaugeInit config.
 *
 * Sets up the embedded GaugeLogic. maxFillPixels is derived from
 * init->layout->length * GAUGE_PIXELS_PER_TILE.
 *
 * If maxValue != maxFillPixels (FILL mode) or PIP mode, a value-to-pixels
 * LUT is built into gauge->logic.valueToPixelsData (heap allocated).
 *
 * Trail mode defaults to disabled, gain mode defaults to disabled,
 * and value animation defaults to instant.
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
        maxValue = GAUGE_LUT_CAPACITY;

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
            return;
        fill_value_to_pixels_lut(gauge->logic.valueToPixelsData, maxValue, maxFillPixels);
        lut = gauge->logic.valueToPixelsData;
    }
    else if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        u16 validatedPipCount = init->layout->pipCount;
        if (validatedPipCount > GAUGE_LUT_CAPACITY)
            validatedPipCount = GAUGE_LUT_CAPACITY;
        if (maxValue != validatedPipCount)
            maxValue = validatedPipCount;

        gauge->logic.valueToPixelsData = (u16 *)gauge_alloc_bytes(
            (u16)((maxValue + 1) * (u8)sizeof(u16)));
        if (!gauge->logic.valueToPixelsData)
            return;

        fill_pip_value_lut(gauge->logic.valueToPixelsData, maxValue, init->layout);
        lut = gauge->logic.valueToPixelsData;
    }

    /* Initialize logic with the correct LUT pointer.
     * Trail mode defaults to disabled and gain mode defaults to disabled. */
    GaugeLogic_init(&gauge->logic, maxValue, maxFillPixels, lut,
                    init->initialValue);

    /* Initialize lane storage (grown on demand by Gauge_addLane). */
    gauge->lanes = NULL;
    gauge->laneCount = 0;
    gauge->laneCapacity = 0;

    /* Runtime dispatch state */
    gauge->valueMode = valueMode;
    gauge->steadyTickAndRenderHandler = resolve_tick_and_render_handler(valueMode);
    gauge->tickAndRenderHandler = gauge_tick_and_render_bootstrap;
    set_mode_flags(&gauge->logic, (GaugeTrailMode)gauge->logic.configuredTrailMode);
    gauge->logic.trailEnabled = compute_trail_enabled(
        (GaugeTrailMode)gauge->logic.configuredTrailMode,
        (GaugeGainMode)gauge->logic.configuredGainMode);
    gauge->logicTickHandler = resolve_logic_tick_handler(
        (GaugeTrailMode)gauge->logic.configuredTrailMode);
    gauge->runtimeLocked = GAUGE_RUNTIME_OPEN;
    gauge->baseLaneIndex = 0;
    gauge->baseLaneSpanPixels = maxFillPixels;
    gauge->baseLaneHasBridge = 0;

    /* VRAM allocation state */
    gauge->vramBase = init->vramBase;
    gauge->vramNextOffset = 0;
    gauge->vramMode = init->vramMode;
}

/** Configure value animation (0=instant changes, 1=animated transitions).
 * No-op after runtime is CLOSED (first Gauge_update already happened). */
void Gauge_setValueAnim(Gauge *gauge, u8 enabled, u8 shift)
{
    if (!gauge || gauge->runtimeLocked == GAUGE_RUNTIME_CLOSED)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->valueAnimEnabled = enabled ? 1 : 0;
    logic->valueAnimShift = (shift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : shift;
}

/** Configure damage trail mode and timing (configuration-time only, before runtime CLOSE). */
void Gauge_setTrailMode(Gauge *gauge,
                        GaugeTrailMode mode,
                        u16 criticalValue,
                        u8 shift,
                        u8 blinkShift)
{
    if (!gauge || gauge->runtimeLocked == GAUGE_RUNTIME_CLOSED)
        return;

    if (mode > GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK)
        mode = GAUGE_TRAIL_MODE_FOLLOW;

    GaugeLogic *logic = &gauge->logic;
    logic->configuredTrailMode = (u8)mode;
    logic->trailEnabled = compute_trail_enabled(
        mode, (GaugeGainMode)logic->configuredGainMode);
    logic->criticalValue = (criticalValue > logic->maxValue) ? logic->maxValue : criticalValue;
    logic->trailAnimShift = (shift == 0) ? GAUGE_DEFAULT_TRAIL_ANIM_SHIFT : shift;
    logic->blinkShift = (blinkShift == 0) ? GAUGE_DEFAULT_BLINK_SHIFT : blinkShift;
    set_mode_flags(logic, mode);
    gauge->logicTickHandler = resolve_logic_tick_handler(mode);

    apply_configured_trail_mode_state(logic, 0);
    invalidate_render_cache(logic);
}

/** Configure gain mode and timing (configuration-time only, before runtime CLOSE). */
void Gauge_setGainMode(Gauge *gauge,
                       GaugeGainMode mode,
                       u8 shift,
                       u8 blinkShift)
{
    if (!gauge || gauge->runtimeLocked == GAUGE_RUNTIME_CLOSED)
        return;

    if (mode > GAUGE_GAIN_MODE_RESERVED_2)
        mode = GAUGE_GAIN_MODE_DISABLED;

    GaugeLogic *logic = &gauge->logic;
    logic->configuredGainMode = (u8)mode;
    logic->trailEnabled = compute_trail_enabled(
        (GaugeTrailMode)logic->configuredTrailMode, mode);
    logic->gainAnimShift = (shift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : shift;
    logic->gainBlinkShift = (blinkShift == 0) ? GAUGE_DEFAULT_BLINK_SHIFT : blinkShift;

    apply_configured_trail_mode_state(logic, 0);
    invalidate_render_cache(logic);
}

static void set_max_fill_pixels_internal(Gauge *gauge, u16 maxFillPixels)
{
    GaugeLogic *logic = &gauge->logic;

    if (logic->maxFillPixels == maxFillPixels)
        return;

    logic->maxFillPixels = maxFillPixels;

    if (gauge->valueMode == GAUGE_VALUE_MODE_FILL && logic->maxValue != maxFillPixels)
    {
        if (!logic->valueToPixelsData)
        {
            logic->valueToPixelsData = (u16 *)gauge_alloc_bytes(
                (u16)((logic->maxValue + 1) * (u8)sizeof(u16)));
            if (!logic->valueToPixelsData)
                return;
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
        logic->valueToPixelsLUT = NULL;
    }

    logic->valueTargetPixels = value_to_pixels(logic, logic->currentValue);
    if (logic->valueTargetPixels > maxFillPixels)
        logic->valueTargetPixels = maxFillPixels;
    logic->valuePixels = logic->valueTargetPixels;
    apply_configured_trail_mode_state(logic, 0);
    invalidate_render_cache(logic);
}

static inline u16 gauge_get_lane_span_pixels(const GaugeLaneInstance *lane)
{
    if (!lane || !lane->layout)
        return 0;
    return (u16)(lane->layout->length * GAUGE_PIXELS_PER_TILE);
}

static inline u8 gauge_layout_has_bridge(const GaugeLaneLayout *layout)
{
    if (!layout || layout->segmentCount == 0)
        return 0;

    return (segment_tileset_array_has_any(layout->tilesetBridgeBySegment, layout->segmentCount) ||
            segment_tileset_array_has_any(layout->gainTilesetBridgeBySegment, layout->segmentCount) ||
            segment_tileset_array_has_any(layout->blinkOffTilesetBridgeBySegment, layout->segmentCount) ||
            segment_tileset_array_has_any(layout->gainBlinkOffTilesetBridgeBySegment, layout->segmentCount))
        ? 1 : 0;
}

static void gauge_sync_pip_baseLane_logic(Gauge *gauge)
{
    GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
    GaugeLogic *logic = &gauge->logic;
    if (!baseLane || !baseLane->layout)
        return;

    const GaugeLaneLayout *baseLaneLayout = baseLane->layout;
    u16 baseLanePipCount = baseLaneLayout->pipCount;
    if (baseLanePipCount == 0)
        return;
    if (baseLanePipCount > GAUGE_LUT_CAPACITY)
        baseLanePipCount = GAUGE_LUT_CAPACITY;

    u16 *newValueToPixels = (u16 *)gauge_alloc_bytes(
        (u16)((baseLanePipCount + 1) * (u8)sizeof(u16)));
    if (!newValueToPixels)
    {
        logic->valueToPixelsLUT = logic->valueToPixelsData;
        return;
    }

    fill_pip_value_lut(newValueToPixels, baseLanePipCount, baseLaneLayout);
    {
        u16 *oldValueToPixels = logic->valueToPixelsData;
        logic->valueToPixelsData = newValueToPixels;
        logic->valueToPixelsLUT = newValueToPixels;
        if (oldValueToPixels)
            MEM_free(oldValueToPixels);
    }
    logic->maxValue = baseLanePipCount;
    logic->maxFillPixels = compute_pip_total_pixels(baseLaneLayout);

    if (logic->currentValue > logic->maxValue)
        logic->currentValue = logic->maxValue;

    logic->valueTargetPixels = value_to_pixels(logic, logic->currentValue);
    if (logic->valueTargetPixels > logic->maxFillPixels)
        logic->valueTargetPixels = logic->maxFillPixels;
    if (logic->valuePixels > logic->maxFillPixels)
        logic->valuePixels = logic->maxFillPixels;
    if (logic->trailPixels > logic->maxFillPixels)
        logic->trailPixels = logic->maxFillPixels;

    apply_configured_trail_mode_state(logic, 0);
    invalidate_render_cache(logic);
}

static void gauge_select_baseLane(Gauge *gauge)
{
    if (!gauge || gauge->laneCount == 0)
    {
        if (gauge)
        {
            gauge->baseLaneIndex = 0;
            gauge->baseLaneSpanPixels = 0;
            gauge->baseLaneHasBridge = 0;
        }
        return;
    }

    u8 bestIndex = gauge->baseLaneIndex;
    u16 bestSpan = 0;
    if (bestIndex < gauge->laneCount)
        bestSpan = gauge_get_lane_span_pixels(gauge->lanes[bestIndex]);

    for (u8 i = 0; i < gauge->laneCount; i++)
    {
        const u16 span = gauge_get_lane_span_pixels(gauge->lanes[i]);
        if (span > bestSpan)
        {
            bestSpan = span;
            bestIndex = i;
        }
    }

    gauge->baseLaneIndex = bestIndex;
    gauge->baseLaneSpanPixels = bestSpan;
}

static void gauge_rebuild_linkedLane_projection_maps(Gauge *gauge)
{
    GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
    if (!gauge || !baseLane || !baseLane->layout)
        return;

    const GaugeLaneLayout *baseLaneLayout = baseLane->layout;
    for (u8 laneIndex = 0; laneIndex < gauge->laneCount; laneIndex++)
    {
        GaugeLaneInstance *lane = gauge->lanes[laneIndex];
        if (!lane || !lane->layout)
            continue;

        const GaugeLaneLayout *layout = lane->layout;
        for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
        {
            const u8 localFillIndex = layout->fillIndexByCell[cellIndex];
            const u16 samplePixel = (u16)(layout->fillOffset +
                                          FILL_IDX_TO_OFFSET(localFillIndex) + 4);
            u8 dummyPx;
            u8 mappedFillIndex = compute_fill_index_and_px(baseLaneLayout, samplePixel, &dummyPx);
            if (mappedFillIndex >= baseLaneLayout->length)
                mappedFillIndex = (u8)(baseLaneLayout->length - 1);
            lane->baseLaneFillIndexByCell[cellIndex] = mappedFillIndex;
        }

        for (u8 cellIndex = layout->length; cellIndex < GAUGE_MAX_LENGTH; cellIndex++)
            lane->baseLaneFillIndexByCell[cellIndex] = 0;
    }
}

static void gauge_apply_baseLane_topology_state(Gauge *gauge)
{
    if (!gauge || gauge->laneCount == 0)
    {
        if (gauge)
        {
            gauge->baseLaneIndex = 0;
            gauge->baseLaneSpanPixels = 0;
            gauge->baseLaneHasBridge = 0;
        }
        return;
    }

    const u8 previousBaseLane = gauge->baseLaneIndex;
    const u16 previousSpan = gauge->baseLaneSpanPixels;

    gauge_select_baseLane(gauge);
    gauge_rebuild_linkedLane_projection_maps(gauge);
    {
        GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
        gauge->baseLaneHasBridge = (baseLane && baseLane->layout)
            ? gauge_layout_has_bridge(baseLane->layout)
            : 0;
    }

    if (gauge->valueMode == GAUGE_VALUE_MODE_FILL)
        set_max_fill_pixels_internal(gauge, gauge->baseLaneSpanPixels);
    else
        gauge_sync_pip_baseLane_logic(gauge);

    if (previousBaseLane != gauge->baseLaneIndex || previousSpan != gauge->baseLaneSpanPixels)
        invalidate_render_cache(&gauge->logic);
}

/** Override maxFillPixels (no-op if unchanged, before runtime CLOSE). */
void Gauge_setMaxFillPixels(Gauge *gauge, u16 maxFillPixels)
{
    if (!gauge || gauge->runtimeLocked == GAUGE_RUNTIME_CLOSED) return;
    set_max_fill_pixels_internal(gauge, maxFillPixels);
}

u8 Gauge_addLane(Gauge *gauge,
                 GaugeLaneLayout *layout,
                 u16 originX,
                 u16 originY)
{
    if (!gauge || !layout || gauge->runtimeLocked == GAUGE_RUNTIME_CLOSED)
        return 0;

    const u16 vramSize = compute_vram_size_for_layout(layout,
                                                      gauge->vramMode,
                                                      gauge->logic.trailEnabled,
                                                      gauge->valueMode);
    const u16 vramBase = (u16)(gauge->vramBase + gauge->vramNextOffset);

    if (!Gauge_addLaneEx(gauge, layout, originX, originY, vramBase, gauge->vramMode))
        return 0;

    gauge->vramNextOffset = (u16)(gauge->vramNextOffset + vramSize);
    return 1;
}

u8 Gauge_addLaneEx(Gauge *gauge,
                   GaugeLaneLayout *layout,
                   u16 originX,
                   u16 originY,
                   u16 vramBase,
                   GaugeVramMode vramMode)
{
    if (!gauge || !layout || gauge->runtimeLocked == GAUGE_RUNTIME_CLOSED)
        return 0;

    if (gauge->laneCount >= GAUGE_MAX_LANES)
        return 0;

    if (!ensure_lane_capacity(gauge, (u8)(gauge->laneCount + 1)))
        return 0;

    GaugeLaneInstance *lane = (GaugeLaneInstance *)gauge_alloc_bytes((u16)sizeof(GaugeLaneInstance));
    if (!lane)
        return 0;

    if (!GaugeLaneInstance_initInternal(lane, gauge, layout, originX, originY, vramBase, vramMode))
    {
        gauge_free_ptr((void **)&lane);
        return 0;
    }

    const u8 newLaneIndex = gauge->laneCount;
    gauge->lanes[newLaneIndex] = lane;
    gauge->laneCount = (u8)(newLaneIndex + 1);
    gauge->runtimeLocked = GAUGE_RUNTIME_HAS_LANES;

    gauge_apply_baseLane_topology_state(gauge);

    if (gauge->valueMode == GAUGE_VALUE_MODE_PIP)
    {
        GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
        if (!baseLane || !validate_pip_layout(&gauge->logic, baseLane->layout))
        {
            GaugeLaneInstance_releaseInternal(lane);
            gauge_free_ptr((void **)&lane);
            gauge->lanes[newLaneIndex] = NULL;
            gauge->laneCount = newLaneIndex;

            if (gauge->laneCount == 0)
            {
                gauge->baseLaneIndex = 0;
                gauge->baseLaneSpanPixels = 0;
                gauge->baseLaneHasBridge = 0;
            }
            else
            {
                gauge_apply_baseLane_topology_state(gauge);
            }
            return 0;
        }
    }

    /* Force next update to render */
    invalidate_render_cache(&gauge->logic);

    return 1;
}

void Gauge_release(Gauge *gauge)
{
    if (!gauge)
        return;

    if (s_activeGaugeForRender == gauge)
        s_activeGaugeForRender = NULL;

    for (u8 i = 0; i < gauge->laneCount; i++)
    {
        GaugeLaneInstance *lane = (gauge->lanes != NULL) ? gauge->lanes[i] : NULL;
        if (!lane)
            continue;

        GaugeLaneInstance_releaseInternal(lane);
        gauge_free_ptr((void **)&lane);
        gauge->lanes[i] = NULL;
    }

    gauge_free_ptr((void **)&gauge->lanes);
    gauge_free_ptr((void **)&gauge->logic.valueToPixelsData);

    for (u8 layoutIndex = 0; layoutIndex < gauge->ownedLayoutCount; layoutIndex++)
    {
        GaugeLaneLayout *ownedLayout = gauge->ownedLayouts[layoutIndex];
        gauge_free_ptr((void **)&ownedLayout);
        gauge->ownedLayouts[layoutIndex] = NULL;
    }
    gauge->ownedLayoutCount = 0;

    memset(gauge, 0, sizeof(*gauge));
}

/* =============================================================================
   Direct GaugeDefinition build helpers
   ============================================================================= */

static inline u8 sanitize_value_mode(u8 mode)
{
    return (mode == GAUGE_VALUE_MODE_PIP) ? GAUGE_VALUE_MODE_PIP : GAUGE_VALUE_MODE_FILL;
}

static inline u8 sanitize_orientation(u8 orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? GAUGE_ORIENT_VERTICAL : GAUGE_ORIENT_HORIZONTAL;
}

static inline u8 sanitize_fill_direction(u8 fillDirection)
{
    return (fillDirection == GAUGE_FILL_REVERSE) ? GAUGE_FILL_REVERSE : GAUGE_FILL_FORWARD;
}

static inline u8 sanitize_palette_index(u8 palette)
{
    return (u8)(palette & 3);
}


static u8 definition_lane_is_empty(const GaugeLane *lane)
{
    if (!lane)
        return 1;

    return (lane->segments[0].cells == 0 || lane->segments[0].skin == NULL) ? 1 : 0;
}

static u8 compute_segment_display_cells(GaugeMode mode,
                                        const GaugeSegment *segment,
                                        u16 *outDisplayCells)
{
    if (!segment || !segment->skin || !outDisplayCells || segment->cells == 0)
        return 0;

    if (mode == GAUGE_MODE_PIP)
    {
        const GaugePipSkin *pip = &segment->skin->pip;
        const u8 pipWidth = pip->pipWidth ? pip->pipWidth : 1;
        *outDisplayCells = (u16)(segment->cells * pipWidth);
    }
    else
    {
        *outDisplayCells = segment->cells;
    }

    return 1;
}

static u16 compute_lane_display_cells(const GaugeDefinition *definition,
                                      const GaugeLane *lane)
{
    if (!definition || definition_lane_is_empty(lane))
        return 0;

    u16 totalCells = 0;
    for (u8 segmentIndex = 0; segmentIndex < GAUGE_MAX_SEGMENTS; segmentIndex++)
    {
        const GaugeSegment *segment = &lane->segments[segmentIndex];
        if (segment->cells == 0 || !segment->skin)
            break;

        u16 displayCells = 0;
        if (!compute_segment_display_cells(definition->mode, segment, &displayCells))
            return 0;

        totalCells = (u16)(totalCells + displayCells);
    }

    return totalCells;
}

static s16 compute_lane_origin_axis(u16 base, s8 offset)
{
    return (s16)base + (s16)offset;
}

static u8 find_baseLane_index(const GaugeDefinition *definition,
                              u8 *outLaneIndex)
{
    if (!definition || !outLaneIndex)
        return 0;

    u16 bestSpan = 0;
    u8 bestIndex = 0xFF;

    for (u8 laneIndex = 0; laneIndex < GAUGE_MAX_LANES; laneIndex++)
    {
        const GaugeLane *lane = &definition->lanes[laneIndex];
        if (definition_lane_is_empty(lane))
            continue;

        const u16 span = compute_lane_display_cells(definition, lane);
        if (span > bestSpan || bestIndex == 0xFF)
        {
            bestSpan = span;
            bestIndex = laneIndex;
        }
    }

    if (bestIndex == 0xFF)
        return 0;

    *outLaneIndex = bestIndex;
    return 1;
}

static u16 compute_fill_offset_pixels_for_lane(const GaugeDefinition *definition,
                                               const GaugeLane *baseLane,
                                               u8 firstValueCell)
{
    if (!definition || !baseLane || firstValueCell == 0)
        return 0;

    u8 remainingCells = firstValueCell;
    u16 displayCellsBefore = 0;

    for (u8 segmentIndex = 0; segmentIndex < GAUGE_MAX_SEGMENTS; segmentIndex++)
    {
        const GaugeSegment *segment = &baseLane->segments[segmentIndex];
        if (segment->cells == 0 || !segment->skin)
            break;

        if (remainingCells == 0)
            break;

        if (remainingCells >= segment->cells)
        {
            u16 displayCells = 0;
            if (!compute_segment_display_cells(definition->mode, segment, &displayCells))
                return 0;
            displayCellsBefore = (u16)(displayCellsBefore + displayCells);
            remainingCells = (u8)(remainingCells - segment->cells);
        }
        else
        {
            if (definition->mode == GAUGE_MODE_PIP)
            {
                const u8 pipWidth = segment->skin->pip.pipWidth ? segment->skin->pip.pipWidth : 1;
                displayCellsBefore = (u16)(displayCellsBefore + ((u16)remainingCells * pipWidth));
            }
            else
            {
                displayCellsBefore = (u16)(displayCellsBefore + remainingCells);
            }
            remainingCells = 0;
            break;
        }
    }

    return (u16)(displayCellsBefore << TILE_TO_PIXEL_SHIFT);
}

static u8 resolve_lane_palette(const GaugeDefinition *definition,
                               const GaugeLane *lane)
{
    if (!definition || !lane)
        return 0;

    if (lane->overridePalette)
        return sanitize_palette_index(lane->palette);

    return sanitize_palette_index(definition->palette);
}

typedef struct
{
    const GaugeLane *lane;
    u8 laneIndex;
    u16 originX;
    u16 originY;
    u16 fillOffset;
    u8 palette;
    u8 length;
    u8 segmentCount;
    u8 segmentIdByCell[GAUGE_MAX_LENGTH];
} GaugeBuildLanePlan;

typedef struct
{
    GaugeDefinition sanitizedDefinition;
    GaugeBuildLanePlan lanePlans[GAUGE_MAX_LANES];
    GaugeLaneLayout *builtLayouts[GAUGE_MAX_LANES];
} GaugeBuildScratch;

static GaugeBuildScratch s_buildScratch;

static inline u8 sanitize_vram_mode(u8 vramMode)
{
    return (vramMode == GAUGE_VRAM_FIXED) ? GAUGE_VRAM_FIXED : GAUGE_VRAM_DYNAMIC;
}

static u8 resolve_pip_strip_geometry_from_skin(const GaugePipSkin *skin,
                                               u8 *outStateCount,
                                               u8 *outSourceWidth,
                                               u8 *outSourceHeight,
                                               u8 *outHalfAxis)
{
    if (!skin || !skin->tileset)
        return 0;

    const u8 pipWidth = skin->pipWidth ? skin->pipWidth : 1;
    u8 pipHeight = skin->pipHeight ? skin->pipHeight : 1;
    if (pipHeight == 0 || pipHeight > 4)
        return 0;

    const u16 pipTileCount = skin->tileset->numTile;
    if (pipTileCount == 0 || pipTileCount > 255)
        return 0;

    const u8 stripCoverage = skin->coverage;
    if (stripCoverage > GAUGE_STRIP_COVERAGE_QUARTER)
        return 0;

    u8 sourceWidth = pipWidth;
    u8 sourceHeight = pipHeight;
    u8 halfAxis = PIP_HALF_AXIS_HORIZONTAL;
    u16 tilesPerState = 0;

    if (stripCoverage == GAUGE_STRIP_COVERAGE_HALF)
    {
        if ((pipWidth & 1) || (pipHeight & 1))
            return 0;

        const u16 horizontalTilesPerState = (u16)pipWidth * (u16)(pipHeight >> 1);
        const u16 verticalTilesPerState = (u16)(pipWidth >> 1) * pipHeight;
        const u8 horizontalValid = (horizontalTilesPerState != 0) && ((pipTileCount % horizontalTilesPerState) == 0);
        const u8 verticalValid = (verticalTilesPerState != 0) && ((pipTileCount % verticalTilesPerState) == 0);

        if (!horizontalValid && !verticalValid)
            return 0;

        if (horizontalValid)
        {
            sourceWidth = pipWidth;
            sourceHeight = (u8)(pipHeight >> 1);
            halfAxis = PIP_HALF_AXIS_HORIZONTAL;
            tilesPerState = horizontalTilesPerState;
        }
        else
        {
            sourceWidth = (u8)(pipWidth >> 1);
            sourceHeight = pipHeight;
            halfAxis = PIP_HALF_AXIS_VERTICAL;
            tilesPerState = verticalTilesPerState;
        }
    }
    else if (stripCoverage == GAUGE_STRIP_COVERAGE_QUARTER)
    {
        if ((pipWidth & 1) || (pipHeight & 1))
            return 0;

        sourceWidth = (u8)(pipWidth >> 1);
        sourceHeight = (u8)(pipHeight >> 1);
        tilesPerState = (u16)sourceWidth * sourceHeight;
    }
    else
    {
        tilesPerState = (u16)pipWidth * pipHeight;
    }

    if (tilesPerState == 0)
        return 0;
    if ((pipTileCount % tilesPerState) != 0)
        return 0;

    const u16 stateCount = (u16)(pipTileCount / tilesPerState);
    if (stateCount < 2 || stateCount > 255)
        return 0;

    if (outStateCount)
        *outStateCount = (u8)stateCount;
    if (outSourceWidth)
        *outSourceWidth = sourceWidth;
    if (outSourceHeight)
        *outSourceHeight = sourceHeight;
    if (outHalfAxis)
        *outHalfAxis = halfAxis;
    return 1;
}

static void release_built_layout(GaugeLaneLayout **layoutPtr)
{
    if (!layoutPtr || !*layoutPtr)
        return;

    GaugeLaneLayout *layout = *layoutPtr;
    if (layout->length != 0 || layout->refCount != 0)
        GaugeLaneLayout_release(layout);
    gauge_free_ptr((void **)layoutPtr);
}

static void release_built_layouts(GaugeLaneLayout **builtLayouts)
{
    if (!builtLayouts)
        return;

    for (u8 laneIndex = 0; laneIndex < GAUGE_MAX_LANES; laneIndex++)
        release_built_layout(&builtLayouts[laneIndex]);
}

static u8 build_segment_id_by_cell_from_lane(GaugeMode mode,
                                             GaugeFillDirection fillDirection,
                                             const GaugeLane *lane,
                                             u8 *outSegmentIdByCell,
                                             u8 *outLength,
                                             u8 *outSegmentCount)
{
    if (!lane || !outSegmentIdByCell || !outLength || !outSegmentCount)
        return 0;

    u8 segmentByFillIndex[GAUGE_MAX_LENGTH] = {0};
    u8 cursor = 0;
    u8 segmentCount = 0;

    for (u8 segmentIndex = 0; segmentIndex < GAUGE_MAX_SEGMENTS; segmentIndex++)
    {
        const GaugeSegment *segment = &lane->segments[segmentIndex];
        if (segment->cells == 0 || !segment->skin)
            break;

        u16 expandedCellCount = segment->cells;
        if (mode == GAUGE_MODE_PIP)
        {
            const GaugePipSkin *pip = &segment->skin->pip;
            if (!pip->tileset)
                return 0;
            expandedCellCount = (u16)(segment->cells * (pip->pipWidth ? pip->pipWidth : 1));
        }

        if ((u16)cursor + expandedCellCount > GAUGE_MAX_LENGTH)
            return 0;

        for (u16 i = 0; i < expandedCellCount; i++)
            segmentByFillIndex[cursor++] = segmentCount;

        segmentCount++;
    }

    if (cursor == 0 || segmentCount == 0)
        return 0;

    *outLength = cursor;
    *outSegmentCount = segmentCount;

    for (u8 cellIndex = 0; cellIndex < cursor; cellIndex++)
    {
        const u8 fillIndex = (fillDirection == GAUGE_FILL_REVERSE)
            ? (u8)(cursor - 1 - cellIndex)
            : cellIndex;
        outSegmentIdByCell[cellIndex] = segmentByFillIndex[fillIndex];
    }

    return 1;
}

static inline const u32 *tileset_asset_to_rom(const TileSet *tileset)
{
    return tileset ? tileset->tiles : NULL;
}

static void scan_fill_assets_usage(const GaugeFillAssets *assets,
                                   u8 fixedStartCap,
                                   u8 fixedEndCap,
                                   SkinSetUsageFlags *usageFlags)
{
    if (!usageFlags)
        return;

    const u32 *body = assets ? tileset_asset_to_rom(assets->body) : NULL;
    const u32 *end = assets ? tileset_asset_to_rom(assets->end) : NULL;
    const u32 *trail = assets ? tileset_asset_to_rom(assets->trail) : NULL;
    const u32 *bridge = assets ? tileset_asset_to_rom(assets->bridge) : NULL;
    const u32 *capStart = fixedStartCap ? (end ? end : body) : NULL;
    const u32 *capStartBreak = fixedStartCap ? body : NULL;
    const u32 *capStartTrail = fixedStartCap ? trail : NULL;
    const u32 *capEnd = fixedEndCap ? (end ? end : body) : NULL;

    usageFlags->body |= (body != NULL);
    usageFlags->end |= (end != NULL);
    usageFlags->trail |= (trail != NULL);
    usageFlags->bridge |= (bridge != NULL);
    usageFlags->capStart |= (capStart != NULL);
    usageFlags->capEnd |= (capEnd != NULL);
    usageFlags->capStartBreak |= (capStartBreak != NULL);
    usageFlags->capStartTrail |= (capStartTrail != NULL);
}

static void assign_layout_context_slot(const LayoutContextTargetView *targetView,
                                       LayoutTilesetSlot slot,
                                       u8 segmentId,
                                       const u32 *tileset)
{
    if (!targetView)
        return;

    const u32 ***targetField = targetView->targetBySlot[slot];
    if (!targetField || !*targetField)
        return;

    if (*targetField == s_nullSegmentTilesets && slot != LAYOUT_TILESET_SLOT_BODY)
        return;

    (*targetField)[segmentId] = tileset;
}

static void populate_layout_context_from_fill_assets(const LayoutContextTargetView *targetView,
                                                     u8 segmentId,
                                                     const GaugeFillAssets *assets,
                                                     u8 fixedStartCap,
                                                     u8 fixedEndCap)
{
    const u32 *body = assets ? tileset_asset_to_rom(assets->body) : NULL;
    const u32 *end = assets ? tileset_asset_to_rom(assets->end) : NULL;
    const u32 *trail = assets ? tileset_asset_to_rom(assets->trail) : NULL;
    const u32 *bridge = assets ? tileset_asset_to_rom(assets->bridge) : NULL;
    const u32 *capStart = fixedStartCap ? (end ? end : body) : NULL;
    const u32 *capStartBreak = fixedStartCap ? body : NULL;
    const u32 *capStartTrail = fixedStartCap ? trail : NULL;
    const u32 *capEnd = fixedEndCap ? (end ? end : body) : NULL;

    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_BODY, segmentId, body);
    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_END, segmentId, end);
    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_TRAIL, segmentId, trail);
    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_BRIDGE, segmentId, bridge);
    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_CAP_START, segmentId, capStart);
    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_CAP_END, segmentId, capEnd);
    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_CAP_START_BREAK, segmentId, capStartBreak);
    assign_layout_context_slot(targetView, LAYOUT_TILESET_SLOT_CAP_START_TRAIL, segmentId, capStartTrail);
}

static u8 build_fill_layout_direct(const GaugeDefinition *definition,
                                   const GaugeBuildLanePlan *lanePlan,
                                   GaugeLaneLayout *layout)
{
    const u8 segmentCount = lanePlan->segmentCount;
    const u32 *baseBodyTilesets[GAUGE_MAX_SEGMENTS] = {0};
    SkinSetUsageFlags baseFlags;
    SkinSetUsageFlags gainFlags;
    SkinSetUsageFlags blinkFlags;
    SkinSetUsageFlags gainBlinkFlags;
    const u8 capEndFlags = definition->fixedEndCap ? 1 : 0;

    memset(&baseFlags, 0, sizeof(baseFlags));
    memset(&gainFlags, 0, sizeof(gainFlags));
    memset(&blinkFlags, 0, sizeof(blinkFlags));
    memset(&gainBlinkFlags, 0, sizeof(gainBlinkFlags));

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
    {
        const GaugeSegment *segment = &lanePlan->lane->segments[segmentIndex];
        if (!segment->skin || !segment->skin->fill.normal.body)
            return 0;

        baseBodyTilesets[segmentIndex] = segment->skin->fill.normal.body->tiles;
        scan_fill_assets_usage(&segment->skin->fill.normal,
                               definition->fixedStartCap,
                               definition->fixedEndCap,
                               &baseFlags);
        scan_fill_assets_usage(&segment->skin->fill.gain,
                               definition->fixedStartCap,
                               definition->fixedEndCap,
                               &gainFlags);
        scan_fill_assets_usage(&segment->skin->fill.blinkOff,
                               definition->fixedStartCap,
                               definition->fixedEndCap,
                               &blinkFlags);
        scan_fill_assets_usage(&segment->skin->fill.blinkOff,
                               definition->fixedStartCap,
                               definition->fixedEndCap,
                               &gainBlinkFlags);
    }

    GaugeLaneLayout_initEx(layout,
                           lanePlan->length,
                           definition->fillDirection,
                           baseBodyTilesets,
                           NULL,
                           NULL,
                           NULL,
                           lanePlan->segmentIdByCell,
                           definition->orientation,
                           lanePlan->palette,
                           definition->priority,
                           definition->verticalFlip,
                           definition->horizontalFlip);

    if (layout->length == 0 || layout->segmentCount == 0)
        return 0;

    sync_base_allocations(layout, &baseFlags, capEndFlags);
    sync_layout_context_slots_from_usage(layout,
                                         LAYOUT_STYLE_CONTEXT_GAIN,
                                         &gainFlags,
                                         LAYOUT_TILESET_SLOT_MASK_ALL);
    sync_layout_context_slots_from_usage(layout,
                                         LAYOUT_STYLE_CONTEXT_BLINK,
                                         &blinkFlags,
                                         LAYOUT_TILESET_SLOT_MASK_ALL);
    sync_layout_context_slots_from_usage(layout,
                                         LAYOUT_STYLE_CONTEXT_GAIN_BLINK,
                                         &gainBlinkFlags,
                                         LAYOUT_TILESET_SLOT_MASK_ALL);

    LayoutContextTargetView baseView;
    LayoutContextTargetView gainView;
    LayoutContextTargetView blinkView;
    LayoutContextTargetView gainBlinkView;
    build_layout_context_target_view(layout, LAYOUT_STYLE_CONTEXT_BASE, &baseView);
    build_layout_context_target_view(layout, LAYOUT_STYLE_CONTEXT_GAIN, &gainView);
    build_layout_context_target_view(layout, LAYOUT_STYLE_CONTEXT_BLINK, &blinkView);
    build_layout_context_target_view(layout, LAYOUT_STYLE_CONTEXT_GAIN_BLINK, &gainBlinkView);

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
    {
        const GaugeFillSkin *skin = &lanePlan->lane->segments[segmentIndex].skin->fill;

        populate_layout_context_from_fill_assets(&baseView,
                                                 segmentIndex,
                                                 &skin->normal,
                                                 definition->fixedStartCap,
                                                 definition->fixedEndCap);
        populate_layout_context_from_fill_assets(&gainView,
                                                 segmentIndex,
                                                 &skin->gain,
                                                 definition->fixedStartCap,
                                                 definition->fixedEndCap);
        populate_layout_context_from_fill_assets(&blinkView,
                                                 segmentIndex,
                                                 &skin->blinkOff,
                                                 definition->fixedStartCap,
                                                 definition->fixedEndCap);
        populate_layout_context_from_fill_assets(&gainBlinkView,
                                                 segmentIndex,
                                                 &skin->blinkOff,
                                                 definition->fixedStartCap,
                                                 definition->fixedEndCap);

        if (layout->capEndBySegment != s_zeroSegmentFlags)
            layout->capEndBySegment[segmentIndex] = definition->fixedEndCap ? 1 : 0;
    }

    finalize_layout_derived_state(layout);
    return 1;
}

static u8 build_pip_layout_direct(const GaugeDefinition *definition,
                                  const GaugeBuildLanePlan *lanePlan,
                                  GaugeLaneLayout *layout)
{
    const u8 segmentCount = lanePlan->segmentCount;
    u8 hasPipStyles = 0;
    u8 hasCustomWidths = 0;
    u8 hasCustomHeights = 0;
    u8 hasCustomCoverage = 0;
    u8 hasCustomHalfAxis = 0;
    u8 hasCustomSourceWidths = 0;
    u8 hasCustomSourceHeights = 0;

    GaugeLaneLayout_initEx(layout,
                           lanePlan->length,
                           definition->fillDirection,
                           NULL,
                           NULL,
                           NULL,
                           NULL,
                           lanePlan->segmentIdByCell,
                           definition->orientation,
                           lanePlan->palette,
                           definition->priority,
                           definition->verticalFlip,
                           definition->horizontalFlip);

    if (layout->length == 0 || layout->segmentCount == 0)
        return 0;

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
    {
        const GaugeSegment *segment = &lanePlan->lane->segments[segmentIndex];
        if (!segment->skin)
            return 0;

        const GaugePipSkin *pip = &segment->skin->pip;
        const u8 pipWidth = pip->pipWidth ? pip->pipWidth : 1;
        const u8 pipHeight = pip->pipHeight ? pip->pipHeight : 1;
        u8 pipStateCount = 0;
        u8 sourceWidth = 1;
        u8 sourceHeight = 1;
        u8 halfAxis = PIP_HALF_AXIS_HORIZONTAL;

        if (!resolve_pip_strip_geometry_from_skin(pip,
                                                  &pipStateCount,
                                                  &sourceWidth,
                                                  &sourceHeight,
                                                  &halfAxis))
        {
            return 0;
        }

        hasPipStyles = 1;
        hasCustomWidths |= (pipWidth > 1);
        hasCustomHeights |= (pipHeight > 1);
        hasCustomCoverage |= (pip->coverage != GAUGE_STRIP_COVERAGE_FULL);
        hasCustomHalfAxis |= (halfAxis != PIP_HALF_AXIS_HORIZONTAL);
        hasCustomSourceWidths |= (sourceWidth > 1);
        hasCustomSourceHeights |= (sourceHeight > 1);
    }

    layout_sync_optional_segment_tilesets_by_usage(
        hasPipStyles,
        &layout->pipTilesetBySegment,
        layout->segmentCount);
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles && hasCustomWidths,
        &layout->pipWidthBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles && hasCustomHeights,
        &layout->pipHeightBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles,
        &layout->pipStateCountBySegment,
        layout->segmentCount,
        s_zeroSegmentFlags,
        0);
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles && hasCustomCoverage,
        &layout->pipStripCoverageBySegment,
        layout->segmentCount,
        s_zeroSegmentFlags,
        GAUGE_STRIP_COVERAGE_FULL);
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles && hasCustomHalfAxis,
        &layout->pipHalfAxisBySegment,
        layout->segmentCount,
        s_zeroSegmentFlags,
        PIP_HALF_AXIS_HORIZONTAL);
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles && hasCustomSourceWidths,
        &layout->pipSourceWidthBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);
    layout_sync_optional_segment_flags_by_usage(
        hasPipStyles && hasCustomSourceHeights,
        &layout->pipSourceHeightBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
    {
        const GaugePipSkin *pip = &lanePlan->lane->segments[segmentIndex].skin->pip;
        const u8 pipWidth = pip->pipWidth ? pip->pipWidth : 1;
        const u8 pipHeight = pip->pipHeight ? pip->pipHeight : 1;
        u8 pipStateCount = 0;
        u8 sourceWidth = 1;
        u8 sourceHeight = 1;
        u8 halfAxis = PIP_HALF_AXIS_HORIZONTAL;
        u8 coverage = pip->coverage;

        if (!resolve_pip_strip_geometry_from_skin(pip,
                                                  &pipStateCount,
                                                  &sourceWidth,
                                                  &sourceHeight,
                                                  &halfAxis))
        {
            return 0;
        }

        if (coverage > GAUGE_STRIP_COVERAGE_QUARTER)
            coverage = GAUGE_STRIP_COVERAGE_FULL;

        if (layout->pipTilesetBySegment != s_nullSegmentTilesets)
            layout->pipTilesetBySegment[segmentIndex] = pip->tileset->tiles;
        if (layout->pipWidthBySegment != s_oneSegmentFlags)
            layout->pipWidthBySegment[segmentIndex] = pipWidth;
        if (layout->pipHeightBySegment != s_oneSegmentFlags)
            layout->pipHeightBySegment[segmentIndex] = (pipHeight > 4) ? 4 : pipHeight;
        if (layout->pipStateCountBySegment != s_zeroSegmentFlags)
            layout->pipStateCountBySegment[segmentIndex] = pipStateCount;
        if (layout->pipStripCoverageBySegment != s_zeroSegmentFlags)
            layout->pipStripCoverageBySegment[segmentIndex] = coverage;
        if (layout->pipHalfAxisBySegment != s_zeroSegmentFlags)
            layout->pipHalfAxisBySegment[segmentIndex] = halfAxis;
        if (layout->pipSourceWidthBySegment != s_oneSegmentFlags)
            layout->pipSourceWidthBySegment[segmentIndex] = sourceWidth;
        if (layout->pipSourceHeightBySegment != s_oneSegmentFlags)
            layout->pipSourceHeightBySegment[segmentIndex] = sourceHeight;
    }

    finalize_layout_derived_state(layout);
    return 1;
}

static u8 build_lane_plan(const GaugeDefinition *definition,
                          u8 laneIndex,
                          const GaugeLane *lane,
                          const GaugeLane *baseLane,
                          GaugeBuildLanePlan *outPlan)
{
    if (!definition || !lane || !baseLane || !outPlan)
        return 0;

    memset(outPlan, 0, sizeof(*outPlan));

    const s16 originX = compute_lane_origin_axis(definition->originX, lane->offsetX);
    const s16 originY = compute_lane_origin_axis(definition->originY, lane->offsetY);
    if (originX < 0 || originY < 0)
        return 0;

    outPlan->lane = lane;
    outPlan->laneIndex = laneIndex;
    outPlan->originX = (u16)originX;
    outPlan->originY = (u16)originY;
    outPlan->fillOffset = compute_fill_offset_pixels_for_lane(definition,
                                                              baseLane,
                                                              lane->firstValueCell);
    outPlan->palette = resolve_lane_palette(definition, lane);

    if (!build_segment_id_by_cell_from_lane(definition->mode,
                                            definition->fillDirection,
                                            lane,
                                            outPlan->segmentIdByCell,
                                            &outPlan->length,
                                            &outPlan->segmentCount))
    {
        return 0;
    }

    if (definition->orientation == GAUGE_ORIENT_VERTICAL &&
        outPlan->originY < (u16)(outPlan->length - 1))
    {
        return 0;
    }

    return 1;
}

static u8 build_layout_from_lane_plan(const GaugeDefinition *definition,
                                      const GaugeBuildLanePlan *lanePlan,
                                      GaugeLaneLayout **outLayout)
{
    if (!definition || !lanePlan || !outLayout)
        return 0;

    GaugeLaneLayout *layout = (GaugeLaneLayout *)gauge_alloc_bytes((u16)sizeof(GaugeLaneLayout));
    if (!layout)
        return 0;

    const u8 buildOk = (definition->mode == GAUGE_MODE_FILL)
        ? build_fill_layout_direct(definition, lanePlan, layout)
        : build_pip_layout_direct(definition, lanePlan, layout);

    if (!buildOk)
    {
        release_built_layout(&layout);
        return 0;
    }

    if (lanePlan->fillOffset != 0)
        GaugeLaneLayout_setFillOffset(layout, lanePlan->fillOffset);

    *outLayout = layout;
    return 1;
}

u8 Gauge_build(Gauge *gauge,
               const GaugeDefinition *definition,
               u16 vramBase)
{
    if (!gauge || !definition)
        return 0;

    memset(&s_buildScratch, 0, sizeof(s_buildScratch));
    GaugeDefinition *sanitized = &s_buildScratch.sanitizedDefinition;
    *sanitized = *definition;
    sanitized->mode = (GaugeMode)sanitize_value_mode((u8)sanitized->mode);
    sanitized->orientation = (GaugeOrientation)sanitize_orientation((u8)sanitized->orientation);
    sanitized->fillDirection = (GaugeFillDirection)sanitize_fill_direction((u8)sanitized->fillDirection);
    sanitized->vramMode = (GaugeVramMode)sanitize_vram_mode((u8)sanitized->vramMode);
    sanitized->palette = sanitize_palette_index(sanitized->palette);
    sanitized->priority = sanitized->priority ? 1 : 0;
    sanitized->verticalFlip = sanitized->verticalFlip ? 1 : 0;
    sanitized->horizontalFlip = sanitized->horizontalFlip ? 1 : 0;
    sanitized->fixedStartCap = sanitized->fixedStartCap ? 1 : 0;
    sanitized->fixedEndCap = sanitized->fixedEndCap ? 1 : 0;
    if (sanitized->maxValue > GAUGE_LUT_CAPACITY)
        sanitized->maxValue = GAUGE_LUT_CAPACITY;

    u8 baseLaneIndex = 0;
    if (!find_baseLane_index(sanitized, &baseLaneIndex))
        return 0;

    const GaugeLane *baseLane = &sanitized->lanes[baseLaneIndex];
    GaugeBuildLanePlan *lanePlans = s_buildScratch.lanePlans;
    GaugeLaneLayout **builtLayouts = s_buildScratch.builtLayouts;
    u8 laneCount = 0;
    u8 longestLaneLength = 0;

    for (u8 laneIndex = 0; laneIndex < GAUGE_MAX_LANES; laneIndex++)
    {
        const GaugeLane *lane = &sanitized->lanes[laneIndex];
        if (definition_lane_is_empty(lane))
            continue;

        if (laneCount >= GAUGE_MAX_LANES)
        {
            release_built_layouts(builtLayouts);
            return 0;
        }

        if (!build_lane_plan(sanitized,
                             laneIndex,
                             lane,
                             baseLane,
                             &lanePlans[laneCount]))
        {
            release_built_layouts(builtLayouts);
            return 0;
        }

        if (lanePlans[laneCount].length > longestLaneLength)
            longestLaneLength = lanePlans[laneCount].length;

        laneCount++;
    }

    if (laneCount == 0)
        return 0;

    u16 resolvedMaxValue = sanitized->maxValue;
    if (sanitized->mode == GAUGE_MODE_FILL && resolvedMaxValue == 0)
        resolvedMaxValue = (u16)(longestLaneLength << TILE_TO_PIXEL_SHIFT);
    if (resolvedMaxValue > GAUGE_LUT_CAPACITY)
        resolvedMaxValue = GAUGE_LUT_CAPACITY;

    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
    {
        if (!build_layout_from_lane_plan(sanitized,
                                         &lanePlans[laneIndex],
                                         &builtLayouts[laneIndex]))
        {
            release_built_layouts(builtLayouts);
            return 0;
        }
    }

    Gauge_release(gauge);
    Gauge_init(gauge, &(GaugeInit){
        .maxValue = resolvedMaxValue,
        .initialValue = resolvedMaxValue,
        .layout = builtLayouts[0],
        .vramBase = vramBase,
        .vramMode = sanitized->vramMode,
        .valueMode = (GaugeValueMode)sanitized->mode
    });

    if (!gauge->tickAndRenderHandler)
    {
        Gauge_release(gauge);
        release_built_layouts(builtLayouts);
        return 0;
    }

    Gauge_setValueAnim(gauge,
                       sanitized->behavior.valueAnimEnabled,
                       sanitized->behavior.valueAnimShift);
    Gauge_setTrailMode(gauge,
                       sanitized->behavior.damageMode,
                       sanitized->behavior.criticalValue,
                       sanitized->behavior.damageAnimShift,
                       sanitized->behavior.damageBlinkShift);
    Gauge_setGainMode(gauge,
                      sanitized->behavior.gainMode,
                      sanitized->behavior.gainAnimShift,
                      sanitized->behavior.gainBlinkShift);

    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
    {
        if (!Gauge_addLane(gauge,
                           builtLayouts[laneIndex],
                           lanePlans[laneIndex].originX,
                           lanePlans[laneIndex].originY))
        {
            Gauge_release(gauge);
            release_built_layouts(builtLayouts);
            return 0;
        }
    }

    gauge->ownedLayoutCount = laneCount;
    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
        gauge->ownedLayouts[laneIndex] = builtLayouts[laneIndex];

    DMA_flushQueue();

    return 1;
}

/**
 * FILL-mode update path: tick logic + render all lanes.
 * Call once per frame in the game loop.
 *
 * Execution flow:
 * 1. logicTickHandler() - advance value animation + trail mode state
 * 2. Compute render state (valuePixels, trailPixelsRendered, blinkOn)
 * 3. Change detection: compare against lastValuePixels, lastTrailPixelsRendered,
 *    lastBlinkOn, and check if value animation is still converging
 * 4. If nothing changed -> early return (zero CPU cost after initial comparison)
 * 5. If changed -> render all lanes via lane->renderHandler
 *
 * The change detection cache (lastValuePixels, lastTrailPixelsRendered, lastBlinkOn)
 * is initialized to CACHE_INVALID_U16/U8 to force the first render.
 * Adding a new lane also invalidates the cache (Gauge_addLane sets lastValuePixels
 * to CACHE_INVALID_U16).
 *
 * When trail is disabled, trailPixelsRendered == valuePixels naturally
 * (enforced by logic tick), so the blink/trail paths are no-ops.
 *
 * Cost: ~20 cycles (early return) to ~2000+ cycles (full render with DMA)
 */

/* --- Shared tick-and-render helpers (fill & pip) --- */

/** Blink and trail mode state, computed once per tick. */
typedef struct {
    u8 blinkOn;
    u8 blinkOffActive;
    u8 blinkOnChanged;
    u8 useValueBlinkRendering;
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
    const u8 activeBlinkShift =
        (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN &&
         logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW)
        ? logic->gainBlinkShift
        : logic->blinkShift;
    const u8 criticalModeActive =
        logic->modeUsesCriticalThreshold ? is_critical_mode_active(logic) : 0;

    s.blinkOn = 1;
    if (logic->blinkFramesRemaining > 0)
    {
        /* Toggle blink on/off at a rate controlled by blinkShift.
         * blinkTimer increments each frame; shifting right by blinkShift
         * divides by 2^blinkShift, so bit 0 of the result toggles every
         * 2^blinkShift frames.  blinkShift=1 -> toggle every 2 frames,
         * blinkShift=2 -> every 4 frames, etc. */
        s.blinkOn = (u8)(((logic->blinkTimer >> activeBlinkShift) & 1) == 0);
    }
    s.blinkOnChanged = (logic->lastBlinkOn != s.blinkOn);

    s.useValueBlinkRendering =
        (logic->activeTrailState != GAUGE_TRAIL_STATE_GAIN &&
         logic->modeUsesValueBlink &&
         criticalModeActive) ? 1 : 0;

    s.trailMode = logic->activeTrailState;
    if (s.useValueBlinkRendering)
        s.trailMode = GAUGE_TRAIL_STATE_DAMAGE;
    s.trailModeChanged = (logic->lastActiveTrailState != s.trailMode);
    s.blinkOffActive = ((logic->trailEnabled || s.useValueBlinkRendering) &&
                        logic->blinkFramesRemaining > 0 &&
                        s.blinkOn == 0);
    return s;
}

static inline u8 compute_pip_blink_off_active(const GaugeLogic *logic,
                                              const BlinkState *blinkState)
{
    if (!logic || !blinkState || !blinkState->blinkOffActive)
        return 0;

    if (blinkState->trailMode == GAUGE_TRAIL_STATE_GAIN)
        return (logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW) ? 1 : 0;

    if (blinkState->useValueBlinkRendering)
        return 0;

    if (blinkState->trailMode == GAUGE_TRAIL_STATE_DAMAGE)
        return (logic->configuredTrailMode == GAUGE_TRAIL_MODE_FOLLOW) ? 1 : 0;

    return 0;
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
    const u8 criticalModeActive =
        logic->modeUsesCriticalThreshold ? is_critical_mode_active(logic) : 0;

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
    logic->lastActiveTrailState = bs->trailMode;

    /* Detect fully idle state: value at target and no active timed work.
     * We still render this frame, then next Gauge_update returns via needUpdate. */
    if (!criticalModeActive &&
        valueForIdle == logic->valueTargetPixels &&
        logic->holdFramesRemaining == 0 &&
        logic->blinkFramesRemaining == 0)
    {
        if (logic->modeKeepsStaticTrail)
        {
            logic->needUpdate = 0;
        }
        else if (trailForIdle == valueForIdle &&
                 logic->activeTrailState == GAUGE_TRAIL_STATE_NONE)
        {
            logic->needUpdate = 0;
        }
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
    if (gauge->logicTickHandler)
        gauge->logicTickHandler(logic);
    
    /* --- Compute current render state --- */
    const u16 valuePixelsRaw = logic->valuePixels;

    /* Compute trail with blink effect.
     * trailPixels = actual trail position (clamped to at least valuePixels).
     * trailPixelsRendered = what the render loop uses:
     *   - blinkOn=1 -> show trail normally (trailPixels)
     *   - blinkOn=0 -> hide trail by collapsing it to valuePixels */
    u16 trailPixelsRaw = logic->trailPixels;
    if (trailPixelsRaw < valuePixelsRaw)
        trailPixelsRaw = valuePixelsRaw;

    const BlinkState bs = compute_blink_state(logic);
    u16 valuePixels = valuePixelsRaw;
    u16 trailPixelsRendered = bs.blinkOn ? trailPixelsRaw : valuePixelsRaw;
    u16 trailPixelsActual = trailPixelsRaw;
    u16 valueTargetForEarlyReturn = logic->valueTargetPixels;

    if (bs.useValueBlinkRendering && !bs.blinkOn)
    {
        valuePixels = 0;
        trailPixelsRendered = valuePixelsRaw;
        trailPixelsActual = valuePixelsRaw;
        valueTargetForEarlyReturn = 0;
    }

    if (update_render_cache_and_check(logic, valuePixels, trailPixelsRendered,
                                       &bs, valueTargetForEarlyReturn,
                                       valuePixelsRaw, trailPixelsRaw))
        return;

    gauge_build_baseLane_fill_decisions(gauge, valuePixels, trailPixelsRendered,
                                      trailPixelsActual, bs.blinkOffActive,
                                      bs.trailMode);
    s_activeGaugeForRender = gauge;

#if GAUGE_ENABLE_TRACE
    trace_begin_frame(gauge, valuePixels, trailPixelsRendered, trailPixelsActual,
                      bs.trailMode, bs.blinkOffActive);
#endif

    /* --- Render all lanes (countdown: 68000 dbra optimization) --- */
    u8 i = gauge->laneCount;
    while (i--)
    {
        GaugeLaneInstance *lane = gauge->lanes[i];
        if (!lane)
            continue;
#if GAUGE_ENABLE_TRACE
        trace_set_lane_index(i);
#endif
        lane->renderHandler(lane, valuePixels, trailPixelsRendered, trailPixelsActual,
                            bs.blinkOffActive, bs.blinkOnChanged,
                            bs.trailMode, bs.trailModeChanged);
    }

#if GAUGE_ENABLE_TRACE
    trace_end_frame();
#endif
    s_activeGaugeForRender = NULL;
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
 * PIP-mode update path: tick logic + render all lanes.
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
    if (gauge->logicTickHandler)
        gauge->logicTickHandler(logic);

    /* --- Compute current render state (quantized to pip steps) --- */
    const u16 valuePixelsRaw = logic->valuePixels;
    u16 trailPixelsRaw = logic->trailPixels;
    if (trailPixelsRaw < valuePixelsRaw)
        trailPixelsRaw = valuePixelsRaw;

    const BlinkState bs = compute_blink_state(logic);

    const u16 valuePixelsQuantized = quantize_pixels_to_pip_step(logic, valuePixelsRaw);
    u16 trailPixels = quantize_pixels_to_pip_step(logic, trailPixelsRaw);
    if (trailPixels < valuePixelsQuantized)
        trailPixels = valuePixelsQuantized;

    u16 valuePixels = valuePixelsQuantized;
    u16 trailPixelsRendered = bs.blinkOn ? trailPixels : valuePixelsQuantized;
    u16 trailPixelsActual = trailPixels;
    u16 valueTargetPixels = quantize_pixels_to_pip_step(logic, logic->valueTargetPixels);
    const u8 pipBlinkOffActive = compute_pip_blink_off_active(logic, &bs);

    if (bs.useValueBlinkRendering && bs.blinkOn)
    {
        valuePixels = 0;
        trailPixelsRendered = valuePixelsQuantized;
        trailPixelsActual = valuePixelsQuantized;
        valueTargetPixels = 0;
    }

    if (update_render_cache_and_check(logic, valuePixels, trailPixelsRendered,
                                       &bs, valueTargetPixels,
                                       valuePixelsRaw, trailPixelsRaw))
        return;

    gauge_build_baseLane_pip_states(gauge, valuePixels, trailPixelsRendered,
                                  trailPixelsActual, pipBlinkOffActive,
                                  bs.trailMode);
    s_activeGaugeForRender = gauge;

#if GAUGE_ENABLE_TRACE
    trace_begin_frame(gauge, valuePixels, trailPixelsRendered, trailPixelsActual,
                      bs.trailMode, pipBlinkOffActive);
#endif

    /* --- Render all lanes --- */
    u8 i = gauge->laneCount;
    while (i--)
    {
        GaugeLaneInstance *lane = gauge->lanes[i];
        if (!lane)
            continue;
#if GAUGE_ENABLE_TRACE
        trace_set_lane_index(i);
#endif
        lane->renderHandler(lane, valuePixels, trailPixelsRendered, trailPixelsActual,
                            pipBlinkOffActive, bs.blinkOnChanged,
                            bs.trailMode, bs.trailModeChanged);
    }

#if GAUGE_ENABLE_TRACE
    trace_end_frame();
#endif
    s_activeGaugeForRender = NULL;
}

/* First update call: lock runtime configuration then delegate to steady handler. */
static void gauge_tick_and_render_bootstrap(Gauge *gauge)
{
    GaugeTickAndRenderHandler *steadyHandler;

    if (!gauge)
        return;

    steadyHandler = gauge->steadyTickAndRenderHandler;
    if (!steadyHandler)
        return;

    gauge->runtimeLocked = GAUGE_RUNTIME_CLOSED;
    gauge->tickAndRenderHandler = steadyHandler;
    steadyHandler(gauge);
}

/** Update gauge: tick logic + render all lanes. Call once per frame. */
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
    apply_configured_trail_mode_state(logic, 0);
}

/**
 * Decrease gauge value (damage).
 *
 * FOLLOW mode:
 *   Trail holds at the previous position for holdFrames, then blinks for
 *   blinkFrames, then shrinks toward the new value.
 *
 * Other modes:
 *   Decrease applies their configured immediate behavior
 *   (disabled/static/critical).
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

    if (gauge->logicTickHandler != GaugeLogic_tick_follow_mode)
    {
        apply_non_follow_trail_mode_state(logic, 0);
        return;
    }

    /* Trail anchors at the currently displayed value (not the new target).
     * This creates the visual "damage gap" between trail and value.
     * Uses valuePixels (animated position) so the trail doesn't jump ahead
     * if a previous animation is still in progress.
     * If switching from GAIN mode, reset trail from the target to current. */
    const u16 previousDisplayedValuePixels = logic->valuePixels;
    if (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN)
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
    logic->activeTrailState = GAUGE_TRAIL_STATE_DAMAGE;
}

/**
 * Increase gauge value (heal).
 *
 * Gain mode FOLLOW:
 *   If value animation is enabled, triggers gain trail:
 *   trail jumps to target, holds, blinks, then value catches up.
 *
 * Other gain modes:
 *   Increase falls back to the configured damage-mode visual state.
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

    if ((GaugeGainMode)logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW &&
        logic->valueAnimEnabled)
    {
        /* Gain trail: trail leads, value catches up after hold/blink */
        logic->trailPixels = logic->valueTargetPixels;
        logic->holdFramesRemaining = holdFrames;
        logic->blinkFramesRemaining = blinkFrames;
        logic->blinkTimer = 0;
        logic->activeTrailState = GAUGE_TRAIL_STATE_GAIN;
        return;
    }

    if ((GaugeTrailMode)logic->configuredTrailMode == GAUGE_TRAIL_MODE_FOLLOW)
    {
        /* No gain behavior: trail follows value immediately. */
        logic->trailPixels = logic->valuePixels;
        logic->holdFramesRemaining = 0;
        logic->blinkFramesRemaining = 0;
        logic->blinkTimer = 0;
        logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    }
    else
    {
        /* Non-follow damage mode keeps its own visual contract. */
        apply_non_follow_trail_mode_state(logic, 0);
    }
}

u16 Gauge_getValue(const Gauge *gauge)
{
    return gauge ? gauge->logic.currentValue : 0;
}


/* =============================================================================
   Utility functions
   ============================================================================= */

/**
 * Compute how many VRAM tiles are needed for a layout.
 *
 * VRAM budget depends on mode:
 * - PIP fixed:   1 tile per rendered PIP tile (height-expanded)
 * - PIP dynamic: shared slots per segment (sourceWidth * sourceHeight * stateCount)
 * - Fixed:   1 tile per cell with a valid tileset
 * - Dynamic: 3 standard tiles per unique segment (empty/full/fullTrail) +
 *            partial tiles (value/trail/end/trailSecond) + bridge + cap tiles
 *
 * @return Number of VRAM tiles required
 */
static u16 compute_vram_size_for_layout(const GaugeLaneLayout *layout,
                                        GaugeVramMode vramMode,
                                        u8 trailEnabled,
                                        GaugeValueMode valueMode)
{
    if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        if (vramMode == GAUGE_VRAM_DYNAMIC)
        {
            /* Dynamic PIP mode: shared slots per segment/state/localTile. */
            u16 count = 0;
            for (u8 segmentId = 0; segmentId < layout->segmentCount; segmentId++)
            {
                if (!layout->pipTilesetBySegment[segmentId])
                    continue;

                u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
                if (sourceWidth == 0)
                    sourceWidth = 1;
                u8 sourceHeight = layout->pipSourceHeightBySegment[segmentId];
                if (sourceHeight == 0)
                    sourceHeight = 1;
                if (sourceHeight > 4)
                    sourceHeight = 4;

                const u8 pipStateCount = layout->pipStateCountBySegment[segmentId];
                if (pipStateCount < 2)
                    continue;

                count = (u16)(count + ((u16)sourceWidth * sourceHeight * pipStateCount));
            }
            return count;
        }

        /* Fixed PIP mode: one tile per rendered physical tile (height-expanded). */
        return layout->pipRenderCount;
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
     * Partial tiles (scalars - 1 per GaugeLaneInstance):
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
                      const GaugeLaneLayout *layout)
{
    if (!gauge || !layout)
        return 0;

    return compute_vram_size_for_layout(layout,
                                        gauge->vramMode,
                                        gauge->logic.trailEnabled,
                                        gauge->valueMode);
}

