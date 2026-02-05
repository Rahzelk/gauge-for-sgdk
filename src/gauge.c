#include "gauge.h"

/* =============================================================================
   gauge.c ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Â Simplified single-lane HUD gauge implementation for SGDK
   =============================================================================
 
   IMPLEMENTATION NOTES:
   ---------------------
   - Tile index LUT (s_fillTileStripsIndexByValueTrail) provides O(1) lookup
   - Change detection avoids redundant rendering

   PERFORMANCE CHARACTERISTICS:
   - GaugeLogic_tick: ~50-100 cycles (mostly conditionals)
   - process_fixed_mode: ~200 cycles per cell (DMA dominant)
   - process_dynamic_mode: ~300-500 cycles total (tilemap writes dominant)

   ============================================================================= */

/* -----------------------------------------------------------------------------
   Constants
   ----------------------------------------------------------------------------- */
#define TILE_TO_PIXEL_SHIFT  3

/* Invalid/uninitialized marker for cache values */
#define CACHE_INVALID_U16    0xFFFF
#define CACHE_INVALID_U8     0xFF

/* Helper macros */
#define CALC_ANIM_STEP(diff, shift)   ((u16)(((diff) >> (shift)) + 1))
#define FILL_IDX_TO_OFFSET(idx)       (((u16)(idx)) << 3)



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
static const u8 s_fillTileStripsIndexByValueTrail[9][9] =
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
static const u8 s_trailTileStripsIndexByValueTrail[9][9] =
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
     * IMPORTANT: must NOT cast v to u16 before shift Ã¢â‚¬â€ that would make it unsigned
     * and give 0 or 1 instead of 0 or 0xFFFF, breaking the mask. */
    u16 clamped = (u16)(v & ~(v >> 15));
    /* Upper clamp: cap at 8 (rare case, only at break cells) */
    return (clamped > GAUGE_PIXELS_PER_TILE) ? GAUGE_PIXELS_PER_TILE : (u8)clamped;
}

/**
 * Convert gauge value to pixel position.
 * Uses LUT if available, otherwise returns value directly (1:1 mapping).
 *
 * The LUT is auto-generated by Gauge_init when maxValue != maxFillPixels,
 * or can be provided by user via Gauge_initEx.
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
 * Compute tile position on WINDOW plane.
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
   GaugeBreakInfo Ã¢â‚¬â€ Pre-computed break/transition zone data

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
                                      GaugeBreakInfo *out)
{
    /* Compute value/trail break positions (fillIndex + px inside that cell) */
    out->valueFillIndex = compute_fill_index_and_px(layout, valuePixels, &out->valuePxInBreakCell);
    out->trailFillIndex = compute_fill_index_and_px(layout, trailPixelsRendered, &out->trailPxInBreakCell);

    /* O(1) inverse LUT lookup instead of O(n) linear scan (~300 cycles saved) */
    out->valueCellIndex = layout->cellIndexByFillIndex[out->valueFillIndex];
    out->trailCellIndex = layout->cellIndexByFillIndex[out->trailFillIndex];

    const u8 valueSegId = layout->segmentIdByCell[out->valueCellIndex];
    const u8 trailSegId = layout->segmentIdByCell[out->trailCellIndex];
    const u8 valueSegmentHasEnd = (layout->tilesetEndBySegment[valueSegId] != NULL);
    const u8 trailSegmentHasEnd = (layout->tilesetEndBySegment[trailSegId] != NULL);
    const u8 valueSegmentHasTrail = (layout->tilesetTrailBySegment[valueSegId] != NULL);

    /* Detect if break cells belong to this layout (multi-part gauges) */
    const u16 layoutStart = layout->fillOffset;
    const u16 layoutEnd = (u16)(layoutStart + ((u16)layout->length << TILE_TO_PIXEL_SHIFT));
    const u8 valueInLayout = (valuePixels >= layoutStart && valuePixels <= layoutEnd);
    const u8 trailInLayout = (trailPixelsRendered >= layoutStart && trailPixelsRendered <= layoutEnd);

    /* END cap (E): placed on the visible trail break cell (T) */
    out->endFillIndex = (trailInLayout && trailSegmentHasEnd) ? out->trailFillIndex : CACHE_INVALID_U8;

    /* VALUE BREAK (B): one cell before value break */
    out->valueBreakFillIndex =
        (valueInLayout && valueSegmentHasEnd && out->valueFillIndex > 0)
        ? (u8)(out->valueFillIndex - 1) : CACHE_INVALID_U8;

    /* TRAIL BREAK (T): active only when value and trail are not in the same cell */
    out->trailBreakActive =
        (trailInLayout && valueInLayout && valueSegmentHasEnd && valueSegmentHasTrail &&
         (out->valueCellIndex != out->trailCellIndex));
    out->trailBreakFillIndex = out->trailBreakActive ? out->valueFillIndex : CACHE_INVALID_U8;
    out->trailBreakSecondActive =
        (out->trailBreakActive && (out->trailFillIndex > (u8)(out->valueFillIndex + 1)));
    out->trailBreakFillIndex2 = out->trailBreakSecondActive
        ? (u8)(out->trailFillIndex - 1) : CACHE_INVALID_U8;

    /* Region rendering: active when both edges are visible and have END tilesets */
    out->regionRenderActive = (valueInLayout && trailInLayout &&
                               valueSegmentHasEnd && trailSegmentHasEnd);

    /* Precompute END (E) cell fill values */
    out->endValuePxInTile = 0;
    out->endTrailPxInTile = 0;
    if (trailInLayout && out->endFillIndex != CACHE_INVALID_U8)
    {
        compute_fill_for_fill_index(layout, out->endFillIndex,
                                    valuePixels, trailPixelsRendered,
                                    &out->endValuePxInTile, &out->endTrailPxInTile);
    }
}

/**
 * Check if a cell (by fill index) is in the END/TRAIL/BREAK zone.
 * Used to decide whether blink-off should apply.
 */
static inline u8 is_cell_in_trail_zone(const GaugeBreakInfo *brk, u8 cellFillIndex)
{
    if (brk->endFillIndex != CACHE_INVALID_U8 && cellFillIndex == brk->endFillIndex)
        return 1;
    if (brk->valueBreakFillIndex != CACHE_INVALID_U8 && cellFillIndex == brk->valueBreakFillIndex)
        return 1;
    if (brk->trailBreakActive)
    {
        if (cellFillIndex == brk->trailBreakFillIndex)
            return 1;
        if (brk->trailBreakSecondActive && cellFillIndex == brk->trailBreakFillIndex2)
            return 1;
        if (cellFillIndex > brk->valueFillIndex && cellFillIndex < brk->trailFillIndex)
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
   1. Cell is END (E)             Ã¢â€ â€™ capStartStrip + endIndex
   2. Cell is TRAIL (break/full)  Ã¢â€ â€™ capStartTrailStrip + trail LUT
   3. Next cell is END (E)        Ã¢â€ â€™ capStartBreakStrip + endIndex (aligned to E)
   4. Else (cell is FULL)         Ã¢â€ â€™ capStartBreakStrip + index 44 (full)

   ============================================================================= */
typedef struct
{
    const u32 *strip;     /* Selected ROM strip for this frame */
    u8 fillStripIndex;    /* Index into the selected strip */
    u8 usesBreak;         /* 1 if using the break variant strip */
    u8 usesTrail;         /* 1 if using the trail variant strip */
} CapStartResult;

/**
 * Classify cap start using explicit tileset strips.
 * Used for blink-off variants.
 */
static inline void classify_cap_start_with_strips(const GaugeBreakInfo *brk,
                                                  u8 cellFillIndex,
                                                  const u32 *capStartStrip,
                                                  const u32 *capStartBreakStrip,
                                                  const u32 *capStartTrailStrip,
                                                  CapStartResult *out)
{
    const u8 endValid = (brk->endFillIndex != CACHE_INVALID_U8);
    const u8 isEndHere = (endValid && cellFillIndex == brk->endFillIndex);
    const u8 isEndNext = (endValid && (u8)(cellFillIndex + 1) == brk->endFillIndex);

    /* Pre-compute the END fill index (used in multiple branches) */
    const u8 endIndex = endValid
        ? s_fillTileStripsIndexByValueTrail[brk->endValuePxInTile][brk->endTrailPxInTile]
        : s_fillTileStripsIndexByValueTrail[8][8];

    /* Detect trail zone states */
    const u8 isTrailBreak = (brk->trailBreakActive && capStartTrailStrip &&
                             cellFillIndex == brk->trailBreakFillIndex);
    const u8 isTrailBreak2 = (brk->trailBreakSecondActive && capStartTrailStrip &&
                              cellFillIndex == brk->trailBreakFillIndex2);
    const u8 isTrailFull = (brk->trailBreakActive && capStartTrailStrip &&
                            cellFillIndex > brk->valueFillIndex &&
                            cellFillIndex < brk->trailFillIndex);
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
        if (brk->trailBreakSecondActive)
            out->fillStripIndex = s_trailTileStripsIndexByValueTrail[brk->valuePxInBreakCell][8];
        else
            out->fillStripIndex = s_trailTileStripsIndexByValueTrail[brk->valuePxInBreakCell][brk->trailPxInBreakCell];
    }
    else if (isTrailBreak2)
    {
        /* Trail break #2: second transition cell (before END).
         * If END index is 8 (full trail column), use trail LUT for (0,8).
         * Otherwise reuse the END fill index on the trail strip. */
        if (endIndex == 8)
            out->fillStripIndex = s_trailTileStripsIndexByValueTrail[0][8];
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
        out->fillStripIndex = s_fillTileStripsIndexByValueTrail[8][8];
    }
}

/**
 * Classify the cap start cell and determine which strip + index to use.
 *
 * @param layout         Layout configuration
 * @param brk            Pre-computed break zone info
 * @param cellFillIndex  Fill index of the cap start cell
 * @param segId          Segment ID of the cap start cell
 * @param out            [out] Classification result (strip + index)
 *
 * Cost: ~30-50 cycles (a few comparisons + LUT lookups)
 */
static inline void classify_cap_start(const GaugeLayout *layout,
                                       const GaugeBreakInfo *brk,
                                       u8 cellFillIndex,
                                       u8 segId,
                                       CapStartResult *out)
{
    classify_cap_start_with_strips(brk, cellFillIndex,
                                   layout->tilesetCapStartBySegment[segId],
                                   layout->tilesetCapStartBreakBySegment[segId],
                                   layout->tilesetCapStartTrailBySegment[segId],
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
 * @param brk                 Pre-computed break zone info (for rendered trail)
 * @param cellFillIndex       Fill index of the bridge cell
 * @param valuePixels         Current value fill in pixels
 * @param trailPixelsRendered Current trail fill in pixels (after blink)
 * @return Strip index to use for the bridge tile
 *
 * Cost: ~20-40 cycles (a few comparisons + optional fill computation)
 */
static inline u8 compute_bridge_strip_index(const GaugeLayout *layout,
                                             const GaugeBreakInfo *brk,
                                             u8 cellFillIndex,
                                             u16 valuePixels,
                                             u16 trailPixelsRendered)
{
    const u8 nextFillIndex = (u8)(cellFillIndex + 1);

    /* Case 1: next cell is END Ã¢â€ â€™ use END fill index */
    if (brk->endFillIndex == nextFillIndex)
    {
        return s_fillTileStripsIndexByValueTrail[brk->endValuePxInTile][brk->endTrailPxInTile];
    }

    /* Case 2: next cell is trail break #1 Ã¢â€ â€™ compute its fill */
    if (brk->trailBreakActive && nextFillIndex == brk->trailBreakFillIndex)
    {
        u8 nextValuePx = 0;
        u8 nextTrailPx = 0;
        compute_fill_for_fill_index(layout, nextFillIndex,
                                    valuePixels, trailPixelsRendered,
                                    &nextValuePx, &nextTrailPx);
        return s_fillTileStripsIndexByValueTrail[nextValuePx][nextTrailPx];
    }

    /* Case 3: next cell is full trail (between value and trail edges) */
    if (brk->trailBreakActive &&
        nextFillIndex > brk->valueFillIndex &&
        nextFillIndex < brk->trailFillIndex)
    {
        return s_fillTileStripsIndexByValueTrail[0][8];
    }

    /* Default: fully filled (index 44) */
    return s_fillTileStripsIndexByValueTrail[8][8];
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
                        (layout->tilesetCapEndBySegment[endSegId] != NULL);
    *outCapStartEnabled = (layout->tilesetCapStartBySegment[startSegId] != NULL);

    /* Single-cell layout: cap end takes priority over cap start */
    if (layout->length == 1 && *outCapEndEnabled)
        *outCapStartEnabled = 0;
}

/**
 * Check if any blink-off tileset is provided in the layout.
 */
static inline u8 layout_has_blink_off(const GaugeLayout *layout)
{
    for (u8 i = 0; i < GAUGE_MAX_SEGMENTS; i++)
    {
        if (layout->blinkOffTilesetBySegment[i] ||
            layout->blinkOffTilesetEndBySegment[i] ||
            layout->blinkOffTilesetBreakBySegment[i] ||
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
 * Upload a cell tile from ROM strip to VRAM (fixed mode).
 */
static inline void upload_cell_if_needed(GaugeStreamCell *cell,
                                         const u32 *wantedStrip,
                                         u8 desiredStripIdx)
{
    if (!wantedStrip) return;
    if (cell->loadedFillIdx == desiredStripIdx &&
        cell->loadedFillStrip45 == wantedStrip)
        return;

    const u32 *src = wantedStrip + FILL_IDX_TO_OFFSET(desiredStripIdx);
    VDP_loadTileData(src, cell->vramTileIndex, 1, DMA_QUEUE);
    cell->loadedFillIdx = desiredStripIdx;
    cell->loadedFillStrip45 = wantedStrip;
}

/**
 * Upload a fill tile from ROM strip to VRAM.
 */
static inline void upload_fill_tile(const u32 *strip, u8 fillIndex, u16 vramTile, u8 dmaMode)
{
    const u32 *src = strip + FILL_IDX_TO_OFFSET(fillIndex);
    VDP_loadTileData(src, vramTile, 1, dmaMode);
}

/**
 * Build bridge/break lookup tables (by fillIndex).
 *
 * Scans consecutive cells in fill order. At each segment boundary
 * (where segmentId changes between adjacent cells), applies these rules:
 *
 * - If outgoing segment has a BRIDGE tileset:
 *     Ã¢â€ â€™ last cell of the segment is flagged as bridgeEnd (shows bridge tile)
 *     Ã¢â€ â€™ cell before that is flagged as bridgeBreak (forced BREAK, index 44)
 * - If outgoing segment has NO BRIDGE tileset:
 *     Ã¢â€ â€™ last cell of the segment is flagged as bridgeBreak (forced BREAK)
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
    for (u8 i = 0; i < GAUGE_MAX_LENGTH; i++)
    {
        layout->bridgeEndByFillIndex[i] = 0;
        layout->bridgeBreakByFillIndex[i] = 0;
        layout->bridgeBreakBoundaryByFillIndex[i] = 0;
    }

    for (u8 fillIndex = 0; (u16)(fillIndex + 1) < layout->length; fillIndex++)
    {
        const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
        const u8 nextCellIndex = layout->cellIndexByFillIndex[fillIndex + 1];
        const u8 segId = layout->segmentIdByCell[cellIndex];
        const u8 nextSegId = layout->segmentIdByCell[nextCellIndex];

        if (segId == nextSegId)
            continue;

        if (layout->tilesetBridgeBySegment[segId])
        {
            layout->bridgeEndByFillIndex[fillIndex] = 1;
            if (fillIndex > 0)
            {
                const u8 prevCellIndex = layout->cellIndexByFillIndex[fillIndex - 1];
                if (layout->segmentIdByCell[prevCellIndex] == segId)
                {
                    layout->bridgeBreakByFillIndex[fillIndex - 1] = 1;
                    layout->bridgeBreakBoundaryByFillIndex[fillIndex - 1] = fillIndex;
                }
            }
        }
        else
        {
            layout->bridgeBreakByFillIndex[fillIndex] = 1;
            layout->bridgeBreakBoundaryByFillIndex[fillIndex] = fillIndex;
        }
    }
}


/* =============================================================================
   GaugeLayout implementation
   ============================================================================= */

void GaugeLayout_initEx(GaugeLayout *layout,
                        u8 length,
                        GaugeFillDirection fillDir,
                        const u32 * const *bodyTilesets,
                        const u32 * const *endTilesets,
                        const u32 * const *breakTilesets,
                        const u32 * const *trailTilesets,
                        const u32 * const *bridgeTilesets,
                        const u8 *segmentIdByCell,
                        GaugeOrientation orientation,
                        u8 paletteLine,
                        u8 priority,
                        u8 vflip,
                        u8 hflip)
{
    /* Validate and clamp length */
    if (length == 0) length = 1;
    if (length > GAUGE_MAX_LENGTH) length = GAUGE_MAX_LENGTH;

    layout->length = length;
    layout->fillOffset = 0;

    /* Copy tilesets (NULL-safe) */
    for (u8 i = 0; i < GAUGE_MAX_SEGMENTS; i++)
    {
        layout->tilesetBySegment[i] = bodyTilesets ? bodyTilesets[i] : NULL;
        layout->tilesetEndBySegment[i] = endTilesets ? endTilesets[i] : NULL;
        layout->tilesetBreakBySegment[i] = breakTilesets ? breakTilesets[i] : NULL;
        layout->tilesetTrailBySegment[i] = trailTilesets ? trailTilesets[i] : NULL;
        layout->tilesetBridgeBySegment[i] = bridgeTilesets ? bridgeTilesets[i] : NULL;
        layout->tilesetCapStartBySegment[i] = NULL;
        layout->tilesetCapEndBySegment[i] = NULL;
        layout->tilesetCapStartBreakBySegment[i] = NULL;
        layout->tilesetCapStartTrailBySegment[i] = NULL;
        layout->capEndBySegment[i] = 0;

        layout->blinkOffTilesetBySegment[i] = NULL;
        layout->blinkOffTilesetEndBySegment[i] = NULL;
        layout->blinkOffTilesetBreakBySegment[i] = NULL;
        layout->blinkOffTilesetTrailBySegment[i] = NULL;
        layout->blinkOffTilesetBridgeBySegment[i] = NULL;
        layout->blinkOffTilesetCapStartBySegment[i] = NULL;
        layout->blinkOffTilesetCapEndBySegment[i] = NULL;
        layout->blinkOffTilesetCapStartBreakBySegment[i] = NULL;
        layout->blinkOffTilesetCapStartTrailBySegment[i] = NULL;
    }

    /* Copy segment IDs per cell (NULL = all segment 0) */
    for (u8 i = 0; i < length; i++)
    {
        layout->segmentIdByCell[i] = segmentIdByCell ? segmentIdByCell[i] : 0;
    }

    /* Clear unused cells */
    for (u8 i = length; i < GAUGE_MAX_LENGTH; i++)
    {
        layout->segmentIdByCell[i] = 0;
    }

    /* Set fill direction */
    if (fillDir == GAUGE_FILL_REVERSE)
        GaugeLayout_setFillReverse(layout);
    else
        GaugeLayout_setFillForward(layout);

    /* Set visual properties */
    layout->orientation = orientation;
    layout->paletteLine = paletteLine;
    layout->priority = priority ? 1 : 0;
    layout->vflip = vflip ? 1 : 0;
    layout->hflip = hflip ? 1 : 0;
}

void GaugeLayout_init(GaugeLayout *layout,
                      u8 length,
                      GaugeFillDirection fillDir,
                      const u32 * const *tilesets,
                      const u8 *segmentIdByCell,
                      GaugeOrientation orientation,
                      u8 paletteLine,
                      u8 priority,
                      u8 vflip,
                      u8 hflip)
{
    GaugeLayout_initEx(layout, length, fillDir, tilesets, NULL, NULL,
                       NULL, NULL, segmentIdByCell, orientation, paletteLine,
                       priority, vflip, hflip);
}

void GaugeLayout_setFillForward(GaugeLayout *layout)
{
    for (u8 c = 0; c < layout->length; c++)
    {
        layout->fillIndexByCell[c] = c;
        layout->cellIndexByFillIndex[c] = c;
    }

    build_bridge_luts(layout);
}

void GaugeLayout_setFillReverse(GaugeLayout *layout)
{
    for (u8 c = 0; c < layout->length; c++)
    {
        const u8 fillIdx = (u8)(layout->length - 1 - c);
        layout->fillIndexByCell[c] = fillIdx;
        layout->cellIndexByFillIndex[fillIdx] = c;
    }

    build_bridge_luts(layout);
}

void GaugeLayout_makeMirror(GaugeLayout *dst, const GaugeLayout *src)
{
    dst->length = src->length;
    dst->fillOffset = src->fillOffset;

    /* Reverse segment order */
    for (u8 c = 0; c < src->length; c++)
    {
        const u8 srcCell = (u8)(src->length - 1 - c);
        dst->segmentIdByCell[c] = src->segmentIdByCell[srcCell];
    }

    /* Copy tilesets */
    for (u8 i = 0; i < GAUGE_MAX_SEGMENTS; i++)
    {
        dst->tilesetBySegment[i] = src->tilesetBySegment[i];
        dst->tilesetEndBySegment[i] = src->tilesetEndBySegment[i];
        dst->tilesetBreakBySegment[i] = src->tilesetBreakBySegment[i];
        dst->tilesetTrailBySegment[i] = src->tilesetTrailBySegment[i];
        dst->tilesetBridgeBySegment[i] = src->tilesetBridgeBySegment[i];
        dst->tilesetCapStartBySegment[i] = src->tilesetCapStartBySegment[i];
        dst->tilesetCapEndBySegment[i] = src->tilesetCapEndBySegment[i];
        dst->tilesetCapStartBreakBySegment[i] = src->tilesetCapStartBreakBySegment[i];
        dst->tilesetCapStartTrailBySegment[i] = src->tilesetCapStartTrailBySegment[i];
        dst->capEndBySegment[i] = src->capEndBySegment[i];

        dst->blinkOffTilesetBySegment[i] = src->blinkOffTilesetBySegment[i];
        dst->blinkOffTilesetEndBySegment[i] = src->blinkOffTilesetEndBySegment[i];
        dst->blinkOffTilesetBreakBySegment[i] = src->blinkOffTilesetBreakBySegment[i];
        dst->blinkOffTilesetTrailBySegment[i] = src->blinkOffTilesetTrailBySegment[i];
        dst->blinkOffTilesetBridgeBySegment[i] = src->blinkOffTilesetBridgeBySegment[i];
        dst->blinkOffTilesetCapStartBySegment[i] = src->blinkOffTilesetCapStartBySegment[i];
        dst->blinkOffTilesetCapEndBySegment[i] = src->blinkOffTilesetCapEndBySegment[i];
        dst->blinkOffTilesetCapStartBreakBySegment[i] = src->blinkOffTilesetCapStartBreakBySegment[i];
        dst->blinkOffTilesetCapStartTrailBySegment[i] = src->blinkOffTilesetCapStartTrailBySegment[i];
    }

    /* Opposite fill direction from source */
    if (src->fillIndexByCell[0] == 0)
        GaugeLayout_setFillReverse(dst);
    else
        GaugeLayout_setFillForward(dst);


    /* Copy base visual properties */
    dst->orientation = src->orientation;
    dst->paletteLine = src->paletteLine;
    dst->priority = src->priority;

    /* Set flip based on orientation */
    if (src->orientation == GAUGE_ORIENT_HORIZONTAL)
    {
        dst->hflip = 1;
        dst->vflip = 0;
    }
    else
    {
        dst->hflip = 0;
        dst->vflip = 1;
    }
}

void GaugeLayout_setCaps(GaugeLayout *layout,
                         const u32 * const *capStartTilesets,
                         const u32 * const *capEndTilesets,
                         const u32 * const *capStartBreakTilesets,
                         const u32 * const *capStartTrailTilesets,
                         const u8 *capEndBySegment)
{
    for (u8 i = 0; i < GAUGE_MAX_SEGMENTS; i++)
    {
        layout->tilesetCapStartBySegment[i] = capStartTilesets ? capStartTilesets[i] : NULL;
        layout->tilesetCapEndBySegment[i] = capEndTilesets ? capEndTilesets[i] : NULL;
        layout->tilesetCapStartBreakBySegment[i] =
            capStartBreakTilesets ? capStartBreakTilesets[i] : NULL;
        layout->tilesetCapStartTrailBySegment[i] =
            capStartTrailTilesets ? capStartTrailTilesets[i] : NULL;
        layout->capEndBySegment[i] = capEndBySegment ? (capEndBySegment[i] ? 1 : 0) : 0;
    }
}

void GaugeLayout_setBlinkOff(GaugeLayout *layout,
                             const u32 * const *blinkOffBodyTilesets,
                             const u32 * const *blinkOffEndTilesets,
                             const u32 * const *blinkOffBreakTilesets,
                             const u32 * const *blinkOffTrailTilesets,
                             const u32 * const *blinkOffBridgeTilesets,
                             const u32 * const *blinkOffCapStartTilesets,
                             const u32 * const *blinkOffCapEndTilesets,
                             const u32 * const *blinkOffCapStartBreakTilesets,
                             const u32 * const *blinkOffCapStartTrailTilesets)
{
    for (u8 i = 0; i < GAUGE_MAX_SEGMENTS; i++)
    {
        layout->blinkOffTilesetBySegment[i] =
            blinkOffBodyTilesets ? blinkOffBodyTilesets[i] : NULL;
        layout->blinkOffTilesetEndBySegment[i] =
            blinkOffEndTilesets ? blinkOffEndTilesets[i] : NULL;
        layout->blinkOffTilesetBreakBySegment[i] =
            blinkOffBreakTilesets ? blinkOffBreakTilesets[i] : NULL;
        layout->blinkOffTilesetTrailBySegment[i] =
            blinkOffTrailTilesets ? blinkOffTrailTilesets[i] : NULL;
        layout->blinkOffTilesetBridgeBySegment[i] =
            blinkOffBridgeTilesets ? blinkOffBridgeTilesets[i] : NULL;
        layout->blinkOffTilesetCapStartBySegment[i] =
            blinkOffCapStartTilesets ? blinkOffCapStartTilesets[i] : NULL;
        layout->blinkOffTilesetCapEndBySegment[i] =
            blinkOffCapEndTilesets ? blinkOffCapEndTilesets[i] : NULL;
        layout->blinkOffTilesetCapStartBreakBySegment[i] =
            blinkOffCapStartBreakTilesets ? blinkOffCapStartBreakTilesets[i] : NULL;
        layout->blinkOffTilesetCapStartTrailBySegment[i] =
            blinkOffCapStartTrailTilesets ? blinkOffCapStartTrailTilesets[i] : NULL;
    }
}


/* =============================================================================
   GaugeLogic implementation
   ============================================================================= */

void GaugeLogic_init(GaugeLogic *logic,
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

void GaugeLogic_initWithAnim(GaugeLogic *logic,
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
void GaugeLogic_tick(GaugeLogic *logic)
{
    /* --- Value animation (if enabled) --- */
    if (logic->valueAnimEnabled && logic->valuePixels != logic->valueTargetPixels)
    {
        if (logic->valuePixels < logic->valueTargetPixels)
        {
            /* Increasing (heal animation) */
            const u16 diff = (u16)(logic->valueTargetPixels - logic->valuePixels);
            const u16 step = CALC_ANIM_STEP(diff, logic->valueAnimShift);
            logic->valuePixels = (u16)(logic->valuePixels + step);

            if (logic->valuePixels > logic->valueTargetPixels)
                logic->valuePixels = logic->valueTargetPixels;
            if (logic->valuePixels > logic->maxFillPixels)
                logic->valuePixels = logic->maxFillPixels;
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
 *   | Empty tile (0,0)  |  1 tile  Ã¢â‚¬â€ value=0, trail=0
 *   +-------------------+
 *   | Full value (8,8)  |  1 tile  Ã¢â‚¬â€ value=8, trail=8
 *   +-------------------+
 *   | Full trail (0,8)  |  1 tile  Ã¢â‚¬â€ value=0, trail=8  (only if trailEnabled)
 *   +-------------------+
 *
 *   Partial tiles (1 per GaugePart, shared across all segments):
 *   +-------------------+
 *   | Partial value     |  1 tile  Ã¢â‚¬â€ streamed on demand (also "both" case)
 *   +-------------------+
 *   | Partial trail     |  1 tile  Ã¢â‚¬â€ streamed on demand (only if trailEnabled)
 *   +-------------------+
 *   | Partial END       |  1 tile  Ã¢â‚¬â€ cap tile (only if any segment has END)
 *   +-------------------+
 *   | Partial trail 2nd |  1 tile  Ã¢â‚¬â€ 2nd trail break (only if trail + END)
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
static void init_dynamic_vram(GaugeDynamic *dyn, const GaugeLayout *layout, u16 vramBase, u8 trailEnabled)
{
    u16 nextVram = vramBase;
    u8 hasEndTileset = 0;
    u8 bridgeCount = 0;
    u8 capStartEnabled = 0;
    u8 capEndEnabled = 0;

    /* Initialize cache to invalid */
    for (u8 i = 0; i < GAUGE_MAX_SEGMENTS; i++)
    {
        dyn->vramTileEmpty[i] = 0;
        dyn->vramTileFullValue[i] = 0;
        dyn->vramTileFullTrail[i] = 0;
        dyn->vramTileBridge[i] = 0;
        dyn->loadedFillIdxBridge[i] = CACHE_INVALID_U8;
    }

    dyn->vramTileCapStart = 0;
    dyn->vramTileCapEnd = 0;

    dyn->loadedSegmentPartialValue = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialValue = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrail = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialTrail = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialEnd = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialEnd = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrailSecond = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialTrailSecond = CACHE_INVALID_U8;
    dyn->loadedFillIdxCapStart = CACHE_INVALID_U8;
    dyn->loadedFillIdxCapEnd = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesBreak = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesTrail = CACHE_INVALID_U8;

    /* Initialize tilemap cache */
    for (u8 i = 0; i < GAUGE_MAX_LENGTH; i++)
    {
        dyn->cellCurrentTileIndex[i] = CACHE_INVALID_U16;
    }

    /* Determine which segments are used */
    u8 segmentUsed[GAUGE_MAX_SEGMENTS] = {0};
    for (u8 i = 0; i < layout->length; i++)
    {
        const u8 segId = layout->segmentIdByCell[i];
        if (layout->tilesetBySegment[segId])
        {
            segmentUsed[segId] = 1;
        }
    }

    /* Determine if any used segment has END tiles */
    for (u8 segId = 0; segId < GAUGE_MAX_SEGMENTS; segId++)
    {
        if (segmentUsed[segId] && layout->tilesetEndBySegment[segId])
            hasEndTileset = 1;
        if (segmentUsed[segId] && layout->tilesetBridgeBySegment[segId])
            bridgeCount++;
    }

    /* Determine if caps are enabled for this part */
    detect_caps_enabled(layout, &capStartEnabled, &capEndEnabled);

    /* Allocate standard tiles for each used segment */
    for (u8 segId = 0; segId < GAUGE_MAX_SEGMENTS; segId++)
    {
        if (!segmentUsed[segId])
            continue;

        /* Empty tile (0,0) */
        dyn->vramTileEmpty[segId] = nextVram;
        nextVram++;

        /* Full value tile (8,8) */
        dyn->vramTileFullValue[segId] = nextVram;
        nextVram++;

        /* Full trail tile (0,8) - only if trail enabled */
        if (trailEnabled)
        {
            dyn->vramTileFullTrail[segId] = nextVram;
            nextVram++;
        }

        /* Bridge tile (per segment) */
        if (layout->tilesetBridgeBySegment[segId])
        {
            dyn->vramTileBridge[segId] = nextVram;
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
}

/**
 * Preload standard tiles for dynamic mode.
 */
static void preload_dynamic_standard_tiles(GaugeDynamic *dyn, const GaugeLayout *layout, u8 trailEnabled)
{
    const u8 emptyIndex = s_fillTileStripsIndexByValueTrail[0][0];       /* value=0, trail=0 */
    const u8 fullValueIndex = s_fillTileStripsIndexByValueTrail[8][8];   /* value=8, trail=8 */
    const u8 fullTrailIndexBody = s_fillTileStripsIndexByValueTrail[0][8];   /* value=0, trail=8 */
    const u8 fullTrailIndexTrail = s_trailTileStripsIndexByValueTrail[0][8]; /* value=0, trail=8 */

    for (u8 segId = 0; segId < GAUGE_MAX_SEGMENTS; segId++)
    {
        const u32 *bodyStrip = layout->tilesetBySegment[segId];
        if (!bodyStrip)
            continue;

        /* Upload empty tile */
        if (dyn->vramTileEmpty[segId] != 0)
            upload_fill_tile(bodyStrip, emptyIndex, dyn->vramTileEmpty[segId], DMA_QUEUE);

        /* Upload full value tile */
        if (dyn->vramTileFullValue[segId] != 0)
            upload_fill_tile(bodyStrip, fullValueIndex, dyn->vramTileFullValue[segId], DMA_QUEUE);

        /* Upload full trail tile (only if trail enabled) */
        if (trailEnabled && dyn->vramTileFullTrail[segId] != 0)
        {
            const u32 *trailStrip = layout->tilesetTrailBySegment[segId];
            if (trailStrip)
                upload_fill_tile(trailStrip, fullTrailIndexTrail, dyn->vramTileFullTrail[segId], DMA_QUEUE);
            else
                upload_fill_tile(bodyStrip, fullTrailIndexBody, dyn->vramTileFullTrail[segId], DMA_QUEUE);
        }
    }

    /* Preload cap end tile (index 0 of END strip) if enabled */
    if (dyn->vramTileCapEnd != 0)
    {
        const u8 cellEnd = layout->cellIndexByFillIndex[layout->length - 1];
        const u8 endSegId = layout->segmentIdByCell[cellEnd];
        const u32 *capEndStrip = layout->tilesetCapEndBySegment[endSegId];
        if (capEndStrip)
            upload_fill_tile(capEndStrip, emptyIndex, dyn->vramTileCapEnd, DMA);
    }
}

/**
 * Reset dynamic caches that depend on blink state.
 */
static void reset_dynamic_blink_cache(GaugeDynamic *dyn)
{
    dyn->loadedSegmentPartialValue = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialValue = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrail = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialTrail = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialEnd = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialEnd = CACHE_INVALID_U8;
    dyn->loadedSegmentPartialTrailSecond = CACHE_INVALID_U8;
    dyn->loadedFillIdxPartialTrailSecond = CACHE_INVALID_U8;
    dyn->loadedFillIdxCapStart = CACHE_INVALID_U8;
    dyn->loadedFillIdxCapEnd = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesBreak = CACHE_INVALID_U8;
    dyn->loadedCapStartUsesTrail = CACHE_INVALID_U8;

    for (u8 i = 0; i < GAUGE_MAX_SEGMENTS; i++)
    {
        dyn->loadedFillIdxBridge[i] = CACHE_INVALID_U8;
    }
}

/**
 * Reload full trail tiles when blink state toggles (dynamic mode).
 */
static void reload_dynamic_full_trail_tiles(GaugeDynamic *dyn,
                                            const GaugeLayout *layout,
                                            u8 useBlinkOff)
{
    const u8 fullTrailIndexBody = s_fillTileStripsIndexByValueTrail[0][8];
    const u8 fullTrailIndexTrail = s_trailTileStripsIndexByValueTrail[0][8];

    for (u8 segId = 0; segId < GAUGE_MAX_SEGMENTS; segId++)
    {
        const u16 vramTile = dyn->vramTileFullTrail[segId];
        if (vramTile == 0)
            continue;

        if (useBlinkOff && layout->blinkOffTilesetTrailBySegment[segId])
        {
            upload_fill_tile(layout->blinkOffTilesetTrailBySegment[segId],
                             fullTrailIndexTrail, vramTile, DMA);
        }
        else
        {
            const u32 *trailStrip = layout->tilesetTrailBySegment[segId];
            const u32 *bodyStrip = layout->tilesetBySegment[segId];
            if (trailStrip)
                upload_fill_tile(trailStrip, fullTrailIndexTrail, vramTile, DMA);
            else if (bodyStrip)
                upload_fill_tile(bodyStrip, fullTrailIndexBody, vramTile, DMA);
        }
    }
}

/**
 * Initialize tilemap positions and write initial tilemap (all empty).
 */
static void init_dynamic_tilemap(GaugePart *part)
{
    GaugeLayout *layout = &part->layout;
    GaugeDynamic *dyn = &part->dyn;

    /* Pre-calculate tilemap positions and cell validity for each cell */
    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        compute_tile_xy(layout->orientation, part->originX, part->originY, cellIndex,
                        &layout->tilemapPosByCell[cellIndex].x,
                        &layout->tilemapPosByCell[cellIndex].y);

        /* Pre-compute cell validity (avoids tileset NULL check in render loop) */
        const u8 segId = layout->segmentIdByCell[cellIndex];
        dyn->cellValid[cellIndex] = (layout->tilesetBySegment[segId] != NULL) ? 1 : 0;
    }

    /* Write initial tilemap (all empty tiles) */
    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        if (!dyn->cellValid[cellIndex])
            continue;

        const u8 segId = layout->segmentIdByCell[cellIndex];
        const u16 vramTile = dyn->vramTileEmpty[segId];
        const u16 attr = TILE_ATTR_FULL(layout->paletteLine, layout->priority,
                                        layout->vflip, layout->hflip, vramTile);

        VDP_setTileMapXY(WINDOW, attr,
                         layout->tilemapPosByCell[cellIndex].x,
                         layout->tilemapPosByCell[cellIndex].y);

        /* Initialize cache to avoid redundant writes on first update */
        dyn->cellCurrentTileIndex[cellIndex] = vramTile;
    }
}

/**
 * Initialize tilemap for fixed mode (one VRAM tile per cell).
 */
static void write_tilemap_fixed_init(GaugePart *part)
{
    const GaugeLayout *layout = &part->layout;
    part->cellCount = 0;

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segId = layout->segmentIdByCell[cellIndex];
        const u32 *bodyStrip = layout->tilesetBySegment[segId];

        if (!bodyStrip)
            continue;

        if (part->cellCount >= GAUGE_MAX_LENGTH)
            break;

        const u16 vramTile = (u16)(part->vramBase + part->cellCount);
        const u16 attr = TILE_ATTR_FULL(layout->paletteLine, layout->priority,
                                        layout->vflip, layout->hflip, vramTile);

        u16 x, y;
        compute_tile_xy(layout->orientation, part->originX, part->originY, cellIndex, &x, &y);

        VDP_setTileMapXY(WINDOW, attr, x, y);

        /* Precompute per-cell strips to avoid segment lookups every frame */
        const u32 *endStrip = layout->tilesetEndBySegment[segId];
        const u32 *breakStrip = layout->tilesetBreakBySegment[segId];
        const u32 *trailStrip = layout->tilesetTrailBySegment[segId];
        const u32 *bridgeStrip = layout->tilesetBridgeBySegment[segId];

        /* Fallback rules:
         * - No END => BODY only (ignore BREAK)
         * - END exists + no BREAK => BREAK uses BODY
         */
        if (!endStrip)
        {
            breakStrip = NULL;
        }
        else if (!breakStrip)
        {
            breakStrip = bodyStrip;
        }

        part->cells[part->cellCount].vramTileIndex = vramTile;
        part->cells[part->cellCount].bodyFillStrip45 = bodyStrip;
        part->cells[part->cellCount].endFillStrip45 = endStrip;
        part->cells[part->cellCount].breakFillStrip45 = breakStrip;
        part->cells[part->cellCount].trailFillStrip64 = trailStrip;
        part->cells[part->cellCount].bridgeFillStrip45 = bridgeStrip;
        part->cells[part->cellCount].loadedFillStrip45 = NULL;
        part->cells[part->cellCount].loadedFillIdx = CACHE_INVALID_U8;
        part->cells[part->cellCount].cellIndex = cellIndex;
        part->cellCount++;

    } 
}

/**
 * Process fixed mode: stream tiles for all cells via DMA.
 *
 * Each cell has its own dedicated VRAM tile. When the gauge value changes,
 * only the affected cells get new tile data DMA'd from ROM strips.
 * Change detection per cell (loadedFillIdx + loadedFillStrip45) avoids
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
 *
 * Cost: ~200 cycles per cell (DMA setup dominant), ~50 cycles for trivial cells
 */
static void process_fixed_mode(GaugePart *part,
                               u16 valuePixels,
                               u16 trailPixelsRendered,
                               u16 trailPixelsActual,
                               u8 blinkOffActive)
{
    const GaugeLayout *layout = &part->layout;

    /* --- Break zone computation ---
     * brk: used for normal cell classification (trail with blink applied)
     * brkBridge: used for bridge visibility (trail WITHOUT blink, so bridges
     *            don't flicker on/off during blink phase) */
    GaugeBreakInfo brk;
    compute_break_info(layout, valuePixels, trailPixelsRendered, &brk);
    GaugeBreakInfo brkBridge = brk;
    if (trailPixelsActual != trailPixelsRendered)
        compute_break_info(layout, valuePixels, trailPixelsActual, &brkBridge);

    /* Early-exit boundaries: cells with fillIndex outside [breakLow..breakHigh]
     * are trivially full or empty (~70-80% of cells). Skips full classification. */
    const u8 breakLow = (brk.valueBreakFillIndex != CACHE_INVALID_U8)
                       ? brk.valueBreakFillIndex : brk.valueFillIndex;
    const u8 breakHigh = brk.trailFillIndex;

    /* Cap detection */
    const u8 capStartCellIndex = layout->cellIndexByFillIndex[0];
    const u8 capEndCellIndex = layout->cellIndexByFillIndex[layout->length - 1];
    const u8 capStartSegId = layout->segmentIdByCell[capStartCellIndex];
    const u8 capEndSegId = layout->segmentIdByCell[capEndCellIndex];
    u8 capStartEnabled, capEndEnabled;
    detect_caps_enabled(layout, &capStartEnabled, &capEndEnabled);

    /* Countdown loop: 68000 zero-flag test is free after decrement (dbra) */
    u8 i = part->cellCount;
    while (i--)
    {
        GaugeStreamCell *cell = &part->cells[i];
        const u8 cellIndex = cell->cellIndex;
        const u8 cellFillIndex = layout->fillIndexByCell[cellIndex];
        const u8 segId = layout->segmentIdByCell[cellIndex];

        /* === Cap end (always uses its tileset) === */
        if (cellIndex == capEndCellIndex && capEndEnabled && segId == capEndSegId)
        {
            u8 capValuePx = 0;
            u8 capTrailPx = 0;
            const u32 *capEndStrip = layout->tilesetCapEndBySegment[segId];
            u16 capTrailPixels = trailPixelsRendered;

            if (blinkOffActive &&
                layout->blinkOffTilesetCapEndBySegment[segId] &&
                is_cell_in_trail_zone(&brkBridge, cellFillIndex))
            {
                capEndStrip = layout->blinkOffTilesetCapEndBySegment[segId];
                capTrailPixels = trailPixelsActual;
            }

            compute_fill_for_cell(layout, cellIndex, valuePixels, capTrailPixels,
                                  &capValuePx, &capTrailPx);
            upload_cell_if_needed(cell,
                                  capEndStrip,
                                  s_fillTileStripsIndexByValueTrail[capValuePx][capTrailPx]);
            continue;
        }

        /* === Cap start (fixed border at cell 0) === */
        if (cellIndex == capStartCellIndex && capStartEnabled && segId == capStartSegId)
        {
            if (layout->tilesetCapStartBySegment[segId])
            {
                CapStartResult capResult;
                if (blinkOffActive)
                {
                    CapStartResult capActual;
                    classify_cap_start(layout, &brkBridge, cellFillIndex, segId, &capActual);

                    const u8 inTrailZone = is_cell_in_trail_zone(&brkBridge, cellFillIndex);
                    const u32 *blinkStrip = NULL;

                    if (inTrailZone)
                    {
                        if (capActual.usesTrail)
                            blinkStrip = layout->blinkOffTilesetCapStartTrailBySegment[segId];
                        else if (capActual.usesBreak)
                            blinkStrip = layout->blinkOffTilesetCapStartBreakBySegment[segId];
                        else
                            blinkStrip = layout->blinkOffTilesetCapStartBySegment[segId];
                    }

                    if (blinkStrip)
                    {
                        capActual.strip = blinkStrip;
                        capResult = capActual;
                    }
                    else
                    {
                        classify_cap_start(layout, &brk, cellFillIndex, segId, &capResult);
                    }
                }
                else
                {
                    classify_cap_start(layout, &brk, cellFillIndex, segId, &capResult);
                }
                upload_cell_if_needed(cell, capResult.strip, capResult.fillStripIndex);
                continue;
            }
        }

        /* === Bridge/BREAK forced rendering (D/B before gauge END) ===
         * Bridge cells sit at segment boundaries. When the gauge's value/trail
         * edge is past this cell, the bridge shows a transition tile.
         * brkBridge uses trailPixelsActual (ignoring blink) so bridges don't
         * flicker during blink phase. */
        if (brkBridge.endFillIndex != CACHE_INVALID_U8)
        {
            /* Bridge end cell: last cell of a segment with a bridge tileset */
            if (layout->bridgeEndByFillIndex[cellFillIndex] &&
                brkBridge.endFillIndex > cellFillIndex &&
                brkBridge.valueFillIndex > cellFillIndex)
            {
                const u8 nextFillIndex = (u8)(cellFillIndex + 1);
                const u8 bridgeInTrailZone = is_cell_in_trail_zone(&brkBridge, nextFillIndex);

                if (cell->bridgeFillStrip45)
                {
                    const u8 useBlinkOffBridge = (blinkOffActive &&
                                                  layout->blinkOffTilesetBridgeBySegment[segId] &&
                                                  bridgeInTrailZone);
                    const GaugeBreakInfo *bridgeBrk = useBlinkOffBridge ? &brkBridge : &brk;
                    const u16 bridgeTrailPixels = useBlinkOffBridge ? trailPixelsActual : trailPixelsRendered;
                    const u32 *bridgeStrip = useBlinkOffBridge
                                             ? layout->blinkOffTilesetBridgeBySegment[segId]
                                             : cell->bridgeFillStrip45;
                    const u8 bridgeIdx = compute_bridge_strip_index(
                        layout, bridgeBrk, cellFillIndex, valuePixels, bridgeTrailPixels);
                    upload_cell_if_needed(cell, bridgeStrip, bridgeIdx);
                }
                else
                {
                    /* No bridge tileset: fall back to break or body, fully filled */
                    const u32 *fallbackStrip = cell->breakFillStrip45
                                               ? cell->breakFillStrip45
                                               : cell->bodyFillStrip45;
                    if (blinkOffActive &&
                        layout->blinkOffTilesetBreakBySegment[segId] &&
                        bridgeInTrailZone)
                    {
                        fallbackStrip = layout->blinkOffTilesetBreakBySegment[segId];
                    }
                    upload_cell_if_needed(cell, fallbackStrip,
                                          s_fillTileStripsIndexByValueTrail[8][8]);
                }
                continue;
            }

            /* Forced BREAK cell: cell before a bridge that must show as fully filled */
            if (layout->bridgeBreakByFillIndex[cellFillIndex])
            {
                const u8 boundaryFillIndex = layout->bridgeBreakBoundaryByFillIndex[cellFillIndex];
                if (brkBridge.endFillIndex > boundaryFillIndex &&
                    brkBridge.valueFillIndex > boundaryFillIndex)
                {
                    const u32 *breakStrip = cell->breakFillStrip45
                                            ? cell->breakFillStrip45
                                            : cell->bodyFillStrip45;
                    if (blinkOffActive &&
                        layout->blinkOffTilesetBreakBySegment[segId] &&
                        is_cell_in_trail_zone(&brkBridge, cellFillIndex))
                    {
                        breakStrip = layout->blinkOffTilesetBreakBySegment[segId];
                    }
                    upload_cell_if_needed(cell, breakStrip,
                                          s_fillTileStripsIndexByValueTrail[8][8]);
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
                upload_cell_if_needed(cell, cell->bodyFillStrip45,
                                      s_fillTileStripsIndexByValueTrail[8][8]);
                continue;
            }
            if (cellFillIndex > breakHigh)
            {
                upload_cell_if_needed(cell, cell->bodyFillStrip45,
                                      s_fillTileStripsIndexByValueTrail[0][0]);
                continue;
            }
        }

        /* === Break zone: classify cell by visual mode ===
         * Priority order:
         * 1. END (E)          â€” cap/termination tile
         * 2. TRAIL_BREAK (T)  â€” first trail transition cell (after B)
         * 3. TRAIL_BREAK2 (T) â€” second trail transition (before E)
         * 4. TRAIL_FULL (T)   â€” full trail between value and trail edges
         * 5. VALUE_BREAK (B)  â€” transition cell before value edge
         * 6. DEFAULT          â€” standard fill or region-based rendering
         */
        const GaugeBreakInfo *brkUse = &brk;
        u16 trailPixelsUse = trailPixelsRendered;
        const u32 *bodyStrip = cell->bodyFillStrip45;
        const u32 *endStrip = cell->endFillStrip45;
        const u32 *trailStrip = cell->trailFillStrip64;
        const u32 *breakStrip = cell->breakFillStrip45;
        u8 useBlinkOff = 0;

        if (blinkOffActive)
        {
            const u8 cellIsEndActual = (brkBridge.endFillIndex != CACHE_INVALID_U8 &&
                                        cell->endFillStrip45 &&
                                        cellFillIndex == brkBridge.endFillIndex);
            const u8 cellIsTrailBreakActual =
                (brkBridge.trailBreakActive && cell->trailFillStrip64 &&
                 cellFillIndex == brkBridge.trailBreakFillIndex);
            const u8 cellIsTrailBreak2Actual =
                (brkBridge.trailBreakSecondActive && cell->trailFillStrip64 &&
                 cellFillIndex == brkBridge.trailBreakFillIndex2);
            const u8 cellIsTrailFullActual =
                (brkBridge.trailBreakActive && cell->trailFillStrip64 &&
                 cellFillIndex > brkBridge.valueFillIndex && cellFillIndex < brkBridge.trailFillIndex);
            const u8 cellIsValueBreakActual = (brkBridge.valueBreakFillIndex != CACHE_INVALID_U8 &&
                                               cellFillIndex == brkBridge.valueBreakFillIndex);

            if (cellIsEndActual && layout->blinkOffTilesetEndBySegment[segId])
            {
                useBlinkOff = 1;
                endStrip = layout->blinkOffTilesetEndBySegment[segId];
            }
            else if ((cellIsTrailBreakActual || cellIsTrailBreak2Actual || cellIsTrailFullActual) &&
                     layout->blinkOffTilesetTrailBySegment[segId])
            {
                useBlinkOff = 1;
                trailStrip = layout->blinkOffTilesetTrailBySegment[segId];
            }
            else if (cellIsValueBreakActual && layout->blinkOffTilesetBySegment[segId])
            {
                useBlinkOff = 1;
                bodyStrip = layout->blinkOffTilesetBySegment[segId];
            }

            if (useBlinkOff)
            {
                brkUse = &brkBridge;
                trailPixelsUse = trailPixelsActual;
            }
        }

        const u8 cellIsEnd = (brkUse->endFillIndex != CACHE_INVALID_U8 &&
                              endStrip && cellFillIndex == brkUse->endFillIndex);
        const u8 cellIsTrailBreak =
            (brkUse->trailBreakActive && trailStrip && cellFillIndex == brkUse->trailBreakFillIndex);
        const u8 cellIsTrailBreak2 =
            (brkUse->trailBreakSecondActive && trailStrip && cellFillIndex == brkUse->trailBreakFillIndex2);
        const u8 cellIsTrailFull =
            (brkUse->trailBreakActive && trailStrip &&
             cellFillIndex > brkUse->valueFillIndex && cellFillIndex < brkUse->trailFillIndex);
        const u8 cellIsValueBreak = (brkUse->valueBreakFillIndex != CACHE_INVALID_U8 &&
                                     cellFillIndex == brkUse->valueBreakFillIndex);

        const u32 *stripToUse = bodyStrip;
        u8 desiredStripIndex = 0;

        if (cellIsEnd)
        {
            /* END (E) uses the end strip and the standard fill LUT. */
            stripToUse = endStrip;
            desiredStripIndex = s_fillTileStripsIndexByValueTrail[brkUse->endValuePxInTile][brkUse->endTrailPxInTile];
        }
        else if (cellIsTrailBreak)
        {
        /* TRAIL_BREAK (T, after B):
         * If next cell is full trail, force trailPx=8 for proper raccord.
         */
            stripToUse = trailStrip;
            if (brkUse->trailBreakSecondActive)
                desiredStripIndex = s_trailTileStripsIndexByValueTrail[brkUse->valuePxInBreakCell][8];
            else
                desiredStripIndex = s_trailTileStripsIndexByValueTrail[brkUse->valuePxInBreakCell][brkUse->trailPxInBreakCell];
        }
        else if (cellIsTrailBreak2)
        {
        /* TRAIL_BREAK2 (T, before E): same fill index as END (E) but on TRAIL tileset.
         * Special case: if END index is 8, use trail LUT [0][endIndex].
         */
            stripToUse = trailStrip;
            const u8 endIndex = s_fillTileStripsIndexByValueTrail[brkUse->endValuePxInTile][brkUse->endTrailPxInTile];
            if (endIndex == 8)
                desiredStripIndex = s_trailTileStripsIndexByValueTrail[0][endIndex];
            else
                desiredStripIndex = endIndex;
        }
        else if (cellIsTrailFull)
        {
        /* TRAIL_FULL (T) uses the trail strip, fixed full index (7). */
            stripToUse = trailStrip;
            desiredStripIndex = 7;
        }
        else if (cellIsValueBreak)
        {
        /* VALUE_BREAK (B) always uses BODY (never BREAK tileset).
         * - If trailBreakActive: index = valuePxInBreakCell
         * - Else: index follows END (E) (same fill as break)
         */
            stripToUse = bodyStrip;
            if (brkUse->trailBreakActive)
                desiredStripIndex = brkUse->valuePxInBreakCell;
            else
                desiredStripIndex = s_fillTileStripsIndexByValueTrail[brkUse->endValuePxInTile][brkUse->endTrailPxInTile];
        }
        else
        {
            if (brkUse->regionRenderActive)
            {
                /* Region-based rendering when trail is active. */
                if (cellFillIndex < brkUse->valueFillIndex)
                {
                    /* BODY full */
                    stripToUse = bodyStrip;
                    desiredStripIndex = s_fillTileStripsIndexByValueTrail[8][8]; /* 44 */
                }
                else if (cellFillIndex > brkUse->trailFillIndex)
                {
                    /* BODY empty */
                    stripToUse = bodyStrip;
                    desiredStripIndex = s_fillTileStripsIndexByValueTrail[0][0]; /* 0 */
                }
                else if (cellFillIndex > brkUse->valueFillIndex && cellFillIndex < brkUse->trailFillIndex)
                {
                    /* TRAIL full */
                    stripToUse = trailStrip;
                    desiredStripIndex = 7;
                }
            } 
            else
            {
                /* Default behavior: standard fill computation (no T/B/E special cases). */
                if (brkUse->endFillIndex != CACHE_INVALID_U8)
                {
                    if (cellFillIndex == brkUse->endFillIndex)
                        stripToUse = endStrip;
                    else if (cellFillIndex == brkUse->valueBreakFillIndex && breakStrip)
                        stripToUse = breakStrip;
                }

                u8 valuePxInTile = 0;
                u8 trailPxInTile = 0;

                if (stripToUse == breakStrip)
                {
                    /* BREAK should track END evolution (same fill as break). */
                    valuePxInTile = brkUse->endValuePxInTile;
                    trailPxInTile = brkUse->endTrailPxInTile;
                }
                else
                {
                    compute_fill_for_cell(layout, cell->cellIndex,
                                          valuePixels, trailPixelsUse,
                                          &valuePxInTile, &trailPxInTile);
                }

                desiredStripIndex = s_fillTileStripsIndexByValueTrail[valuePxInTile][trailPxInTile];
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
 *
 * Cost: ~50 cycles for trivial cells, ~300-500 cycles for break zone cells
 */
static void process_dynamic_mode(GaugePart *part,
                                 u16 valuePixels,
                                 u16 trailPixelsRendered,
                                 u16 trailPixelsActual,
                                 u8 blinkOffActive,
                                 u8 blinkOnChanged)
{
    GaugeDynamic *dyn = &part->dyn;
    const GaugeLayout *layout = &part->layout;

    /* Pre-compute tilemap attribute base (without tile index) */
    const u16 attrBase = TILE_ATTR_FULL(layout->paletteLine, layout->priority,
                                        layout->vflip, layout->hflip, 0);

    /* --- Break zone computation ---
     * brk: used for normal cell classification (trail with blink applied)
     * brkBridge: used for bridge visibility (trail WITHOUT blink, so bridges
     *            don't flicker on/off during blink phase) */
    GaugeBreakInfo brk;
    compute_break_info(layout, valuePixels, trailPixelsRendered, &brk);
    GaugeBreakInfo brkBridge = brk;
    if (trailPixelsActual != trailPixelsRendered)
        compute_break_info(layout, valuePixels, trailPixelsActual, &brkBridge);

    if (blinkOnChanged && layout_has_blink_off(layout))
    {
        reset_dynamic_blink_cache(dyn);
        reload_dynamic_full_trail_tiles(dyn, layout, blinkOffActive);
    }

    /* Early-exit boundaries: cells outside [breakLow..breakHigh] are trivially
     * full or empty. Same logic as fixed mode. */
    const u8 breakLow = (brk.valueBreakFillIndex != CACHE_INVALID_U8)
                       ? brk.valueBreakFillIndex : brk.valueFillIndex;
    const u8 breakHigh = brk.trailFillIndex;

    /* Debug: confirm dynamic blink path + break info */
    /* Cap detection */
    const u8 capStartCellIndex = layout->cellIndexByFillIndex[0];
    const u8 capEndCellIndex = layout->cellIndexByFillIndex[layout->length - 1];
    const u8 capStartSegId = layout->segmentIdByCell[capStartCellIndex];
    const u8 capEndSegId = layout->segmentIdByCell[capEndCellIndex];
    u8 capStartEnabled, capEndEnabled;
    detect_caps_enabled(layout, &capStartEnabled, &capEndEnabled);
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
        const u8 segId = layout->segmentIdByCell[cellIndex];

        /* === Cap end (always uses its tileset) === */
        if (cellIndex == capEndCellIndex && capEndEnabled && segId == capEndSegId)
        {
            u8 capValuePx = 0;
            u8 capTrailPx = 0;
            const u32 *capEndStrip = layout->tilesetCapEndBySegment[segId];
            u16 capTrailPixels = trailPixelsRendered;

            if (blinkOffActive &&
                layout->blinkOffTilesetCapEndBySegment[segId] &&
                is_cell_in_trail_zone(&brkBridge, cellFillIndex))
            {
                capEndStrip = layout->blinkOffTilesetCapEndBySegment[segId];
                capTrailPixels = trailPixelsActual;
            }

            compute_fill_for_cell(layout, cellIndex, valuePixels, capTrailPixels,
                                  &capValuePx, &capTrailPx);
            const u8 fillStripIndex = s_fillTileStripsIndexByValueTrail[capValuePx][capTrailPx];
            if (dyn->loadedFillIdxCapEnd != fillStripIndex)
            {
                upload_fill_tile(capEndStrip,
                                 fillStripIndex,
                                 dyn->vramTileCapEnd,
                                 DMA);
                dyn->loadedFillIdxCapEnd = fillStripIndex;
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
        if (cellIndex == capStartCellIndex && capStartEnabled && segId == capStartSegId)
        {
            if (layout->tilesetCapStartBySegment[segId])
            {
                CapStartResult capResult;
                if (blinkOffActive)
                {
                    CapStartResult capActual;
                    classify_cap_start(layout, &brkBridge, cellFillIndex, segId, &capActual);

                    const u8 inTrailZone = is_cell_in_trail_zone(&brkBridge, cellFillIndex);
                    const u32 *blinkStrip = NULL;

                    if (inTrailZone)
                    {
                        if (capActual.usesTrail)
                            blinkStrip = layout->blinkOffTilesetCapStartTrailBySegment[segId];
                        else if (capActual.usesBreak)
                            blinkStrip = layout->blinkOffTilesetCapStartBreakBySegment[segId];
                        else
                            blinkStrip = layout->blinkOffTilesetCapStartBySegment[segId];
                    }

                    if (blinkStrip)
                    {
                        capActual.strip = blinkStrip;
                        capResult = capActual;
                    }
                    else
                    {
                        classify_cap_start(layout, &brk, cellFillIndex, segId, &capResult);
                    }
                }
                else
                {
                    classify_cap_start(layout, &brk, cellFillIndex, segId, &capResult);
                }

                /* Stream cap start tile if strip/index/variant changed */
                if (dyn->loadedFillIdxCapStart != capResult.fillStripIndex ||
                    dyn->loadedCapStartUsesBreak != capResult.usesBreak ||
                    dyn->loadedCapStartUsesTrail != capResult.usesTrail)
                {
                    upload_fill_tile(capResult.strip, capResult.fillStripIndex,
                                     dyn->vramTileCapStart, DMA);
                    dyn->loadedFillIdxCapStart = capResult.fillStripIndex;
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
         * brkBridge uses trailPixelsActual (ignoring blink) so bridges don't
         * flicker during blink phase. */
        if (brkBridge.endFillIndex != CACHE_INVALID_U8)
        {
            /* Bridge end cell: last cell of a segment with a bridge tileset */
            if (layout->bridgeEndByFillIndex[cellFillIndex] &&
                brkBridge.endFillIndex > cellFillIndex &&
                brkBridge.valueFillIndex > cellFillIndex)
            {
                const u16 vramTileBridge = dyn->vramTileBridge[segId];
                if (vramTileBridge != 0)
                {
                    const u8 nextFillIndex = (u8)(cellFillIndex + 1);
                    const u8 bridgeInTrailZone = is_cell_in_trail_zone(&brkBridge, nextFillIndex);
                    const u8 useBlinkOffBridge = (blinkOffActive &&
                                                  layout->blinkOffTilesetBridgeBySegment[segId] &&
                                                  bridgeInTrailZone);
                    const GaugeBreakInfo *bridgeBrk = useBlinkOffBridge ? &brkBridge : &brk;
                    const u16 bridgeTrailPixels = useBlinkOffBridge ? trailPixelsActual : trailPixelsRendered;
                    const u32 *bridgeStrip = useBlinkOffBridge
                                             ? layout->blinkOffTilesetBridgeBySegment[segId]
                                             : layout->tilesetBridgeBySegment[segId];
                    const u8 bridgeIdx = compute_bridge_strip_index(
                        layout, bridgeBrk, cellFillIndex, valuePixels, bridgeTrailPixels);

                    if (dyn->loadedFillIdxBridge[segId] != bridgeIdx)
                    {
                        upload_fill_tile(bridgeStrip,
                                         bridgeIdx, vramTileBridge, DMA);
                        dyn->loadedFillIdxBridge[segId] = bridgeIdx;
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
                if (brkBridge.endFillIndex > boundaryFillIndex &&
                    brkBridge.valueFillIndex > boundaryFillIndex)
                {
                    const u16 vramTile = dyn->vramTileFullValue[segId];
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
                const u16 vt = dyn->vramTileFullValue[segId];
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
                const u16 vt = dyn->vramTileEmpty[segId];
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
        const GaugeBreakInfo *brkUse = &brk;
        u16 trailPixelsUse = trailPixelsRendered;
        const u32 *bodyStrip = layout->tilesetBySegment[segId];
        const u32 *endStrip = layout->tilesetEndBySegment[segId];
        const u32 *trailStrip = layout->tilesetTrailBySegment[segId];
        u8 useBlinkOff = 0;

        const u8 cellHasEndNormal = (endStrip != NULL);
        const u8 cellHasTrailNormal = (trailStrip != NULL);

        if (blinkOffActive)
        {
            const u8 cellIsEndActual = (brkBridge.endFillIndex != CACHE_INVALID_U8 &&
                                        cellHasEndNormal && cellFillIndex == brkBridge.endFillIndex);
            const u8 cellIsTrailBreakActual =
                (brkBridge.trailBreakActive && cellHasTrailNormal && cellFillIndex == brkBridge.trailBreakFillIndex);
            const u8 cellIsTrailBreak2Actual =
                (brkBridge.trailBreakSecondActive && cellHasTrailNormal && cellFillIndex == brkBridge.trailBreakFillIndex2);
            const u8 cellIsTrailFullActual =
                (brkBridge.trailBreakActive && cellHasTrailNormal &&
                 cellFillIndex > brkBridge.valueFillIndex && cellFillIndex < brkBridge.trailFillIndex);
            const u8 cellIsValueBreakActual = (brkBridge.valueBreakFillIndex != CACHE_INVALID_U8 &&
                                               cellFillIndex == brkBridge.valueBreakFillIndex);

            if (cellIsEndActual && layout->blinkOffTilesetEndBySegment[segId])
            {
                useBlinkOff = 1;
                endStrip = layout->blinkOffTilesetEndBySegment[segId];
            }
            else if ((cellIsTrailBreakActual || cellIsTrailBreak2Actual || cellIsTrailFullActual) &&
                     layout->blinkOffTilesetTrailBySegment[segId])
            {
                useBlinkOff = 1;
                trailStrip = layout->blinkOffTilesetTrailBySegment[segId];
            }
            else if (cellIsValueBreakActual && layout->blinkOffTilesetBySegment[segId])
            {
                useBlinkOff = 1;
                bodyStrip = layout->blinkOffTilesetBySegment[segId];
            }

            if (useBlinkOff)
            {
                brkUse = &brkBridge;
                trailPixelsUse = trailPixelsActual;
            }
        }

        const u8 cellHasEnd = (endStrip != NULL);
        const u8 cellHasTrail = (trailStrip != NULL);
        const u32 *trailStripUse = cellHasTrail ? trailStrip : bodyStrip;

        const u8 cellIsEnd = (brkUse->endFillIndex != CACHE_INVALID_U8 &&
                              cellHasEnd && cellFillIndex == brkUse->endFillIndex);
        const u8 cellIsTrailBreak =
            (brkUse->trailBreakActive && cellHasTrail && cellFillIndex == brkUse->trailBreakFillIndex);
        const u8 cellIsTrailBreak2 =
            (brkUse->trailBreakSecondActive && cellHasTrail && cellFillIndex == brkUse->trailBreakFillIndex2);
        const u8 cellIsTrailFull =
            (brkUse->trailBreakActive && cellHasTrail &&
             cellFillIndex > brkUse->valueFillIndex && cellFillIndex < brkUse->trailFillIndex);
        const u8 cellIsValueBreak = (brkUse->valueBreakFillIndex != CACHE_INVALID_U8 &&
                                     cellFillIndex == brkUse->valueBreakFillIndex);

        u16 vramTile = 0;
        u8 needsUpload = 0;
        u8 fillStripIndex = 0;
        const u32 *stripToUse = NULL;

        if (cellIsEnd)
        {
            /* END (E) uses the end strip and the standard fill LUT. */
            stripToUse = endStrip;
            fillStripIndex = s_fillTileStripsIndexByValueTrail[brkUse->endValuePxInTile][brkUse->endTrailPxInTile];
            vramTile = dyn->vramTilePartialEnd;

            if (dyn->loadedSegmentPartialEnd != segId ||
                dyn->loadedFillIdxPartialEnd != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialEnd = segId;
                dyn->loadedFillIdxPartialEnd = fillStripIndex;
            }
        }
        else if (cellIsTrailBreak)
        {
            /* TRAIL_BREAK (T, after B): if next cell is full trail, force trailPx=8. */
            stripToUse = trailStripUse;
            if (brkUse->trailBreakSecondActive)
                fillStripIndex = s_trailTileStripsIndexByValueTrail[brkUse->valuePxInBreakCell][8];
            else
                fillStripIndex = s_trailTileStripsIndexByValueTrail[brkUse->valuePxInBreakCell][brkUse->trailPxInBreakCell];
            vramTile = dyn->vramTilePartialTrail;

            if (dyn->loadedSegmentPartialTrail != segId ||
                dyn->loadedFillIdxPartialTrail != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialTrail = segId;
                dyn->loadedFillIdxPartialTrail = fillStripIndex;
            }
        }
        else if (cellIsTrailBreak2)
        {
            /* TRAIL_BREAK2 (T, before E): same fill index as END (E) but on TRAIL tileset. */
            stripToUse = trailStripUse;
            const u8 endIndex = s_fillTileStripsIndexByValueTrail[brkUse->endValuePxInTile][brkUse->endTrailPxInTile];
            if (endIndex == 8)
                fillStripIndex = s_trailTileStripsIndexByValueTrail[0][endIndex];
            else
                fillStripIndex = endIndex;
            vramTile = dyn->vramTilePartialTrailSecond;

            if (dyn->loadedSegmentPartialTrailSecond != segId ||
                dyn->loadedFillIdxPartialTrailSecond != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialTrailSecond = segId;
                dyn->loadedFillIdxPartialTrailSecond = fillStripIndex;
            }
        }
        else if (cellIsTrailFull)
        {
            /* TRAIL_FULL (T) uses the trail strip, fixed full index (7). */
            vramTile = dyn->vramTileFullTrail[segId];
        }
        else if (cellIsValueBreak)
        {
            /* VALUE_BREAK (B) always uses BODY.
             * - If trailBreakActive: index = valuePxInBreakCell
             * - Else: index follows END (E) (same fill as break)
             */
            stripToUse = bodyStrip;
            if (brkUse->trailBreakActive)
                fillStripIndex = brkUse->valuePxInBreakCell;
            else
                fillStripIndex = s_fillTileStripsIndexByValueTrail[brkUse->endValuePxInTile][brkUse->endTrailPxInTile];
            vramTile = dyn->vramTilePartialValue;

            if (dyn->loadedSegmentPartialValue != segId ||
                dyn->loadedFillIdxPartialValue != fillStripIndex)
            {
                needsUpload = 1;
                dyn->loadedSegmentPartialValue = segId;
                dyn->loadedFillIdxPartialValue = fillStripIndex;
            }
        }
        else if (brkUse->regionRenderActive)
        {
            /* Region-based rendering when trail is active. */
            if (cellFillIndex < brkUse->valueFillIndex)
            {
                /* BODY full */
                vramTile = dyn->vramTileFullValue[segId];
            }
            else if (cellFillIndex > brkUse->trailFillIndex)
            {
                /* BODY empty */
                vramTile = dyn->vramTileEmpty[segId];
            }
            else if (cellFillIndex > brkUse->valueFillIndex && cellFillIndex < brkUse->trailFillIndex)
            {
                /* TRAIL full */
                vramTile = dyn->vramTileFullTrail[segId];
            }
            else
            {
                /* Value/trail break cells (B/T) are handled by special cases above. */
                vramTile = dyn->vramTileEmpty[segId];
            }
        }
        else
        {
            /* Default behavior: standard fill computation (no E/T/B special cases). */
            if (cellFillIndex < brkUse->valueFillIndex)
            {
                /* Full value region */
                vramTile = dyn->vramTileFullValue[segId];
            }
            else if (cellFillIndex > brkUse->valueFillIndex && cellFillIndex < brkUse->trailFillIndex)
            {
                /* Full trail region */
                vramTile = dyn->vramTileFullTrail[segId];
            }
            else if (cellFillIndex > brkUse->trailFillIndex)
            {
                /* Empty region */
                vramTile = dyn->vramTileEmpty[segId];
            }
            else if (cellFillIndex == brkUse->valueFillIndex && cellFillIndex == brkUse->trailFillIndex)
            {
                /* Both value and trail partial in same cell */
                u8 valuePxInTile, trailPxInTile;
                compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsUse,
                                      &valuePxInTile, &trailPxInTile);

                stripToUse = bodyStrip;
                fillStripIndex = s_fillTileStripsIndexByValueTrail[valuePxInTile][trailPxInTile];
                vramTile = dyn->vramTilePartialValue;  /* Reuse PartialValue for "both" case */

                if (dyn->loadedSegmentPartialValue != segId ||
                    dyn->loadedFillIdxPartialValue != fillStripIndex)
                {
                    needsUpload = 1;
                    dyn->loadedSegmentPartialValue = segId;
                    dyn->loadedFillIdxPartialValue = fillStripIndex;
                }
            }
            else if (cellFillIndex == brkUse->valueFillIndex)
            {
                /* Value partial cell (trail is full=8) */
                u8 valuePxInTile, trailPxInTile;
                compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsUse,
                                      &valuePxInTile, &trailPxInTile);

                stripToUse = bodyStrip;
                fillStripIndex = s_fillTileStripsIndexByValueTrail[valuePxInTile][8];
                vramTile = dyn->vramTilePartialValue;

                if (dyn->loadedSegmentPartialValue != segId ||
                    dyn->loadedFillIdxPartialValue != fillStripIndex)
                {
                    needsUpload = 1;
                    dyn->loadedSegmentPartialValue = segId;
                    dyn->loadedFillIdxPartialValue = fillStripIndex;
                }
            }
            else  /* cellFillIndex == brkUse->trailFillIndex */
            {
                /* Trail partial cell (value=0) */
                u8 valuePxInTile, trailPxInTile;
                compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsUse,
                                      &valuePxInTile, &trailPxInTile);

                stripToUse = bodyStrip;
                fillStripIndex = s_fillTileStripsIndexByValueTrail[0][trailPxInTile];
                vramTile = dyn->vramTilePartialTrail;

                if (dyn->loadedSegmentPartialTrail != segId ||
                    dyn->loadedFillIdxPartialTrail != fillStripIndex)
                {
                    needsUpload = 1;
                    dyn->loadedSegmentPartialTrail = segId;
                    dyn->loadedFillIdxPartialTrail = fillStripIndex;
                }
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


/* =============================================================================
   GaugePart initialization (internal)
   ============================================================================= */

/**
 * Initialize a GaugePart with all parameters.
 */
static void GaugePart_initInternal(GaugePart *part,
                                   const Gauge *gauge,
                                   const GaugeLayout *layout,
                                   u16 originX, u16 originY,
                                   u16 vramBase,
                                   GaugeVramMode vramMode)
{
    part->originX = originX;
    part->originY = originY;
    part->vramBase = vramBase;
    part->vramMode = vramMode;

    /* Copy layout (includes visual properties) */
    part->layout = *layout;
    if (part->layout.length == 0) part->layout.length = 1;
    if (part->layout.length > GAUGE_MAX_LENGTH) part->layout.length = GAUGE_MAX_LENGTH;

    /* Initialize based on VRAM mode */
    if (part->vramMode == GAUGE_VRAM_DYNAMIC)
    {
        /* Dynamic mode initialization */
        init_dynamic_vram(&part->dyn, &part->layout, part->vramBase, gauge->logic.trailEnabled);
        preload_dynamic_standard_tiles(&part->dyn, &part->layout, gauge->logic.trailEnabled);
        init_dynamic_tilemap(part);
    }
    else
    {
        /* Fixed mode */
        write_tilemap_fixed_init(part);
    }
}



/* =============================================================================
   Gauge public API
   ============================================================================= */

/**
 * Generate value-to-pixels LUT when maxValue != maxFillPixels.
 * Formula: lut[v] = (v * maxFillPixels + maxValue/2) / maxValue
 * The +maxValue/2 provides rounding instead of truncation.
 */
static u16* generate_value_to_pixels_lut(u16 maxValue, u16 maxFillPixels)
{
    /* Allocate (maxValue + 1) entries */
    u16 *lut = MEM_alloc((maxValue + 1) * sizeof(u16));
    if (!lut) return NULL;

    const u16 halfMax = maxValue >> 1;
    for (u16 v = 0; v <= maxValue; v++)
    {
        const u32 numerator = (u32)v * (u32)maxFillPixels + halfMax;
        lut[v] = (u16)(numerator / maxValue);
    }

    return lut;
}

void Gauge_init(Gauge *gauge,
                u16 maxValue,
                u16 maxFillPixels,
                u16 initialValue,
                GaugePart *parts,
                u16 vramBase,
                GaugeVramMode vramMode)
{
    /* Auto-generate LUT when maxValue != maxFillPixels */
    const u16 *lut = (maxValue != maxFillPixels)
                     ? generate_value_to_pixels_lut(maxValue, maxFillPixels)
                     : NULL;

    /* Initialize embedded logic with generated LUT (or NULL for 1:1 mapping)
     * Trail disabled by default - use Gauge_setTrailAnim() to enable */
    GaugeLogic_init(&gauge->logic, maxValue, maxFillPixels, lut,
                    0, initialValue);  /* trailEnabled=0 by default */

    /* Store gaugePart array reference */
    gauge->parts = parts;
    gauge->partCount = 0;

    /* VRAM allocation state */
    gauge->vramBase = vramBase;
    gauge->vramNextOffset = 0;
    gauge->vramMode = vramMode;
}

void Gauge_setValueAnim(Gauge *gauge, u8 enabled, u8 shift)
{
    GaugeLogic *logic = &gauge->logic;
    logic->valueAnimEnabled = enabled ? 1 : 0;
    logic->valueAnimShift = (shift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : shift;
}

void Gauge_setTrailAnim(Gauge *gauge, u8 enabled, u8 shift, u8 blinkShift)
{
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

void Gauge_addPart(Gauge *gauge,
                   GaugePart *part,
                   const GaugeLayout *layout,
                   u16 originX,
                   u16 originY)
{
    if (gauge->partCount >= GAUGE_MAX_PARTS) return;

    /* Compute VRAM size for this layout */
    const u16 vramSize = Gauge_getVramSize(layout, gauge->vramMode, gauge->logic.trailEnabled);

    /* Allocate VRAM from gauge's pool */
    const u16 vramBase = (u16)(gauge->vramBase + gauge->vramNextOffset);
    gauge->vramNextOffset = (u16)(gauge->vramNextOffset + vramSize);

    /* Initialize part */
    GaugePart_initInternal(part, gauge, layout, originX, originY, vramBase, gauge->vramMode);

    /* Force next update to render (new part needs initial draw) */
    gauge->logic.lastValuePixels = CACHE_INVALID_U16;
    gauge->logic.needUpdate = 1;

    gauge->partCount++;
}

void Gauge_addPartEx(Gauge *gauge,
                     GaugePart *part,
                     const GaugeLayout *layout,
                     u16 originX,
                     u16 originY,
                     u16 vramBase,
                     GaugeVramMode vramMode)
{
    if (gauge->partCount >= GAUGE_MAX_PARTS) return;

    /* Initialize part with custom VRAM settings */
    GaugePart_initInternal(part, gauge, layout, originX, originY, vramBase, vramMode);

    /* Force next update to render */
    gauge->logic.lastValuePixels = CACHE_INVALID_U16;
    gauge->logic.needUpdate = 1;

    gauge->partCount++;
}

/**
 * Gauge_update -- Update gauge: tick logic + render all parts.
 * Call once per frame in the game loop.
 *
 * Execution flow:
 * 1. GaugeLogic_tick() Ã¢â‚¬â€ advance value/trail animations
 * 2. Compute render state (valuePixels, trailPixelsRendered, blinkOn)
 * 3. Change detection: compare against lastValuePixels, lastTrailPixelsRendered,
 *    lastBlinkOn, and check if value animation is still converging
 * 4. If nothing changed Ã¢â€ â€™ early return (zero CPU cost after initial comparison)
 * 5. If changed Ã¢â€ â€™ render all parts via process_fixed_mode/process_dynamic_mode
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
void Gauge_update(Gauge *gauge)
{
    GaugeLogic *logic = &gauge->logic;

    /* Skip entirely when gauge is idle (no animations, no pending render).
     * Cost: ~8 cycles (load byte + branch) vs ~70-90 cycles for the full path. */
    if (!logic->needUpdate) return;

    /* Tick logic state machine */
    GaugeLogic_tick(logic);
    
    /* --- Compute current render state --- */
    const u16 valuePixels = logic->valuePixels;

    /* Compute trail with blink effect */
    u16 trailPixels = logic->trailPixels;
    if (trailPixels < valuePixels)
        trailPixels = valuePixels;

    /* Compute blink state from internal timer */
    u8 blinkOn = 1;
    if (logic->trailEnabled && logic->blinkFramesRemaining > 0)
    {
        blinkOn = (u8)(((logic->blinkTimer >> logic->blinkShift) & 1) == 0);
    }

    const u8 previousBlinkOn = logic->lastBlinkOn;
    const u8 blinkOnChanged = (previousBlinkOn != blinkOn);
    const u8 blinkOffActive = (logic->trailEnabled &&
                               logic->blinkFramesRemaining > 0 &&
                               blinkOn == 0);

    /* Apply blink to trail.
     * When trail is disabled, trailPixels == valuePixels (enforced by GaugeLogic_tick),
     * and blinkOn == 1 (no blink frames), so trailPixelsRendered == valuePixels naturally. */
    const u16 trailPixelsRendered = blinkOn ? trailPixels : valuePixels;

    /* --- Early return if nothing changed --- */
    if (logic->lastValuePixels == valuePixels &&
        logic->lastTrailPixelsRendered == trailPixelsRendered &&
        logic->lastBlinkOn == blinkOn &&
        logic->valueTargetPixels == valuePixels)
    {
        return;
    }

    /* Update render cache */
    logic->lastValuePixels = valuePixels;
    logic->lastTrailPixelsRendered = trailPixelsRendered;
    logic->lastBlinkOn = blinkOn;

    /* Detect fully idle state: value at target, trail caught up, no timers active.
     * We still render this frame (the final converged state), then next call to
     * Gauge_update will return immediately via the needUpdate check above. */
    if (valuePixels == logic->valueTargetPixels &&
        trailPixels == valuePixels &&
        logic->holdFramesRemaining == 0 &&
        logic->blinkFramesRemaining == 0)
    {
        logic->needUpdate = 0;
    }

    /* --- Render all parts (countdown: 68000 dbra optimization) --- */
    u8 i = gauge->partCount;
    while (i--)
    {
        GaugePart *part = &gauge->parts[i];

        if (part->vramMode == GAUGE_VRAM_DYNAMIC)
        {
            process_dynamic_mode(part, valuePixels, trailPixelsRendered, trailPixels,
                                 blinkOffActive, blinkOnChanged);
        }
        else
        {
            process_fixed_mode(part, valuePixels, trailPixelsRendered, trailPixels,
                               blinkOffActive);
        }
    }
}

void Gauge_setValue(Gauge *gauge, u16 newValue)
{
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
}

void Gauge_decrease(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames)
{
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
        return;
    }

    /* Trail stays at previous position (damage effect) */
    const u16 previousDisplayedValuePixels = logic->valuePixels;
    if (logic->trailPixels < previousDisplayedValuePixels)
        logic->trailPixels = previousDisplayedValuePixels;

    /* Start hold/blink sequence */
    logic->holdFramesRemaining = holdFrames;
    logic->blinkFramesRemaining = blinkFrames;
    logic->blinkTimer = 0;
}

void Gauge_increase(Gauge *gauge, u16 amount)
{
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

    /* Trail follows value immediately on heal */
    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->blinkTimer = 0;
}


/* =============================================================================
   Utility functions
   ============================================================================= */

u16 Gauge_getVramSize(const GaugeLayout *layout,
                      GaugeVramMode vramMode,
                      u8 trailEnabled)
{
    if (vramMode == GAUGE_VRAM_FIXED)
    {
        /* Fixed: one VRAM tile per cell with valid tileset */
        u16 count = 0;
        for (u8 i = 0; i < layout->length; i++)
        {
            const u8 segId = layout->segmentIdByCell[i];
            if (layout->tilesetBySegment[segId])
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
        const u8 segId = layout->segmentIdByCell[cellIndex];
        if (layout->tilesetBySegment[segId] && !segmentUsed[segId])
        {
            segmentUsed[segId] = 1;
            segmentCount++;
        }
    }

    for (u8 segId = 0; segId < GAUGE_MAX_SEGMENTS; segId++)
    {
        if (segmentUsed[segId] && layout->tilesetEndBySegment[segId])
            hasEndTileset = 1;
        if (segmentUsed[segId] && layout->tilesetBridgeBySegment[segId])
            bridgeCount++;
    }

    detect_caps_enabled(layout, &capStartEnabled, &capEndEnabled);

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





