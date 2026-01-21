#include "gauge.h"

/* (valuePxInTile, trailPxInTile) -> idx 0..44 */
static const u8 s_fillIdxByValueTrail[9][9] =
{
    { 0, 1, 2, 3, 4, 5, 6, 7, 8 },
    { 0, 9,10,11,12,13,14,15,16 },
    { 0, 0,17,18,19,20,21,22,23 },
    { 0, 0, 0,24,25,26,27,28,29 },
    { 0, 0, 0, 0,30,31,32,33,34 },
    { 0, 0, 0, 0, 0,35,36,37,38 },
    { 0, 0, 0, 0, 0, 0,39,40,41 },
    { 0, 0, 0, 0, 0, 0, 0,42,43 },
    { 0, 0, 0, 0, 0, 0, 0, 0,44 },
};

static inline u16 clamp_u16(u16 v, u16 lo, u16 hi)
{
    if (v < lo) return lo;
    if (v > hi) return hi;
    return v;
}

static inline u16 sat_sub_u16(u16 a, u16 b)
{
    return (b >= a) ? 0 : (u16)(a - b);
}

static inline u16 sat_add_u16(u16 a, u16 b, u16 maxVal)
{
    u32 s = (u32)a + (u32)b;
    return (s >= (u32)maxVal) ? maxVal : (u16)s;
}

static inline u8 clamp_px_0_8(s16 v)
{
    if (v <= 0) return 0;
    if (v >= 8) return 8;
    return (u8)v;
}

static inline u16 value_to_pixels(const GaugeLogic *logic, u16 value)
{
    if (logic->valueToPixelsLUT) return logic->valueToPixelsLUT[value];
    return value;
}

/* =============================================================================
   GaugeLayout
   ============================================================================= */

void GaugeLayout_setFillForward(GaugeLayout *layout)
{
    for (u8 c = 0; c < layout->lengthTiles; c++)
        layout->fillIndexByCell[c] = c;
}

void GaugeLayout_setFillReverse(GaugeLayout *layout)
{
    for (u8 c = 0; c < layout->lengthTiles; c++)
        layout->fillIndexByCell[c] = (u8)(layout->lengthTiles - 1 - c);
}

void GaugeLayout_buildLaneListsFromRom(GaugeLayout *layout, const GaugeRom *rom)
{
    layout->lane0CellCount = 0;
    layout->lane1CellCount = 0;

    if (!rom) return;

    for (u8 cell = 0; cell < layout->lengthTiles; cell++)
    {
        const u8 seg = layout->segmentIdByCell[cell];
        if (seg >= rom->segmentCount) continue;

        const u8 mask = rom->segmentLaneMask[seg];

        if ((mask & GAUGE_LANE_MASK_0) && rom->fillTileStrips[seg][GAUGE_LANE_0])
        {
            layout->lane0CellList[layout->lane0CellCount++] = cell;
            if (layout->lane0CellCount >= GAUGE_MAX_CELLS)
                break;
        }

        if ((mask & GAUGE_LANE_MASK_1) && rom->fillTileStrips[seg][GAUGE_LANE_1])
        {
            layout->lane1CellList[layout->lane1CellCount++] = cell;
            if (layout->lane1CellCount >= GAUGE_MAX_CELLS)
                break;
        }
    }
}

void GaugeLayout_makeMirror(GaugeLayout *dst, const GaugeLayout *src, const GaugeRom *rom)
{
    dst->lengthTiles = src->lengthTiles;

    for (u8 c = 0; c < src->lengthTiles; c++)
    {
        const u8 srcCell = (u8)(src->lengthTiles - 1 - c);
        dst->segmentIdByCell[c] = src->segmentIdByCell[srcCell];
    }

    /* Mirrored HUD usually fills from the opposite end */
    GaugeLayout_setFillReverse(dst);

    GaugeLayout_buildLaneListsFromRom(dst, rom);
}

/* =============================================================================
   GaugeLogic
   ============================================================================= */

void GaugeLogic_init(GaugeLogic *logic,
                     u16 maxValue,
                     u16 maxFillPixels,
                     const u16 *valueToPixelsLUT)
{
    logic->maxValue = maxValue;
    logic->currentValue = maxValue;

    logic->maxFillPixels = maxFillPixels;
    logic->valueToPixelsLUT = valueToPixelsLUT;

    logic->valueTargetPixels = clamp_u16(value_to_pixels(logic, maxValue), 0, maxFillPixels);
    logic->valuePixels = logic->valueTargetPixels;

    logic->trailPixels = logic->valuePixels;

    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;

    /* Defaults */
    logic->valueAnimEnabled = 0;
    logic->valueAnimShift = 4;
    logic->trailAnimShift = 4;
    logic->blinkShift = 3;
}

void GaugeLogic_setAnimation(GaugeLogic *logic,
                             u8 valueAnimEnabled,
                             u8 valueAnimShift,
                             u8 trailAnimShift,
                             u8 blinkShift)
{
    logic->valueAnimEnabled = valueAnimEnabled ? 1 : 0;

    logic->valueAnimShift = (valueAnimShift == 0) ? 4 : valueAnimShift;
    logic->trailAnimShift = (trailAnimShift == 0) ? 4 : trailAnimShift;
    logic->blinkShift     = (blinkShift == 0) ? 3 : blinkShift;
}

void GaugeLogic_setValue(GaugeLogic *logic, u16 newValue)
{
    logic->currentValue = (newValue > logic->maxValue) ? logic->maxValue : newValue;

    logic->valueTargetPixels = clamp_u16(value_to_pixels(logic, logic->currentValue), 0, logic->maxFillPixels);
    logic->valuePixels = logic->valueTargetPixels;

    /* Cancel trailing effect (example: debug set HP directly) */
    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
}

void GaugeLogic_applyDecrease(GaugeLogic *logic,
                              u16 amount,
                              u8 holdFrames,
                              u8 blinkFrames)
{
    /* Life bar example:
       - damage decreases HP immediately (value)
       - trail jumps to previous displayed width to show the lost part */
    const u16 previousDisplayedValuePixels = logic->valuePixels;

    logic->currentValue = sat_sub_u16(logic->currentValue, amount);

    logic->valueTargetPixels = clamp_u16(value_to_pixels(logic, logic->currentValue), 0, logic->maxFillPixels);
    if (!logic->valueAnimEnabled)
        logic->valuePixels = logic->valueTargetPixels;

    if (logic->trailPixels < previousDisplayedValuePixels)
        logic->trailPixels = previousDisplayedValuePixels;

    logic->holdFramesRemaining = holdFrames;
    logic->blinkFramesRemaining = blinkFrames;
}

void GaugeLogic_applyIncrease(GaugeLogic *logic, u16 amount)
{
    /* Life bar example:
       - heal increases HP (value)
       - no trailing red part on heal => trail resets to value */
    logic->currentValue = sat_add_u16(logic->currentValue, amount, logic->maxValue);

    logic->valueTargetPixels = clamp_u16(value_to_pixels(logic, logic->currentValue), 0, logic->maxFillPixels);
    if (!logic->valueAnimEnabled)
        logic->valuePixels = logic->valueTargetPixels;

    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
}

void GaugeLogic_tick(GaugeLogic *logic)
{
    /* 1) animate valuePixels toward valueTargetPixels (optional) */
    if (logic->valueAnimEnabled && (logic->valuePixels != logic->valueTargetPixels))
    {
        if (logic->valuePixels < logic->valueTargetPixels)
        {
            const u16 diff = (u16)(logic->valueTargetPixels - logic->valuePixels);
            const u16 step = (u16)((diff >> logic->valueAnimShift) + 1);
            logic->valuePixels += step;
            if (logic->valuePixels > logic->valueTargetPixels)
                logic->valuePixels = logic->valueTargetPixels;
        }
        else
        {
            const u16 diff = (u16)(logic->valuePixels - logic->valueTargetPixels);
            const u16 step = (u16)((diff >> logic->valueAnimShift) + 1);
            logic->valuePixels = (logic->valuePixels > step) ? (u16)(logic->valuePixels - step) : 0;
            if (logic->valuePixels < logic->valueTargetPixels)
                logic->valuePixels = logic->valueTargetPixels;
        }
    }

    /* 2) blink timer */
    if (logic->blinkFramesRemaining)
        logic->blinkFramesRemaining--;

    /* 3) hold then trail shrinks toward valuePixels */
    if (logic->holdFramesRemaining)
    {
        logic->holdFramesRemaining--;
    }
    else
    {
        if (logic->trailPixels > logic->valuePixels)
        {
            const u16 diff = (u16)(logic->trailPixels - logic->valuePixels);
            const u16 step = (u16)((diff >> logic->trailAnimShift) + 1);
            logic->trailPixels = (logic->trailPixels > step) ? (u16)(logic->trailPixels - step) : 0;
            if (logic->trailPixels < logic->valuePixels)
                logic->trailPixels = logic->valuePixels;
        }
        else
        {
            logic->trailPixels = logic->valuePixels;
        }
    }

    /* Safety clamps */
    if (logic->valuePixels > logic->maxFillPixels) logic->valuePixels = logic->maxFillPixels;
    if (logic->trailPixels > logic->maxFillPixels) logic->trailPixels = logic->maxFillPixels;
    if (logic->trailPixels < logic->valuePixels)   logic->trailPixels = logic->valuePixels;
}

/* =============================================================================
   GaugeRenderer internals
   ============================================================================= */

static inline void bind_cell_rom(GaugeStreamCell *cell, const GaugeRom *rom, u8 segmentId, u8 lane)
{
    if (!rom) { cell->romFillStrip45 = NULL; return; }
    if (segmentId >= rom->segmentCount) { cell->romFillStrip45 = NULL; return; }
    cell->romFillStrip45 = rom->fillTileStrips[segmentId][lane];
}

static inline void upload_cell_if_needed(GaugeStreamCell *cell, u8 wantedFillIdx)
{
    if (!cell->romFillStrip45) return;
    if (cell->loadedFillIdx == wantedFillIdx) return;

    /* Each tile = 8 u32, so offset = idx * 8 */
    const u32 *src = cell->romFillStrip45 + (((u16)wantedFillIdx) << 3);
    VDP_loadTileData(src, cell->vramTileIndex, 1, DMA_QUEUE);
    cell->loadedFillIdx = wantedFillIdx;
}

static inline void compute_tile_xy(GaugeOrientation orient,
                                  u16 originX, u16 originY,
                                  u8 lane, u8 cell,
                                  u16 *outX, u16 *outY)
{
    /* No multiplication; only additions. */
    if (orient == GAUGE_ORIENT_HORIZONTAL)
    {
        *outX = (u16)(originX + cell);
        *outY = (u16)(originY + lane);
    }
    else
    {
        *outX = (u16)(originX + lane);
        *outY = (u16)(originY + cell);
    }
}

static void write_window_tilemap_once(GaugeRenderer *r)
{
    r->lane0CellCount = r->layout.lane0CellCount;
    for (u8 i = 0; i < r->lane0CellCount; i++)
    {
        const u8 cellIndex = r->layout.lane0CellList[i];
        const u16 vramTile = (u16)(r->vramLane0Base + i);
        const u16 attr = TILE_ATTR_FULL(r->paletteLine, r->priority, r->vflip, r->hflip, vramTile);

        u16 x, y;
        compute_tile_xy(r->orientation, r->originX, r->originY, GAUGE_LANE_0, cellIndex, &x, &y);

        VDP_setTileMapXY(WINDOW, attr, x, y);

        r->lane0Cells[i].vramTileIndex = vramTile;
        r->lane0Cells[i].loadedFillIdx = 0xFF;
        r->lane0Cells[i].cellIndex = cellIndex;
    }

    r->lane1CellCount = r->layout.lane1CellCount;
    for (u8 i = 0; i < r->lane1CellCount; i++)
    {
        const u8 cellIndex = r->layout.lane1CellList[i];
        const u16 vramTile = (u16)(r->vramLane1Base + i);
        const u16 attr = TILE_ATTR_FULL(r->paletteLine, r->priority, r->vflip, r->hflip, vramTile);

        u16 x, y;
        compute_tile_xy(r->orientation, r->originX, r->originY, GAUGE_LANE_1, cellIndex, &x, &y);

        VDP_setTileMapXY(WINDOW, attr, x, y);

        r->lane1Cells[i].vramTileIndex = vramTile;
        r->lane1Cells[i].loadedFillIdx = 0xFF;
        r->lane1Cells[i].cellIndex = cellIndex;
    }
}

/* =============================================================================
   GaugeRenderer public API
   ============================================================================= */

void GaugeRenderer_bindRom(GaugeRenderer *renderer, const GaugeRom *rom)
{
    renderer->rom = rom;

    /* Lane 0 */
    for (u8 i = 0; i < renderer->lane0CellCount; i++)
    {
        const u8 cellIndex = renderer->lane0Cells[i].cellIndex;
        const u8 seg = renderer->layout.segmentIdByCell[cellIndex];
        bind_cell_rom(&renderer->lane0Cells[i], rom, seg, GAUGE_LANE_0);
    }

    /* Lane 1 */
    for (u8 i = 0; i < renderer->lane1CellCount; i++)
    {
        const u8 cellIndex = renderer->lane1Cells[i].cellIndex;
        const u8 seg = renderer->layout.segmentIdByCell[cellIndex];
        bind_cell_rom(&renderer->lane1Cells[i], rom, seg, GAUGE_LANE_1);
    }
}

void GaugeRenderer_init(GaugeRenderer *renderer,
                        GaugeOrientation orientation,
                        const GaugeRom *rom,
                        const GaugeLayout *layout,
                        u16 originX, u16 originY,
                        u16 vramLane0Base, u16 vramLane1Base,
                        u8 paletteLine, u8 priority, u8 vflip, u8 hflip)
{
    renderer->orientation = orientation;

    renderer->originX = originX;
    renderer->originY = originY;

    renderer->paletteLine = paletteLine;
    renderer->priority = priority ? 1 : 0;
    renderer->vflip = vflip ? 1 : 0;
    renderer->hflip = hflip ? 1 : 0;

    renderer->vramLane0Base = vramLane0Base;
    renderer->vramLane1Base = vramLane1Base;

    /* Copy layout + clamp lengthTiles */
    renderer->layout = *layout;
    if (renderer->layout.lengthTiles == 0) renderer->layout.lengthTiles = 1;
    if (renderer->layout.lengthTiles > GAUGE_MAX_CELLS) renderer->layout.lengthTiles = GAUGE_MAX_CELLS;

    /* Build lane lists from ROM (mask + non-null pointer) */
    GaugeLayout_buildLaneListsFromRom(&renderer->layout, rom);

    /* Write tilemap only where needed */
    write_window_tilemap_once(renderer);

    /* Bind ROM strips */
    GaugeRenderer_bindRom(renderer, rom);

    /* Init renderer cache to force first update */
    renderer->lastValuePixels = 0xFFFF;
    renderer->lastTrailPixelsRendered = 0xFFFF;
    renderer->lastBlinkOn = 0xFF;
}

void GaugeRenderer_queueDma(GaugeRenderer *renderer,
                            const GaugeLogic *logic,
                            u16 frameCounter)
{
    /* Blink: when blink is OFF, hide trail by rendering trailPixelsRendered=valuePixels */
    u8 blinkOn = 1;
    if (logic->blinkFramesRemaining)
        blinkOn = (u8)(((frameCounter >> logic->blinkShift) & 1) == 0);

    const u16 valuePixels = logic->valuePixels;

    u16 trailPixels = logic->trailPixels;
    if (trailPixels < valuePixels) trailPixels = valuePixels;

    const u16 trailPixelsRendered = blinkOn ? trailPixels : valuePixels;

    if (renderer->lastValuePixels == valuePixels &&
        renderer->lastTrailPixelsRendered == trailPixelsRendered &&
        renderer->lastBlinkOn == blinkOn)
        return;
    renderer->lastValuePixels = valuePixels;
    renderer->lastTrailPixelsRendered = trailPixelsRendered;
    renderer->lastBlinkOn = blinkOn;

    /* Lane 0 cells */
    for (u8 i = 0; i < renderer->lane0CellCount; i++)
    {
        GaugeStreamCell *cell = &renderer->lane0Cells[i];
        const u8 axisCell = cell->cellIndex;

        const u8 fillIndex = renderer->layout.fillIndexByCell[axisCell];

        const u8 valuePxInTile = clamp_px_0_8((s16)valuePixels - (s16)(fillIndex << 3));
        u8 trailPxInTile       = clamp_px_0_8((s16)trailPixelsRendered - (s16)(fillIndex << 3));
        if (trailPxInTile < valuePxInTile) trailPxInTile = valuePxInTile;

        const u8 wantedIdx = s_fillIdxByValueTrail[valuePxInTile][trailPxInTile];
        upload_cell_if_needed(cell, wantedIdx);
    }

    /* Lane 1 cells */
    for (u8 i = 0; i < renderer->lane1CellCount; i++)
    {
        GaugeStreamCell *cell = &renderer->lane1Cells[i];
        const u8 axisCell = cell->cellIndex;

        const u8 fillIndex = renderer->layout.fillIndexByCell[axisCell];

        const u8 valuePxInTile = clamp_px_0_8((s16)valuePixels - (s16)(fillIndex << 3));
        u8 trailPxInTile       = clamp_px_0_8((s16)trailPixelsRendered - (s16)(fillIndex << 3));
        if (trailPxInTile < valuePxInTile) trailPxInTile = valuePxInTile;

        const u8 wantedIdx = s_fillIdxByValueTrail[valuePxInTile][trailPxInTile];
        upload_cell_if_needed(cell, wantedIdx);
    }
}
