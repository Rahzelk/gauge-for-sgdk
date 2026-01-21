#ifndef GAUGE_H
#define GAUGE_H

#include <genesis.h>

/* =============================================================================
   gauge.h / gauge.c — Generic 2-level HUD gauge (SGDK / Mega Drive)
   -----------------------------------------------------------------------------
   What this module does
   ---------------------
   Renders a configurable gauge on the WINDOW plane with ROM->VRAM tile streaming
   (DMA_QUEUE). 
   
   The gauge is composed of Segments'Style.  
   
   The gauge supports two fill levels:
     - value : primary fill (the "real" gauge value)
     - trail : secondary delayed fill (typical "damage bar" effect)

   Life bar analogy (for understanding)
   ------------------------------------
   - value = current HP (often drawn in yellow or green)
   - trail = delayed HP (often drawn in red) that blinks briefly after damage

   For instance, when you get "Damage" :
     value decreases immediately, trail stays higher for a moment, blinks, then
     shrinks toward value.
   
   and when you "Heal" you would have the following behavior : 
     value increases, trail is reset to value (no blinking, no red part).

   
   Pixel-perfect fill with 8x8 tiles
   ---------------------------------
   The gauge is made of "cells" along its main axis. Each cell corresponds to
   one 8x8 tile pattern and represents 8 pixels of fill resolution.
   For each cell:
     valuePxInTile : 0..8
     trailPxInTile : 0..8 (must be >= valuePxInTile)

   (valuePxInTile, trailPxInTile) has 45 valid combinations:
     value=0 -> trail=0..8 : 9
     value=1 -> trail=1..8 : 8
     ...
     value=8 -> trail=8    : 1
     total = 45

   Therefore each segment style provides a ROM strip of 45 tiles for each lane.

   Lanes (thickness)
   -----------------
   A gauge can be 1-tile or 2-tiles thick, and we call Lane each "line of tile".
   - lane 0 = primary lane
   - lane 1 = secondary lane
   In horizontal orientation, lanes are stacked vertically (lane1 is below lane0).
   In vertical orientation, lanes are side-by-side (lane1 is to the right of lane0).

   Orientation
   -----------
   - Horizontal: cells advance along X, lanes along Y
   - Vertical  : cells advance along Y, lanes along X

   
   Some reminders
   ------------------
   - This module uses VDP_loadTileData(..., DMA_QUEUE) during the frame.
   - One tile can reference only ONE palette line, so value/trail/empty 
     pixels must all use colors from the same palette line.


   ============================================================================= */

/* ------------------------------
   Compile-time limits
   ------------------------------ */
#define GAUGE_MAX_SEGMENTS     8
#define GAUGE_MAX_CELLS       16  /* max number of cells along the axis */

/* Fill tile strip properties */
#define GAUGE_FILL_TILE_COUNT  45  /* number of tiles per segment per lane */
#define GAUGE_TILE_U32_COUNT    8  /* SGDK TileSet tile size in u32 words (8*4 = 32 bytes) */

/* ------------------------------
   Orientation and fill direction
   ------------------------------ */
typedef enum
{
    GAUGE_ORIENT_HORIZONTAL = 0,
    GAUGE_ORIENT_VERTICAL   = 1
} GaugeOrientation;

/* Optional helper enum (you can ignore and call the functions directly). */
typedef enum
{
    GAUGE_FILL_FORWARD = 0, /* cell 0 -> cell N-1 */
    GAUGE_FILL_REVERSE = 1  /* cell N-1 -> cell 0 */
} GaugeFillDirection;

/* ------------------------------
   Lanes
   ------------------------------ */
#define GAUGE_LANE_0 0
#define GAUGE_LANE_1 1

#define GAUGE_LANE_MASK_0 (1 << GAUGE_LANE_0)
#define GAUGE_LANE_MASK_1 (1 << GAUGE_LANE_1)


/* =============================================================================
   SECTION A — ROM ASSETS ORDERING (45 tiles per segment per lane)
   -----------------------------------------------------------------------------
   For each segmentId and lane used, provide a ROM strip of EXACTLY 45 tiles,
   ordered as:

     for valuePxInTile = 0..8:
       for trailPxInTile = valuePxInTile..8:
         append tile(valuePxInTile, trailPxInTile)

   Visual mnemonic for a HORIZONTAL gauge (8 pixels wide):
     V = primary pixels (value)
     T = trailing pixels (trail-only)
     . = empty/background

     value=3, trail=6 -> "VVVTTT.."

   For a VERTICAL gauge, the ordering is IDENTICAL, but your tiles must represent
   vertical fill (e.g., pixels from bottom to top, depending on your art).
   You cannot rotate tiles 90° on Mega Drive BG, so vertical gauges need dedicated
   vertical tile strips.

   Full idx table (0..44) is identical to the one shown in previous versions.
   ============================================================================= */


/* =============================================================================
   GaugeRom — ROM pointers for segment styles and lanes
   -----------------------------------------------------------------------------
   - segmentLaneMask[segmentId] tells which lanes exist for that segment.
     A segment can be:
       - lane0-only
       - lane1-only
       - lane0 + lane1

   - fillTileStrips[segmentId][lane] points to a ROM strip of 45 tiles for that
     segment+lane. Pointer can be NULL if lane does not exist.

   IMPORTANT SAFETY (UI ownership)
   -------------------------------
   The renderer writes WINDOW tilemap ONLY for cells/lane where BOTH are true:
     - segmentLaneMask says the lane exists
     - fillTileStrips pointer is not NULL
   So if a segment is lane0-only at some cell, the renderer will NOT modify lane1
   tilemap at that position (no accidental overwrite of other UI elements).
   ============================================================================= */

typedef struct
{
    const u32 *fillTileStrips[GAUGE_MAX_SEGMENTS][2]; /* [segmentId][lane] -> 45 tiles strip or NULL */
    u8 segmentLaneMask[GAUGE_MAX_SEGMENTS];           /* bit0=lane0, bit1=lane1 */
    u8 segmentCount;                                  /* valid IDs: 0..segmentCount-1 */
} GaugeRom;


/* =============================================================================
   GaugeLayout — gauge geometry along the axis
   -----------------------------------------------------------------------------
   lengthTiles:
     Number of cells along the gauge axis (each cell = one 8x8 tile).
     This is the gauge length along its main axis (horizontal or vertical).

     maxFillPixels must be lengthTiles * 8.

   segmentIdByCell[cell]:
     Which segment style is used for that axis cell.

   fillIndexByCell[cell]:
     Which 8px block index this cell represents for pixel-perfect computation:
       valuePxInTile = clamp(valuePixels - fillIndex*8, 0..8)

     You typically set it using:
       - GaugeLayout_setFillForward()  : fillIndex[cell] = cell
       - GaugeLayout_setFillReverse()  : fillIndex[cell] = lengthTiles-1-cell

   lane lists:
     Derived from GaugeRom (mask + non-null pointer), used by renderer to:
       - write tilemap only where needed
       - stream only existing lane cells
   ============================================================================= */

typedef struct
{
    u8 lengthTiles; /* 1..GAUGE_MAX_CELLS */

    u8 segmentIdByCell[GAUGE_MAX_CELLS];
    u8 fillIndexByCell[GAUGE_MAX_CELLS];

    u8 lane0CellList[GAUGE_MAX_CELLS];
    u8 lane1CellList[GAUGE_MAX_CELLS];
    u8 lane0CellCount;
    u8 lane1CellCount;

} GaugeLayout;

void GaugeLayout_setFillForward(GaugeLayout *layout);
void GaugeLayout_setFillReverse(GaugeLayout *layout);

void GaugeLayout_buildLaneListsFromRom(GaugeLayout *layout, const GaugeRom *rom);

/* Mirror along axis:
   - reverse segmentIdByCell order
   - set fillReverse (common for mirrored player HUD)
   - rebuild lane lists
*/
void GaugeLayout_makeMirror(GaugeLayout *dst, const GaugeLayout *src, const GaugeRom *rom);


/* =============================================================================
   GaugeLogic — generic value/trail behavior
   -----------------------------------------------------------------------------
   Life bar examples are included for understanding.

   If maxValue != maxFillPixels, you typically provide a LUT (valueToPixelsLUT)
   so that value maps to a pixel width exactly.

   On heal:
     - applyIncrease(): value increases, trail resets to value (no blink).

   On damage:
     - applyDecrease(): value decreases, trail jumps to previous displayed width,
       holds for holdFrames, blinks for blinkFrames, then shrinks toward value.
   ============================================================================= */

typedef struct
{
    u16 maxValue;
    u16 currentValue;

    u16 maxFillPixels;           /* MUST be layout.lengthTiles * 8 */
    const u16 *valueToPixelsLUT; /* optional: [0..maxValue], NULL => 1:1 mapping */

    u16 valueTargetPixels;
    u16 valuePixels;

    u16 trailPixels;

    u8 holdFramesRemaining;
    u8 blinkFramesRemaining;

    u8 valueAnimEnabled;
    u8 valueAnimShift;
    u8 trailAnimShift;
    u8 blinkShift;

} GaugeLogic;

void GaugeLogic_init(GaugeLogic *logic,
                     u16 maxValue,
                     u16 maxFillPixels,
                     const u16 *valueToPixelsLUT);

void GaugeLogic_setAnimation(GaugeLogic *logic,
                             u8 valueAnimEnabled,
                             u8 valueAnimShift,
                             u8 trailAnimShift,
                             u8 blinkShift);

void GaugeLogic_setValue(GaugeLogic *logic, u16 newValue);

void GaugeLogic_applyDecrease(GaugeLogic *logic,
                              u16 amount,
                              u8 holdFrames,
                              u8 blinkFrames);

void GaugeLogic_applyIncrease(GaugeLogic *logic, u16 amount);

void GaugeLogic_tick(GaugeLogic *logic);


/* =============================================================================
   GaugeRenderer — WINDOW tilemap + ROM->VRAM streaming
   -----------------------------------------------------------------------------
   Renderer configuration:
     - orientation (horizontal / vertical)
     - origin tile (originX, originY) on WINDOW plane
     - vram base indices for each lane (dedicated to this gauge)

   Slot allocation:
     lane0 uses compact slots: vramLane0Base + [0..lane0CellCount-1]
     lane1 uses compact slots: vramLane1Base + [0..lane1CellCount-1]

   Tilemap ownership:
     Only writes tilemap for cells/lane that exist (mask + pointer).
     No "empty tile" is written for absent lanes, so the module does not overwrite
     other HUD elements occupying those positions.

   Runtime:
     GaugeRenderer_queueDma() computes wanted idx 0..44 per lane cell and uploads
     only if idx changed (DMA_QUEUE).
   ============================================================================= */

typedef struct
{
    u16 vramTileIndex;
    const u32 *romFillStrip45; /* pointer to 45-tile strip in ROM */
    u8 loadedFillIdx;          /* last uploaded idx, 0xFF unknown */
    u8 cellIndex;              /* axis cell index 0..lengthTiles-1 */
} GaugeStreamCell;

typedef struct
{
    GaugeOrientation orientation;

    u16 originX;
    u16 originY; /* origin on WINDOW plane */

    u8 paletteLine;
    u8 priority;
    u8 vflip;
    u8 hflip;

    GaugeLayout layout;     /* copied */
    const GaugeRom *rom;    /* referenced */

    u16 vramLane0Base;
    u16 vramLane1Base;

    GaugeStreamCell lane0Cells[GAUGE_MAX_CELLS];
    GaugeStreamCell lane1Cells[GAUGE_MAX_CELLS];
    u8 lane0CellCount;
    u8 lane1CellCount;

    /* Optional micro-optimization: skip per-cell loops if nothing changed. */
    u16 lastValuePixels;
    u16 lastTrailPixelsRendered;
    u8 lastBlinkOn;

} GaugeRenderer;

void GaugeRenderer_init(GaugeRenderer *renderer,
                        GaugeOrientation orientation,
                        const GaugeRom *rom,
                        const GaugeLayout *layout,
                        u16 originX, u16 originY,
                        u16 vramLane0Base, u16 vramLane1Base,
                        u8 paletteLine, u8 priority, u8 vflip, u8 hflip);

void GaugeRenderer_bindRom(GaugeRenderer *renderer, const GaugeRom *rom);

void GaugeRenderer_queueDma(GaugeRenderer *renderer,
                            const GaugeLogic *logic,
                            u16 frameCounter);

#endif /* GAUGE_H */
