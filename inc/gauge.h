#ifndef GAUGE_H
#define GAUGE_H

#include <genesis.h>

/* =============================================================================
   gauge.h / gauge.c â€” Single-lane HUD gauge for SGDK / Mega Drive
   =============================================================================

   SIMPLIFIED GAUGE MODULE
   -----------------------
   This module provides a pixel-perfect gauge rendering system optimized for
   16-bit hardware with minimal CPU/VRAM overhead.

   KEY CONCEPTS:
   - NO lane concept: 1 gauge = 1 row/column of tiles
   - Shared GaugeLogic via Gauge container (multiple parts sync perfectly)
   - Only 2 main objects: Gauge (high-level) and GaugeLayout (configuration)
   - Fill offset support for extended/segmented gauges

   ARCHITECTURE:
   - GaugeLayout: Geometry + visual appearance (orientation, palette, flip)
   - GaugeLogic:  Value state machine (value, trail, animation timers)
   - GaugePart: VRAM streaming engine (internal, managed by Gauge)
   - Gauge: High-level container (1 logic + N parts)

   TYPICAL USAGE:
   ```c
   static Gauge myGauge;
   static GaugePart myParts[2];
   static GaugeLayout myLayout;

   // 1. Initialize layout with visual properties
   GaugeLayout_init(&myLayout, 8, GAUGE_FILL_FORWARD, tilesets, segments,
                    GAUGE_ORIENT_HORIZONTAL, PAL0, 1, 0, 0);

   // 2. Initialize gauge (auto-allocates VRAM for parts)
   Gauge_init(&myGauge, 100, 64, 1, 100, myRenderers, VRAM_BASE, GAUGE_VRAM_DYNAMIC);

   // 3. Add parts (VRAM auto-allocated sequentially)
   Gauge_addPart(&myGauge, &myRenderers[0], &myLayout, 2, 5);
   Gauge_addPart(&myGauge, &myRenderers[1], &myLayout, 2, 6);

   // 4. Game loop
   while(1) {
       Gauge_update(&myGauge);  // Tick logic + render all
       SYS_doVBlankProcess();
   }

   // 5. Change value
   Gauge_decrease(&myGauge, 10, 20, 60);  // Damage with trail effect
   Gauge_increase(&myGauge, 5, 20, 60);   // Heal with gain trail
   ```

   2-LANE SIMULATION:
   To simulate 2-tile thickness, use 2 parts sharing the same Gauge logic,
   positioned at adjacent tiles (Y+1 for horizontal, X+1 for vertical).

   ============================================================================= */
/* -----------------------------------------------------------------------------
   Compile-time limits
   ----------------------------------------------------------------------------- */
#define GAUGE_MAX_SEGMENTS    12   /* Maximum different tile styles per layout */
#define GAUGE_MAX_LENGTH      16   /* Maximum tiles per gauge */
#define GAUGE_MAX_PARTS        8   /* Maximum parts per gauge (safety guard) */

/* Tile properties */
#define GAUGE_PIXELS_PER_TILE  8   /* Pixels per tile (8x8) */
#define GAUGE_FILL_TILE_COUNT 45   /* Tiles in a 45-tile fill strip */
#define GAUGE_TILE_U32_COUNT   8   /* 32-bit words per tile (8x8 @ 4bpp = 32 bytes) */

/* Default animation values */
#define GAUGE_DEFAULT_VALUE_ANIM_SHIFT  4  /* step = diff/16 + 1 */
#define GAUGE_DEFAULT_TRAIL_ANIM_SHIFT  4  /* step = diff/16 + 1 */
#define GAUGE_DEFAULT_BLINK_SHIFT       3  /* ~7.5 Hz @ 60fps */

/* Fill pixel range */
#define GAUGE_FILL_PX_MIN  0
#define GAUGE_FILL_PX_MAX  8

/* -----------------------------------------------------------------------------
   Enumerations
   ----------------------------------------------------------------------------- */

/**
 * Gauge orientation on screen.
 * HORIZONTAL: fills left-to-right (or right-to-left if reversed)
 * VERTICAL: fills top-to-bottom (or bottom-to-top if reversed)
 */
typedef enum
{
    GAUGE_ORIENT_HORIZONTAL = 0,
    GAUGE_ORIENT_VERTICAL   = 1
} GaugeOrientation;

/**
 * Fill direction determines which end fills first.
 * FORWARD: cell 0 fills first (left/top)
 * REVERSE: last cell fills first (right/bottom)
 */
typedef enum
{
    GAUGE_FILL_FORWARD = 0,
    GAUGE_FILL_REVERSE = 1
} GaugeFillDirection;

/**
 * VRAM allocation mode for rendering.
 * FIXED:   One VRAM tile per gauge cell. More VRAM, less CPU.
 *          Best for: mirrors, static gauges, when VRAM is plentiful.
 * DYNAMIC: Shared VRAM tiles, remapped per frame. Less VRAM, more CPU.
 *          Best for: P1 gauges, when VRAM is limited.
 */
typedef enum
{
    GAUGE_VRAM_FIXED   = 0,
    GAUGE_VRAM_DYNAMIC = 1
} GaugeVramMode;

/**
 * Trail mode (current active trail behavior).
 * NONE   : no trail effect active
 * DAMAGE : classic damage trail (value decreases, trail shrinks)
 * GAIN   : gain trail (value increases, trail leads then value catches up)
 */
typedef enum
{
    GAUGE_TRAIL_NONE   = 0,
    GAUGE_TRAIL_DAMAGE = 1,
    GAUGE_TRAIL_GAIN   = 2
} GaugeTrailMode;


/* =============================================================================
   Forward declarations (needed for circular references)
   ============================================================================= */
typedef struct Gauge Gauge;


/* =============================================================================
   GaugeLayout â€” Geometry + visual configuration
   =============================================================================

   Contains everything needed to describe a gauge's appearance:
   - Tile geometry (length, segments, fill direction)
   - Visual properties (orientation, palette, flip flags, priority)
   - Asset references (tilesets)

   FIELDS:
   -------
   length:
     Number of cells along the gauge axis (1..GAUGE_MAX_LENGTH).
     Each cell = one 8x8 tile = 8 pixels of fill resolution.

   segmentIdByCell[cell]:
     Which segment style (tileset) to use for each cell.
     Allows different tile graphics within one gauge.

   fillIndexByCell[cell]:
     Index for pixel-perfect fill computation:
       valuePxInTile = clamp(valuePixels - fillOffset - fillIndex*8, 0..8)
     Set automatically by GaugeLayout_setFillForward/Reverse().

   fillOffset:
     Pixel offset for this layout's fill computation (default 0).
     Allows rendering a "window" into a larger virtual gauge.
     Example: offset=64 means this gauge starts rendering at pixel 64.

   tilesetBySegment[segmentId]:
     Pointer to 45-tile ROM strip for segment BODY (interior).
     NULL if segment not used.

   tilesetEndBySegment[segmentId]:
     Optional 45-tile ROM strip for segment END (termination).
     If NULL, segment uses BODY only (break ignored).

   tilesetBreakBySegment[segmentId]:
     Optional 45-tile ROM strip for segment BREAK (transition before end).
     If NULL but END exists, BREAK falls back to BODY.

   tilesetTrailBySegment[segmentId]:
     Optional 64-tile ROM strip for TRAIL (trail-specific shapes).
     If NULL, trail rendering falls back to BODY rules.

   tilesetBridgeBySegment[segmentId]:
     Optional 45-tile ROM strip for BRIDGE (segment transition tile).
     Placed at the last cell of a segment before a different segment begins.
     The bridge tile mirrors the fill state of the NEXT cell to ensure a
     seamless visual transition between segments.

     Example with 2 segments (A=zone, B=zone):
       Cell:    [A][A][A][D][B][B][B][E]
                          ^
                     Bridge (D) shows what the next B cell would show

     If NULL, the last cell of the segment uses a forced BREAK instead
     (fully filled with BREAK tileset, index 44).

   tilesetCapStartBySegment[segmentId]:
     Optional 45-tile ROM strip for CAP START (fixed border at gauge start).
     Applied to cell 0 in fill order. This cell always uses a cap tileset
     instead of the normal body, but the fill index varies with gauge state.

     Cap start has 3 visual variants depending on gauge state:
       State     | Tileset used            | Meaning
       ----------|-------------------------|------------------------------------
       END here  | capStartStrip           | Value/trail edge is in this cell
       TRAIL zone| capStartTrailStrip      | Trail is visible in this cell
       Other     | capStartBreakStrip      | Cell is full or value hasn't arrived

     Example:  [CS][F][F][B][V][T][T][E]
                 ^
            Cap Start cell (always rendered with cap tileset)

   tilesetCapEndBySegment[segmentId]:
     Optional 45-tile ROM strip for CAP END (fixed border at gauge end).
     Applied to the last cell in fill order. Always rendered using this
     tileset regardless of fill state.

   tilesetCapStartBreakBySegment[segmentId]:
     Optional 45-tile ROM strip for CAP START in break/full state.
     Used when the value edge has not yet reached the cap start cell,
     or when the cell is fully filled. Falls back to capStartStrip if NULL.

   tilesetCapStartTrailBySegment[segmentId]:
     Optional 45-tile ROM strip for CAP START in trail zone.
     Used when the trail break or full trail zone covers the cap start cell.

   capEndBySegment[segmentId]:
     1 to enable CAP END for the segment used by the last cell.
     When enabled, the last cell always uses tilesetCapEndBySegment.

   orientation:
     GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL.

   paletteLine:
     Palette line (0-3) for tile rendering.

   priority:
     Tile priority (0=behind sprites, 1=in front of sprites).

   vflip, hflip:
     Vertical/horizontal flip flags for tile rendering.
     Used for mirrored gauges (P2 side).

   ============================================================================= */
typedef struct
{ 
    /* --- Geometry (u16 first for 68000 word alignment) --- */
    u16 fillOffset;                              /* Pixel offset for fill calc */
    u8 length;                                   /* Number of cells (1..16) */

    u8 segmentIdByCell[GAUGE_MAX_LENGTH];        /* Segment style per cell */
    u8 fillIndexByCell[GAUGE_MAX_LENGTH];        /* Fill order per cell */
    u8 cellIndexByFillIndex[GAUGE_MAX_LENGTH];   /* Inverse LUT: cell index for a given fill index (O(1) lookup) */

    /* --- Tilemap positions --- */
    Vect2D_u16 tilemapPosByCell[GAUGE_MAX_LENGTH]; /* Tilemap X,Y coordinates per cell */
 
    /* --- Tilesets --- */
    const u32 *tilesetBySegment[GAUGE_MAX_SEGMENTS];     /* BODY: 45-tile strips */
    const u32 *tilesetEndBySegment[GAUGE_MAX_SEGMENTS];  /* END: optional 45-tile strips */
    const u32 *tilesetBreakBySegment[GAUGE_MAX_SEGMENTS];/* BREAK: optional 45-tile strips */
    const u32 *tilesetTrailBySegment[GAUGE_MAX_SEGMENTS];/* TRAIL: optional 64-tile strips */
    const u32 *tilesetBridgeBySegment[GAUGE_MAX_SEGMENTS];/* BRIDGE: optional 45-tile strips */
    const u32 *tilesetCapStartBySegment[GAUGE_MAX_SEGMENTS];/* CAP START: optional 45-tile strips */
    const u32 *tilesetCapEndBySegment[GAUGE_MAX_SEGMENTS];/* CAP END: optional 45-tile strips */
    const u32 *tilesetCapStartBreakBySegment[GAUGE_MAX_SEGMENTS];/* CAP START BREAK: optional 45-tile strips */
    const u32 *tilesetCapStartTrailBySegment[GAUGE_MAX_SEGMENTS];/* CAP START TRAIL: optional 45-tile strips */
    u8 capEndBySegment[GAUGE_MAX_SEGMENTS];             /* CAP END: 1 if enabled for segment */

    /* --- Gain trail tilesets (optional, per segment) ---
     * Used when trailMode == GAUGE_TRAIL_GAIN (increase).
     * NULL entries fall back to the normal tilesets.
     */
    const u32 *gainTilesetBySegment[GAUGE_MAX_SEGMENTS];     /* BODY: 45-tile strips */
    const u32 *gainTilesetEndBySegment[GAUGE_MAX_SEGMENTS];  /* END: 45-tile strips */
    const u32 *gainTilesetBreakBySegment[GAUGE_MAX_SEGMENTS];/* BREAK: 45-tile strips */
    const u32 *gainTilesetTrailBySegment[GAUGE_MAX_SEGMENTS];/* TRAIL: 64-tile strips */
    const u32 *gainTilesetBridgeBySegment[GAUGE_MAX_SEGMENTS];/* BRIDGE: 45-tile strips */
    const u32 *gainTilesetCapStartBySegment[GAUGE_MAX_SEGMENTS];     /* CAP START: 45-tile strips */
    const u32 *gainTilesetCapEndBySegment[GAUGE_MAX_SEGMENTS];       /* CAP END: 45-tile strips */
    const u32 *gainTilesetCapStartBreakBySegment[GAUGE_MAX_SEGMENTS];/* CAP START BREAK: 45-tile strips */
    const u32 *gainTilesetCapStartTrailBySegment[GAUGE_MAX_SEGMENTS];/* CAP START TRAIL: 45-tile strips */

    /* --- Blink-off tilesets (optional, per segment) ---
     * Used only during trail blink OFF frames (blinkFramesRemaining > 0).
     * If a blink-off tileset is NULL, rendering falls back to normal behavior
     * for that visual element (i.e., trail hidden).
     */
    const u32 *blinkOffTilesetBySegment[GAUGE_MAX_SEGMENTS];      /* BODY: 45-tile strips */
    const u32 *blinkOffTilesetEndBySegment[GAUGE_MAX_SEGMENTS];   /* END: 45-tile strips */
    const u32 *blinkOffTilesetBreakBySegment[GAUGE_MAX_SEGMENTS]; /* BREAK: 45-tile strips */
    const u32 *blinkOffTilesetTrailBySegment[GAUGE_MAX_SEGMENTS]; /* TRAIL: 64-tile strips */
    const u32 *blinkOffTilesetBridgeBySegment[GAUGE_MAX_SEGMENTS];/* BRIDGE: 45-tile strips */
    const u32 *blinkOffTilesetCapStartBySegment[GAUGE_MAX_SEGMENTS];     /* CAP START: 45-tile strips */
    const u32 *blinkOffTilesetCapEndBySegment[GAUGE_MAX_SEGMENTS];       /* CAP END: 45-tile strips */
    const u32 *blinkOffTilesetCapStartBreakBySegment[GAUGE_MAX_SEGMENTS];/* CAP START BREAK: 45-tile strips */
    const u32 *blinkOffTilesetCapStartTrailBySegment[GAUGE_MAX_SEGMENTS];/* CAP START TRAIL: 45-tile strips */

    /* --- Gain blink-off tilesets (optional, per segment) ---
     * Used only during gain trail blink OFF frames.
     * If a gain blink-off tileset is NULL, rendering falls back to normal behavior
     * for that visual element (i.e., trail hidden).
     */
    const u32 *gainBlinkOffTilesetBySegment[GAUGE_MAX_SEGMENTS];      /* BODY: 45-tile strips */
    const u32 *gainBlinkOffTilesetEndBySegment[GAUGE_MAX_SEGMENTS];   /* END: 45-tile strips */
    const u32 *gainBlinkOffTilesetBreakBySegment[GAUGE_MAX_SEGMENTS]; /* BREAK: 45-tile strips */
    const u32 *gainBlinkOffTilesetTrailBySegment[GAUGE_MAX_SEGMENTS]; /* TRAIL: 64-tile strips */
    const u32 *gainBlinkOffTilesetBridgeBySegment[GAUGE_MAX_SEGMENTS];/* BRIDGE: 45-tile strips */
    const u32 *gainBlinkOffTilesetCapStartBySegment[GAUGE_MAX_SEGMENTS];     /* CAP START: 45-tile strips */
    const u32 *gainBlinkOffTilesetCapEndBySegment[GAUGE_MAX_SEGMENTS];       /* CAP END: 45-tile strips */
    const u32 *gainBlinkOffTilesetCapStartBreakBySegment[GAUGE_MAX_SEGMENTS];/* CAP START BREAK: 45-tile strips */
    const u32 *gainBlinkOffTilesetCapStartTrailBySegment[GAUGE_MAX_SEGMENTS];/* CAP START TRAIL: 45-tile strips */

    /* --- Segment boundary LUTs (by fillIndex) ---
     *
     * These LUTs are auto-computed by build_bridge_luts() when fill direction
     * is set. They identify where segment transitions happen.
     *
     * Example with 3 segments (A, B, C) and bridges between A-B and B-C:
     *   fillIndex:            0  1  2  3  4  5  6  7
     *   segment:              A  A  A  A  B  B  B  C
     *   bridgeEndByFillIndex: 0  0  0  1  0  0  1  0
     *                                  ^           ^
     *                            bridge cells (last of segment with bridge tileset)
     *   bridgeBreakByFillIndex:0  0  1  0  0  1  0  0
     *                               ^        ^
     *                         forced BREAK cells (one before bridge)
     */
    u8 bridgeEndByFillIndex[GAUGE_MAX_LENGTH];     /* 1 if fillIndex is last cell of a segment with bridge */
    u8 bridgeBreakByFillIndex[GAUGE_MAX_LENGTH];   /* 1 if fillIndex is forced BREAK before a bridge */
    u8 bridgeBreakBoundaryByFillIndex[GAUGE_MAX_LENGTH]; /* fillIndex of the bridge cell this BREAK aligns to */

    /* --- Visual properties (all u8 for compact packing) --- */
    u8 orientation;                              /* GAUGE_ORIENT_HORIZONTAL=0 or GAUGE_ORIENT_VERTICAL=1 */
    u8 paletteLine;                              /* Palette line (0-3) */
    u8 priority;                                 /* Tile priority (0-1) */
    u8 vflip;                                    /* Vertical flip */
    u8 hflip;                                    /* Horizontal flip */

} GaugeLayout;


/* -----------------------------------------------------------------------------
   GaugeLayout API
   ----------------------------------------------------------------------------- */

/**
 * Initialize layout with complete configuration.
 *
 * @param layout        Layout to initialize
 * @param length        Number of cells (1..GAUGE_MAX_LENGTH)
 * @param fillDir       GAUGE_FILL_FORWARD or GAUGE_FILL_REVERSE
 * @param tilesets      Array of GAUGE_MAX_SEGMENTS tileset pointers (45-tile strips)
 *                      Can be NULL, tiles will be set from this array
 * @param segmentIdByCell Array of length elements specifying segment ID per cell
 *                      Can be NULL, all cells will use segment 0
 * @param orientation   GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL
 * @param paletteLine   Palette line (0-3, typically PAL0-PAL3)
 * @param priority      Tile priority (0 or 1)
 * @param vflip         Vertical flip (0 or 1)
 * @param hflip         Horizontal flip (0 or 1)
 */
void GaugeLayout_init(GaugeLayout *layout,
                      u8 length,
                      GaugeFillDirection fillDir,
                      const u32 * const *tilesets,
                      const u8 *segmentIdByCell,
                      GaugeOrientation orientation,
                      u8 paletteLine,
                      u8 priority,
                      u8 vflip,
                      u8 hflip);

/**
 * Initialize layout with BODY + optional END/BREAK tilesets.
 *
 * Fallback rules:
 * - If END tileset is NULL for a segment, BODY is used everywhere (BREAK ignored).
 * - If END exists but BREAK is NULL, BREAK uses BODY.
 *
 * @param layout        Layout to initialize
 * @param length        Number of cells (1..GAUGE_MAX_LENGTH)
 * @param fillDir       GAUGE_FILL_FORWARD or GAUGE_FILL_REVERSE
 * @param bodyTilesets  Array of GAUGE_MAX_SEGMENTS BODY tileset pointers (45-tile strips)
 * @param endTilesets   Array of GAUGE_MAX_SEGMENTS END tileset pointers (optional, can be NULL)
 * @param breakTilesets Array of GAUGE_MAX_SEGMENTS BREAK tileset pointers (optional, can be NULL)
 * @param trailTilesets Array of GAUGE_MAX_SEGMENTS TRAIL tileset pointers (optional, can be NULL)
 * @param bridgeTilesets Array of GAUGE_MAX_SEGMENTS BRIDGE tileset pointers (optional, can be NULL)
 * @param segmentIdByCell Array of length elements specifying segment ID per cell
 * @param orientation   GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL
 * @param paletteLine   Palette line (0-3, typically PAL0-PAL3)
 * @param priority      Tile priority (0 or 1)
 * @param vflip         Vertical flip (0 or 1)
 * @param hflip         Horizontal flip (0 or 1)
 */
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
                        u8 hflip);

/**
 * Set fill direction to forward (cell 0 fills first).
 * Use for gauges that fill from left or top.
 */
void GaugeLayout_setFillForward(GaugeLayout *layout);

/**
 * Set fill direction to reverse (last cell fills first).
 * Use for gauges that fill from right or bottom.
 */
void GaugeLayout_setFillReverse(GaugeLayout *layout);

/**
 * Create mirrored layout for P2/opponent side.
 * - Reverses segment order
 * - Reverses fill direction
 * - Sets appropriate flip flags based on orientation
 *
 * @param dst   Destination layout (will be initialized)
 * @param src   Source layout to mirror
 */
void GaugeLayout_makeMirror(GaugeLayout *dst, const GaugeLayout *src);

/**
 * Configure optional start/end cap tilesets for a layout.
 * Call AFTER GaugeLayout_init/initEx. Each parameter is an array of
 * GAUGE_MAX_SEGMENTS pointers; pass NULL for the entire array to skip,
 * or set individual entries to NULL to disable caps for that segment.
 *
 * @param layout                Layout to configure (must be initialized)
 * @param capStartTilesets      45-tile strips for cap start (cell 0 in fill order)
 * @param capEndTilesets        45-tile strips for cap end (last cell in fill order)
 * @param capStartBreakTilesets 45-tile strips for cap start in break/full state
 * @param capStartTrailTilesets 45-tile strips for cap start in trail zone
 * @param capEndBySegment       Array of flags: 1=enable cap end for segment, 0=disabled
 */
void GaugeLayout_setCaps(GaugeLayout *layout,
                         const u32 * const *capStartTilesets,
                         const u32 * const *capEndTilesets,
                         const u32 * const *capStartBreakTilesets,
                         const u32 * const *capStartTrailTilesets,
                         const u8 *capEndBySegment);

/**
 * Configure optional gain trail tilesets (used when trailMode == GAUGE_TRAIL_GAIN).
 * Pass NULL for any array to fall back to normal tilesets for that element.
 *
 * @param layout                   Layout to configure (must be initialized)
 * @param gainBodyTilesets         BODY gain strips (45 tiles)
 * @param gainEndTilesets          END gain strips (45 tiles)
 * @param gainBreakTilesets        BREAK gain strips (45 tiles)
 * @param gainTrailTilesets        TRAIL gain strips (64 tiles)
 * @param gainBridgeTilesets       BRIDGE gain strips (45 tiles)
 * @param gainCapStartTilesets     CAP START gain strips (45 tiles)
 * @param gainCapEndTilesets       CAP END gain strips (45 tiles)
 * @param gainCapStartBreakTilesets CAP START BREAK gain strips (45 tiles)
 * @param gainCapStartTrailTilesets CAP START TRAIL gain strips (45 tiles)
 */
void GaugeLayout_setGainTrail(GaugeLayout *layout,
                              const u32 * const *gainBodyTilesets,
                              const u32 * const *gainEndTilesets,
                              const u32 * const *gainBreakTilesets,
                              const u32 * const *gainTrailTilesets,
                              const u32 * const *gainBridgeTilesets,
                              const u32 * const *gainCapStartTilesets,
                              const u32 * const *gainCapEndTilesets,
                              const u32 * const *gainCapStartBreakTilesets,
                              const u32 * const *gainCapStartTrailTilesets);

/**
 * Configure optional blink-off tilesets (used during trail blink OFF frames).
 * Pass NULL for any array to disable blink-off for that element.
 *
 * Blink-off is applied only to END/TRAIL/BREAK cells while blinkFramesRemaining > 0.
 * Missing blink-off tilesets fall back to the normal rendering behavior.
 *
 * @param layout                   Layout to configure (must be initialized)
 * @param blinkOffBodyTilesets     BODY blink-off strips (45 tiles)
 * @param blinkOffEndTilesets      END blink-off strips (45 tiles)
 * @param blinkOffBreakTilesets    BREAK blink-off strips (45 tiles)
 * @param blinkOffTrailTilesets    TRAIL blink-off strips (64 tiles)
 * @param blinkOffBridgeTilesets   BRIDGE blink-off strips (45 tiles)
 * @param blinkOffCapStartTilesets CAP START blink-off strips (45 tiles)
 * @param blinkOffCapEndTilesets   CAP END blink-off strips (45 tiles)
 * @param blinkOffCapStartBreakTilesets CAP START BREAK blink-off strips (45 tiles)
 * @param blinkOffCapStartTrailTilesets CAP START TRAIL blink-off strips (45 tiles)
 */
void GaugeLayout_setBlinkOff(GaugeLayout *layout,
                             const u32 * const *blinkOffBodyTilesets,
                             const u32 * const *blinkOffEndTilesets,
                             const u32 * const *blinkOffBreakTilesets,
                             const u32 * const *blinkOffTrailTilesets,
                             const u32 * const *blinkOffBridgeTilesets,
                             const u32 * const *blinkOffCapStartTilesets,
                             const u32 * const *blinkOffCapEndTilesets,
                             const u32 * const *blinkOffCapStartBreakTilesets,
                             const u32 * const *blinkOffCapStartTrailTilesets);

/**
 * Configure optional gain blink-off tilesets (used during gain trail blink OFF frames).
 * Pass NULL for any array to disable gain blink-off for that element.
 *
 * @param layout                   Layout to configure (must be initialized)
 * @param gainBlinkOffBodyTilesets     BODY gain blink-off strips (45 tiles)
 * @param gainBlinkOffEndTilesets      END gain blink-off strips (45 tiles)
 * @param gainBlinkOffBreakTilesets    BREAK gain blink-off strips (45 tiles)
 * @param gainBlinkOffTrailTilesets    TRAIL gain blink-off strips (64 tiles)
 * @param gainBlinkOffBridgeTilesets   BRIDGE gain blink-off strips (45 tiles)
 * @param gainBlinkOffCapStartTilesets CAP START gain blink-off strips (45 tiles)
 * @param gainBlinkOffCapEndTilesets   CAP END gain blink-off strips (45 tiles)
 * @param gainBlinkOffCapStartBreakTilesets CAP START BREAK gain blink-off strips (45 tiles)
 * @param gainBlinkOffCapStartTrailTilesets CAP START TRAIL gain blink-off strips (45 tiles)
 */
void GaugeLayout_setGainBlinkOff(GaugeLayout *layout,
                                 const u32 * const *gainBlinkOffBodyTilesets,
                                 const u32 * const *gainBlinkOffEndTilesets,
                                 const u32 * const *gainBlinkOffBreakTilesets,
                                 const u32 * const *gainBlinkOffTrailTilesets,
                                 const u32 * const *gainBlinkOffBridgeTilesets,
                                 const u32 * const *gainBlinkOffCapStartTilesets,
                                 const u32 * const *gainBlinkOffCapEndTilesets,
                                 const u32 * const *gainBlinkOffCapStartBreakTilesets,
                                 const u32 * const *gainBlinkOffCapStartTrailTilesets);

/* =============================================================================
   GaugeLogic â€” Value/trail state machine
   =============================================================================

   Manages the gauge value and trail animation. Embedded in Gauge struct.
   All timing is frame-based (60fps on NTSC, 50fps on PAL).

   VALUE BEHAVIOR:
   ---------------
   On damage (Gauge_decrease):
     - value decreases immediately (or animates if valueAnimEnabled)
     - trail holds at previous width for holdFrames
     - trail blinks for blinkFrames
     - trail shrinks toward value

   On heal (Gauge_increase):
     - value increases (or animates if valueAnimEnabled)
     - if valueAnimEnabled + trailEnabled: gain trail (hold/blink then value catches up)
     - otherwise trail resets to value (no blink/hold)

   ANIMATION PARAMETERS:
   ---------------------
   valueAnimEnabled: 0=instant value changes, 1=animated value transition
   valueAnimShift:   Speed divider for value animation (higher=slower)
                     step = (distance >> valueAnimShift) + 1
                     Recommended: 3-6, default: GAUGE_DEFAULT_VALUE_ANIM_SHIFT (4)
   trailAnimShift:   Speed divider for trail shrink (higher=slower)
                     Recommended: 3-6, default: GAUGE_DEFAULT_TRAIL_ANIM_SHIFT (4)
   blinkShift:       Speed divider for blink rate (higher=slower)
                     frequency = blinkTimer >> blinkShift
                     Recommended: 2-4, default: GAUGE_DEFAULT_BLINK_SHIFT (3 = ~7.5Hz @ 60fps)

   ============================================================================= */
typedef struct
{
    /* --- 16-bit / pointer fields first (word-aligned on 68000) --- */

    u16 maxValue;                   /* Maximum gauge value */
    u16 currentValue;               /* Current logical value (0..maxValue) */
    u16 maxFillPixels;              /* Maximum fill width in pixels */
    const u16 *valueToPixelsLUT;    /* Optional LUT for non-linear mapping (NULL = auto) */

    u16 valueTargetPixels;          /* Target pixel value (for animation) */
    u16 valuePixels;                /* Current displayed pixel value */
    u16 trailPixels;                /* Current trail pixel position */
    u16 blinkTimer;                 /* Internal blink phase counter */

    /* Render cache (for change detection) */
    u16 lastValuePixels;            /* Last rendered value pixels */
    u16 lastTrailPixelsRendered;    /* Last rendered trail pixels */

    /* --- 8-bit fields (packed, no padding) --- */

    u8 holdFramesRemaining;         /* Frames until trail starts shrinking */
    u8 blinkFramesRemaining;        /* Frames of blinking remaining */
    u8 valueAnimEnabled;            /* 0=instant, 1=animated value changes */
    u8 valueAnimShift;              /* Value animation speed (higher=slower) */
    u8 trailAnimShift;              /* Trail shrink speed (higher=slower) */
    u8 blinkShift;                  /* Blink frequency (higher=slower) */
    u8 trailEnabled;                /* Enable trail effect (0=no trail) */
    u8 trailMode;                   /* GAUGE_TRAIL_NONE / DAMAGE / GAIN */
    u8 lastBlinkOn;                 /* Last blink state */
    u8 lastTrailMode;               /* Last trail mode (for render cache) */
    u8 needUpdate;                  /* 1 = animations running or pending render, 0 = fully idle (Gauge_update skips) */

} GaugeLogic;


/* =============================================================================
   Internal structures (used by GaugePart, exposed for sizeof)
   ============================================================================= */

/**
 * Per-cell streaming data for fixed VRAM mode.
 * Each cell has its own VRAM tile that gets updated via DMA.
 *
 * Strip pointers (pre-cached at init to avoid per-frame segment lookups):
 * - bodyFillStrip45:   main interior strip (45 tiles, always set)
 * - endFillStrip45:    termination strip (NULL if segment has no end tileset)
 * - breakFillStrip45:  transition strip (NULL if no end, else falls back to body)
 * - trailFillStrip64:  trail strip (64 tiles, NULL if no trail tileset)
 * - bridgeFillStrip45: bridge strip (NULL if no bridge tileset for this segment)
 */
typedef struct
{
    const u32 *bodyFillStrip45;     /* BODY strip (always set when cell is valid) */
    const u32 *endFillStrip45;      /* END strip (NULL if not supported) */
    const u32 *breakFillStrip45;    /* BREAK strip (NULL if not supported) */
    const u32 *trailFillStrip64;    /* TRAIL strip (NULL if not supported) */
    const u32 *bridgeFillStrip45;   /* BRIDGE strip (NULL if not supported) */
    const u32 *loadedFillStrip45;   /* Last strip uploaded (for cache) */
    u16 vramTileIndex;              /* VRAM tile index for this cell */
    u8 loadedFillIdx;               /* Last fill index uploaded (0xFF=none) */
    u8 cellIndex;                   /* Index in layout (for fill calculation) */
} GaugeStreamCell;


/**
 * Dynamic VRAM mode data.
 *
 * In dynamic mode, VRAM is shared across cells. Pre-loaded "standard" tiles
 * (empty, full value, full trail) exist per segment and never change.
 * "Partial" tiles are streamed on demand when a cell needs a unique fill
 * (e.g., the value/trail edge cell). Only 1 partial tile per type per
 * GaugePart, so only one cell at a time can show each partial type.
 *
 * Additional dedicated VRAM tiles exist for bridges and caps, since these
 * use different tilesets and can't share the standard segment tiles.
 *
 * Change detection caches (loadedFillIdx*, loadedSegment*) avoid redundant
 * DMA transfers when the same tile content is already in VRAM.
 */
typedef struct
{
    /* --- Standard tiles per segment --- */
    u16 vramTileEmpty[GAUGE_MAX_SEGMENTS];       /* Empty tile per segment (0,0) */
    u16 vramTileFullValue[GAUGE_MAX_SEGMENTS];   /* Full value tile per segment (8,8) */
    u16 vramTileFullTrail[GAUGE_MAX_SEGMENTS];   /* Full trail tile per segment (0,8) */
    u16 vramTileBridge[GAUGE_MAX_SEGMENTS];      /* Bridge tile per segment (dynamic) */
    u16 vramTileCapStart;                        /* Cap start tile (per part) */
    u16 vramTileCapEnd;                          /* Cap end tile (per part) */

    /* --- Partial tiles (streamed on demand, scalars per GaugePart) --- */
    u16 vramTilePartialValue;       /* Partial value tile - also used for "both" case */
    u16 vramTilePartialTrail;       /* Partial trail tile (value=0, trail=1-7 px) */
    u16 vramTilePartialEnd;         /* End cap tile (value/trail frontier) */
    u16 vramTilePartialTrailSecond; /* Second trail-break tile (before END) */

    /* --- Cache for loaded partial tiles --- */
    u8 loadedSegmentPartialValue;   /* Which segment's partial value is loaded */
    u8 loadedFillIdxPartialValue;   /* Loaded fill index for partial value */
    u8 loadedSegmentPartialTrail;   /* Which segment's partial trail is loaded */
    u8 loadedFillIdxPartialTrail;   /* Loaded fill index for partial trail */
    u8 loadedSegmentPartialEnd;     /* Which segment's END tile is loaded */
    u8 loadedFillIdxPartialEnd;     /* Loaded fill index for END */
    u8 loadedSegmentPartialTrailSecond; /* Which segment's second trail break is loaded */
    u8 loadedFillIdxPartialTrailSecond; /* Loaded fill index for second trail break */
    u8 loadedFillIdxBridge[GAUGE_MAX_SEGMENTS]; /* Loaded fill index for bridge tile */
    u8 loadedFillIdxCapStart;           /* Loaded fill index for cap start tile */
    u8 loadedFillIdxCapEnd;             /* Loaded fill index for cap end tile */
    u8 loadedCapStartUsesBreak;         /* 1 if cap start break strip is loaded */
    u8 loadedCapStartUsesTrail;         /* 1 if cap start trail strip is loaded */

    /* --- Tilemap cache (for change detection) --- */
    u16 cellCurrentTileIndex[GAUGE_MAX_LENGTH];  /* Currently displayed VRAM tile per cell */

    /* --- Cell validity (pre-computed for CPU optimization) ---
     * Avoids checking tilesetBySegment[segId] != NULL in render loop.
     * Trade-off: 16 bytes SRAM vs 2 memory accesses + 1 branch per cell per frame.
     */
    u8 cellValid[GAUGE_MAX_LENGTH];              /* 1 if cell has valid tileset, 0 otherwise */

} GaugeDynamic;


/* =============================================================================
   GaugePart â€” VRAM streaming engine (internal)
   =============================================================================

   Renders a gauge on the WINDOW plane using ROM->VRAM tile streaming.
   Managed internally by Gauge; users typically don't interact directly.
   One part = one section of a gauge (e.g., 2-lane effect = 2 parts).

   FEATURES:
   - Copies GaugeLayout at init (includes visual properties)
   - Supports dynamic or fixed VRAM allocation
   - Optimized tile streaming with change detection

   ============================================================================= */
typedef struct
{
    /* --- Position --- */
    u16 originX;                    /* Tilemap X origin */
    u16 originY;                    /* Tilemap Y origin */

    /* --- VRAM --- */
    GaugeVramMode vramMode;         /* Fixed or dynamic mode */
    u16 vramBase;                   /* Base VRAM tile index */

    /* --- Layout copy (includes visual properties) --- */
    GaugeLayout layout;

    /* --- Fixed mode data --- */
    GaugeStreamCell cells[GAUGE_MAX_LENGTH];    /* Per-cell streaming data */
    u8 cellCount;                               /* Active cell count */

    /* --- Dynamic mode data --- */
    GaugeDynamic dyn;

} GaugePart;


/* =============================================================================
   Gauge â€” High-level gauge container
   =============================================================================

   Encapsulates one GaugeLogic and multiple GaugePart instances.
   This is the main object users interact with.

   USAGE PATTERN:
   1. Declare parts array: GaugePart parts[N];
   2. Initialize gauge: Gauge_init(&gauge, ...);
   3. Add parts: Gauge_addPart(&gauge, ...);
   4. Game loop: Gauge_update(&gauge);
   5. Change value: Gauge_decrease/increase(&gauge, ...);

   MULTIPLE PARTS:
   Multiple parts can share the same logic for perfect synchronization.
   Use cases:
   - 2-lane gauge (2 parts at adjacent positions)
   - P1 + P2 mirrored gauges
   - Extended gauge (multiple segments with different fillOffset)

   ============================================================================= */
struct Gauge
{
    /* --- Logic (value state machine) --- */
    GaugeLogic logic;

    /* --- Parts (rendering units) --- */
    GaugePart *parts;           /* Array of parts (user-allocated) */
    u8 partCount;               /* Number of active parts */

    /* --- VRAM allocation --- */
    u16 vramBase;                   /* Base VRAM tile index */
    u16 vramNextOffset;             /* Offset for next part allocation */
    GaugeVramMode vramMode;         /* Default VRAM mode for parts */
};


/* -----------------------------------------------------------------------------
   Gauge API
   ----------------------------------------------------------------------------- */

/**
 * Initialize gauge with embedded logic.
 * Trail and value animation are disabled by default.
 * Use Gauge_setTrailAnim() and Gauge_setValueAnim() to configure animations.
 *
 * @param gauge         Gauge to initialize
 * @param maxValue      Maximum value (e.g., 100 for percentage)
 * @param maxFillPixels Maximum fill width in pixels (usually length*8)
 * @param initialValue  Starting value (will be clamped to maxValue)
 * @param parts         Pointer to user-allocated array of GaugePart
 * @param vramBase      Base VRAM tile index for allocation
 * @param vramMode      Default VRAM mode (GAUGE_VRAM_FIXED or GAUGE_VRAM_DYNAMIC)
 */
void Gauge_init(Gauge *gauge,
                u16 maxValue,
                u16 maxFillPixels,
                u16 initialValue,
                GaugePart *parts,
                u16 vramBase,
                GaugeVramMode vramMode);

/**
 * Configure value animation.
 *
 * @param gauge    Gauge to configure
 * @param enabled  Enable animated value changes (0=instant, 1=animated)
 * @param shift    Animation speed divider (3-6 recommended, 0=use default 4)
 */
void Gauge_setValueAnim(Gauge *gauge, u8 enabled, u8 shift);

/**
 * Configure trail animation.
 *
 * @param gauge      Gauge to configure
 * @param enabled    Enable trail effect (0=no trail, 1=trail with hold/blink/shrink)
 * @param shift      Trail shrink speed divider (3-6 recommended, 0=use default 4)
 * @param blinkShift Blink frequency divider (2-4 recommended, 0=use default 3)
 */
void Gauge_setTrailAnim(Gauge *gauge, u8 enabled, u8 shift, u8 blinkShift);

/**
 * Add a part to the gauge (simplified).
 * VRAM is allocated automatically from gauge's vramBase.
 *
 * @param gauge     Parent gauge
 * @param part      Part to add (must be in gauge's parts array)
 * @param layout    Layout configuration (copied, includes visual properties)
 * @param originX   Tilemap X position
 * @param originY   Tilemap Y position
 */
void Gauge_addPart(Gauge *gauge,
                   GaugePart *part,
                   const GaugeLayout *layout,
                   u16 originX,
                   u16 originY);

/**
 * Add a part with custom VRAM configuration.
 * Use when you need specific VRAM placement or different VRAM mode.
 *
 * @param gauge     Parent gauge
 * @param part      Part to add
 * @param layout    Layout configuration (copied)
 * @param originX   Tilemap X position
 * @param originY   Tilemap Y position
 * @param vramBase  Custom VRAM base tile index
 * @param vramMode  Custom VRAM mode (overrides gauge default)
 */
void Gauge_addPartEx(Gauge *gauge,
                     GaugePart *part,
                     const GaugeLayout *layout,
                     u16 originX,
                     u16 originY,
                     u16 vramBase,
                     GaugeVramMode vramMode);

/**
 * Update gauge: tick logic + render all parts.
 * Call once per frame.
 *
 * Performs:
 * - Value animation (if enabled)
 * - Trail animation (hold, blink, shrink)
 * - Tile streaming for all parts
 * - Early return optimization if nothing changed
 */
void Gauge_update(Gauge *gauge);

/**
 * Set gauge value directly (instant, no trail effect).
 *
 * @param gauge     Gauge to modify
 * @param newValue  New value (clamped to maxValue)
 */
void Gauge_setValue(Gauge *gauge, u16 newValue);

/**
 * Decrease gauge value (damage).
 * Trail holds then blinks before shrinking toward new value.
 *
 * @param gauge       Gauge to modify
 * @param amount      Amount to decrease
 * @param holdFrames  Frames to hold trail before blinking
 * @param blinkFrames Frames to blink before shrinking
 */
void Gauge_decrease(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames);
 
/**
 * Increase gauge value (heal).
 * When valueAnim is enabled, can trigger a gain trail (lead blink then catch-up).
 *
 * @param gauge   Gauge to modify
 * @param amount  Amount to increase
 * @param holdFrames  Frames to hold gain trail before blinking
 * @param blinkFrames Frames to blink gain trail before value catches up
 */
void Gauge_increase(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames);

/**
 * Get current gauge value.
 */
static inline u16 Gauge_getValue(const Gauge *gauge)
{
    return gauge->logic.currentValue;
}

/**
 * Get maximum gauge value.
 */
static inline u16 Gauge_getMaxValue(const Gauge *gauge)
{
    return gauge->logic.maxValue;
}

/**
 * Check if gauge is empty (value == 0).
 */
static inline u8 Gauge_isEmpty(const Gauge *gauge)
{
    return gauge->logic.currentValue == 0;
}

/**
 * Check if gauge is full (value == maxValue).
 */
static inline u8 Gauge_isFull(const Gauge *gauge)
{
    return gauge->logic.currentValue == gauge->logic.maxValue;
}


/* -----------------------------------------------------------------------------
   Utility functions
   ----------------------------------------------------------------------------- */

/**
 * Compute VRAM tiles needed for a layout (for manual allocation).
 *
 * The size is derived from the current gauge configuration:
 * - gauge->vramMode
 * - gauge->logic.trailEnabled
 *
 * @param gauge       Gauge configuration source (must be initialized)
 * @param layout      Layout configuration
 * @return Number of VRAM tiles needed
 */
u16 Gauge_getVramSize(const Gauge *gauge,
                      const GaugeLayout *layout);


/* -----------------------------------------------------------------------------
   Low-level API (for advanced usage)
   ----------------------------------------------------------------------------- */

/**
 * Initialize GaugeLogic directly.
 * Use when you need manual control over logic separate from Gauge container.
 */
void GaugeLogic_init(GaugeLogic *logic,
                     u16 maxValue,
                     u16 maxFillPixels,
                     const u16 *valueToPixelsLUT,
                     u8 trailEnabled,
                     u16 initialValue);

/**
 * Initialize GaugeLogic with custom animation.
 */
void GaugeLogic_initWithAnim(GaugeLogic *logic,
                             u16 maxValue,
                             u16 maxFillPixels,
                             const u16 *valueToPixelsLUT,
                             u8 trailEnabled,
                             u16 initialValue,
                             u8 valueAnimEnabled,
                             u8 valueAnimShift,
                             u8 trailAnimShift,
                             u8 blinkShift);

/**
 * Tick logic state machine.
 * Called automatically by Gauge_update().
 */
void GaugeLogic_tick(GaugeLogic *logic);


#endif /* GAUGE_H */
