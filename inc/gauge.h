#ifndef GAUGE_H
#define GAUGE_H

#include <genesis.h>

/* =============================================================================
   gauge.h / gauge.c - Pixel-perfect HUD gauge for SGDK / Mega Drive
   =============================================================================

   WHAT IS A GAUGE?
   ----------------
   A gauge is a visual bar (health bar, energy bar, etc.) rendered on the
   Mega Drive's tile-based hardware. It fills/empties pixel by pixel to show
   a value between 0 and maxValue.

   Example: a 12-tile horizontal health bar
     [################............]   value = 60%
      <-- filled -->  <-- empty -->


   ============================================================================
   VISUAL CONCEPTS
   ============================================================================

   CELLS AND FILL
   ~~~~~~~~~~~~~~
   A gauge is a row of cells. Each cell is one 8x8 pixel tile on screen.
   The gauge fills pixel by pixel (0..8 px per cell), left to right:

     Cell:        [  0  ] [  1  ] [  2  ] [  3  ] [  4  ]
     Fill px:     |<-8->| |<-8->| |<-8->| |<-8->| |<-8->|
     Total:       40 pixels of fill resolution (5 cells x 8 px)

   When value = 26 pixels (out of 40), cells 0-2 are full (24 px),
   and cell 3 is partially filled (2 px):

     [########] [########] [########] [##......] [........]
       full=8     full=8     full=8   partial=2   empty=0


   SEGMENTS
   ~~~~~~~~
   Each cell belongs to a "segment" -- a group of cells sharing the same
   tileset (graphic style). Segments allow visually distinct zones:

     Cell:      [ 0 ][ 1 ][ 2 ][ 3 ][ 4 ][ 5 ][ 6 ][ 7 ][ 8 ]
     Segment:     0    0    0    1    1    1    2    2    2
     Visual:   |<-- green -->|<-- yellow -->|<--- red --->|

   Each segment can have its own set of pre-rendered tile strips.
   The renderer picks the correct strip based on which segment a cell
   belongs to.


   TILESETS (TILE STRIPS)
   ~~~~~~~~~~~~~~~~~~~~~~
   Art is organized as "tile strips" -- pre-rendered sequences of tiles
   in ROM covering all possible fill combinations:

   BODY strip (45 tiles):
     Contains one tile for every valid (valuePx, trailPx) combination
     where 0 <= valuePx <= trailPx <= 8. These 45 combinations form
     a triangular matrix. Index 0 = empty (0,0), index 44 = full (8,8).

   END strip (45 tiles, optional):
     Same layout as BODY, but drawn with a different graphic style for the
     cell where the value/trail edge falls (the "termination" cell).

   TRAIL strip (64 tiles, optional):
     For cells showing trail-specific rendering. 64 tiles cover the full
     9x9 matrix minus the diagonal (trail is always >= value in these cells).

   BRIDGE strip (45 tiles, optional): see BRIDGE section below.

   CAP strips (45 tiles each, optional): see CAPS section below.


   TRAIL (DAMAGE AND GAIN)
   ~~~~~~~~~~~~~~~~~~~~~~~
   The trail is a ghost bar that follows the gauge value with a delay.

   DAMAGE TRAIL: when the gauge decreases, the old value position stays
   as a trail. The trail holds, blinks, then shrinks toward the new value.

     Time 0 (damage happens):
       [########|########|########|===TRAIL=|==TRAIL==|........]
        value=24 ------>            <--- trail stays at 40 --->

     Time 1 (hold phase -- trail stays still):
       [########|########|########|===TRAIL=|==TRAIL==|........]

     Time 2 (blink phase -- trail blinks on/off):
       [########|########|########|        HIDDEN       |........]
       [########|########|########|===TRAIL=|==TRAIL==|........]

     Time 3 (shrink phase -- trail moves toward value):
       [########|########|########|=TRAIL|...................]

     Time 4 (done -- trail caught up):
       [########|########|########|.............................]

   GAIN TRAIL: when the gauge increases, the trail leads ahead of value.
   The value then catches up after hold+blink.

     Time 0 (heal happens):
       [########|###value|===TRAIL=|==TRAIL==|........|........]
        value moving -->   <--- trail at new target

     Time 1 (value catches up after blink):
       [########|########|########|########|........|........]

   Timeline summary:
     Gauge_decrease() -> hold (N frames) -> blink (M frames) -> shrink -> done
     Gauge_increase() -> hold (N frames) -> blink (M frames) -> value catches up -> done


   BLINK-OFF
   ~~~~~~~~~
   During the blink phase, the trail alternates every few frames between:
   - Normal appearance (blink ON)  -- trail is visible
   - Blink-off appearance (blink OFF) -- trail uses a "blink-off" tileset

   The blink-off tileset can show:
   - Blank tiles (trail disappears) -- classic Street Fighter style
   - A dimmed/faded version of the trail
   - Any other custom graphic

   Blink rate is controlled by blinkShift:
     blinkShift=1 -> toggles every  2 frames (fast)
     blinkShift=2 -> toggles every  4 frames
     blinkShift=3 -> toggles every  8 frames (default, ~7.5 Hz @ 60fps)
     blinkShift=4 -> toggles every 16 frames (slow)


   CAPS (CAP START / CAP END)
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   Caps are decorative border tiles at the ends of the gauge. They always
   stay in place but change their tileset depending on the gauge state.

   CAP END: the last cell in fill order. Always uses its dedicated tileset.
     It shows partial fill like a normal cell, but with a rounded/bordered look.

   CAP START: the first cell in fill order. Has 3 visual variants:
     - capStart:      used when the value edge is in this cell (normal fill)
     - capStartBreak: used when the cell is fully filled or empty
     - capStartTrail: used when the trail zone covers this cell

   Example (8-cell gauge with caps):

     Without caps:
       [  body  |  body  |  body  |  body  |  body  |  body  |  body  |  body  ]

     With cap start + cap end:
       [cap_strt|  body  |  body  |  body  |  body  |  body  |  body  |cap_end ]
        ^                                                                ^
        Always uses capStart tileset                   Always uses capEnd tileset

   Visual behavior as value changes:

     value = 100%:
       [CS_break|  FULL  |  FULL  |  FULL  |  FULL  |  FULL  |  FULL  | CE=100%]

     value = 75%:
       [CS_break|  FULL  |  FULL  |  FULL  |  FULL  |##edge..|  empty | CE=0%  ]

     value = 10%:
       [CS_fill |  empty |  empty |  empty |  empty |  empty |  empty | CE=0%  ]
        ^ capStart shows partial fill since value edge is here

     (CS = cap start, CE = cap end)


   BRIDGE
   ~~~~~~
   When two segments meet, the boundary between them can look jarring if
   one segment's fill style doesn't align with the next. A bridge is a
   special tileset for the last cell of a segment that smoothly transitions
   to the next segment's visual style.

   Without bridge (2 segments: green, red):
     Cell:     [grn|grn|grn|grn|red|red|red|red]
     Problem:   hard visual cut at segment boundary ----^

   With bridge:
     Cell:     [grn|grn|grn|BRG|red|red|red|red]
                            ^ bridge cell: uses a tileset that blends
                              green (left) into red (right)

   The bridge tile mirrors the fill state of the NEXT cell to ensure
   seamless visual transition. When the value edge passes through the
   bridge, it shows the correct partial fill.

   BREAK cell: the cell BEFORE the bridge is forced to show as fully
   filled. This prevents visual artifacts where a partially-filled cell
   sits next to a bridge transition tile.

     [grn_full|grn_BREAK|grn_BRIDGE|red|red|red]
                 ^            ^
                 forced full   bridge tile (blends to next segment)


   ============================================================================
   ARCHITECTURE -- THE 4 MAIN STRUCTURES
   ============================================================================

   GaugeLayout -- "What does the gauge look like?"
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Static visual configuration, set once at init time. Describes:
   - How many cells (tiles) the gauge has
   - Which segment each cell belongs to (for tileset selection)
   - All tileset pointers (body, end, trail, bridge, caps, blink-off, gain)
   - Visual properties (orientation, palette, flip flags, priority)
   - Fill direction (forward or reverse)

   You typically create one GaugeLayout per gauge visual design, then pass
   it to Gauge_addPart(). The layout is retained by each part (refcounted),
   so rebuild/reconfigure only after all referencing gauges are released.


   GaugeLogic -- "What is the gauge's current value?"
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   State machine ticked every frame. Manages:
   - currentValue -> valueTargetPixels -> valuePixels (animated)
   - trailPixels (holds, blinks, shrinks toward value)
   - blinkTimer, holdFramesRemaining, blinkFramesRemaining
   - Animation speed parameters (valueAnimShift, trailAnimShift, blinkShift)

   One GaugeLogic per Gauge. All parts read from the same logic,
   so multiple parts always show the same value (perfect sync).


   GaugePart -- "Where and how is the gauge drawn on screen?"
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   The VRAM rendering engine. One part = one visual instance on screen.
   Each part has:
   - A retained GaugeLayout reference (shared visual config with refcount)
   - A screen position (originX, originY in tilemap coordinates)
   - A VRAM allocation (vramBase + tiles)
   - A render handler (selected at init based on VRAM mode + value mode)

   Each frame, the part reads the shared GaugeLogic and streams the
   correct tile data from ROM strips to VRAM.

   WHY MULTIPLE PARTS?
   A single Gauge can have multiple GaugeParts for these use cases:

   1. 2-lane gauge (fake 2-tile height):
        Part 0: [####====........]  at Y=5
        Part 1: [####====........]  at Y=6  (same layout, adjacent row)

   2. 2-lane with shorter bottom row (like Sample 7):
        Part 0: [####====........]  12 cells, fillOffset=0  (full row)
        Part 1: [####]              3 cells,  fillOffset=8  (short row)
      Part 1 uses fillOffset to shift which pixel range its cells show.
      See fillOffset field reference for a detailed visual explanation.

   IMPORTANT: all parts of a Gauge share the SAME GaugeLogic (same value).
   For two independent gauges (e.g., P1 and P2 health bars with different HP),
   you need two separate Gauge objects, each with its own GaugeLogic.
   A mirrored P2 gauge uses GaugeLayout_makeMirror() on a SEPARATE Gauge.

   VRAM MODES:
   - FIXED:   Each cell gets its own VRAM tile. Tile data is DMA'd from
              ROM when it changes. Simple but uses more VRAM.
              Best for: mirrors, small gauges, when VRAM is plentiful.
   - DYNAMIC: All cells share a small pool of VRAM tiles. The tilemap is
              updated to point each cell at the correct shared tile.
              Uses less VRAM but more CPU (tilemap writes).
              Best for: large gauges, when VRAM is limited.


   Gauge -- "The main object you create and interact with"
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   High-level container that binds everything together:
   - 1 embedded GaugeLogic (value state machine)
   - N GaugeParts (rendering units, allocated internally)
   - VRAM allocation state (auto-increments as parts are added)

   This is the only struct most users need to interact with.
   Call Gauge_init() once, add parts, then Gauge_update() every frame.


   ============================================================================
   VALUE MODES
   ============================================================================

   FILL MODE (default):
   Classic continuous gauge. Each tile fills pixel by pixel (0..8 px).
   Total fill resolution = cellCount * 8 pixels.

     value=0:    [........|........|........|........]
     value=16:   [########|########|........|........]
     value=20:   [########|########|####....|........]
     value=32:   [########|########|########|########]

   PIP MODE:
   Discrete gauge. Each "pip" is 1 or more tiles that switch between
   5 visual states (no partial fill):
     EMPTY     = pip is not reached by value or trail
     VALUE     = pip is covered by the current value
     LOSS      = pip is in the damage trail zone (lost recently)
     GAIN      = pip is in the gain trail zone (gained recently)
     BLINK_OFF = pip is in trail zone during blink-off frame

   Example: 5-pip gauge, value=3, trail covers pip 3 (damage):
     [VALUE] [VALUE] [VALUE] [LOSS ] [EMPTY]


   ============================================================================
   TYPICAL USAGE
   ============================================================================

   ```c
   static Gauge myGauge;
   static GaugeLayout myLayout;

   // 1. Initialize layout with visual properties
   GaugeLayout_init(&myLayout, 8, GAUGE_FILL_FORWARD, tilesets, segments,
                    GAUGE_ORIENT_HORIZONTAL, PAL0, 1, 0, 0);

   // 2. Initialize gauge (maxFillPixels = myLayout.length * 8 = 64, auto)
   Gauge_init(&myGauge, &(GaugeInit){
       .maxValue = 100,
       .initialValue = 100,
       .layout = &myLayout,
       .vramBase = VRAM_BASE,
       .vramMode = GAUGE_VRAM_DYNAMIC,
       .valueMode = GAUGE_VALUE_MODE_FILL
   });

   // 3. Add parts (VRAM auto-allocated sequentially)
   Gauge_addPart(&myGauge, &myLayout, 2, 5);
   Gauge_addPart(&myGauge, &myLayout, 2, 6);

   // 4. Game loop
   while(1) {
       Gauge_update(&myGauge);  // Tick logic + render all parts
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

/* Maximum logical value (maxValue) supported by the LUT buffer.
 * Each Gauge allocates a value-to-pixels lookup table of
 * (GAUGE_LUT_CAPACITY + 1) entries when needed.
 *
 * Any maxValue passed to Gauge_init() must be <= GAUGE_LUT_CAPACITY.
 * Set this to the largest maxValue you'll use across all gauges.
 *
 * Default: 160 (= 20 tiles * 8 pixels/tile).
 *
 * Worst-case RAM cost per Gauge instance: (GAUGE_LUT_CAPACITY + 1) * 2 bytes.
 *   GAUGE_LUT_CAPACITY  96  -> 194 bytes/gauge (12-tile gauges)
 *   GAUGE_LUT_CAPACITY 128  -> 258 bytes/gauge (16-tile gauges)
 *   GAUGE_LUT_CAPACITY 160  -> 322 bytes/gauge (20-tile gauges, default)
 *
 * Note: FILL mode 1:1 (maxValue == maxFillPixels) does not need the LUT.
 * LUT allocation is dynamic and can be omitted in this case.
 */
#ifndef GAUGE_LUT_CAPACITY
#define GAUGE_LUT_CAPACITY  160
#endif

/* Tile properties */
#define GAUGE_PIXELS_PER_TILE  8   /* Pixels per tile (8x8) */
#define GAUGE_FILL_TILE_COUNT 45   /* Tiles in a 45-tile fill strip */
#define GAUGE_TILE_U32_COUNT   8   /* 32-bit words per tile (8x8 @ 4bpp = 32 bytes) */

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
 *
 * FIXED:   Each cell gets its own VRAM tile. When a cell changes, 32 bytes
 *          of tile data are DMA'd to its slot. Simple but uses N VRAM tiles
 *          for an N-cell gauge.
 *          Best for: small gauges, mirrors, when VRAM is plentiful.
 *
 * DYNAMIC: All cells share a small pool of VRAM tiles (3-7 tiles typical).
 *          The tilemap is updated to point cells at the right shared tile.
 *          Uses much less VRAM but more CPU (tilemap writes each frame).
 *          Best for: large gauges, when VRAM is tight.
 *
 *   VRAM usage comparison for a 10-cell gauge with trail:
 *     FIXED:   10 tiles (1 per cell)
 *     DYNAMIC: ~5-7 tiles (3 standard + 2-4 partial/bridge/cap)
 */
typedef enum
{
    GAUGE_VRAM_FIXED   = 0,
    GAUGE_VRAM_DYNAMIC = 1
} GaugeVramMode;

/**
 * Gauge value mode.
 *
 * FILL: Classic continuous gauge. Each tile fills pixel by pixel (0..8 px).
 *       This is the standard mode for health bars and energy gauges.
 *       Example: [########|####....|........]  (16 px filled out of 24)
 *
 * PIP:  Discrete gauge. Each "pip" is 1 or more tiles that switch between
 *       5 states: EMPTY, VALUE, LOSS, GAIN, BLINK_OFF. No partial fill.
 *       Example: [VALUE] [VALUE] [VALUE] [LOSS ] [EMPTY]  (3/5 pips filled)
 *
 *       PIP setup requires:
 *       - Configure compact strips via GaugeLayout_setPipStyles()
 *       - maxValue must equal the layout pip count
 */
typedef enum
{
    GAUGE_VALUE_MODE_FILL = 0,
    GAUGE_VALUE_MODE_PIP  = 1
} GaugeValueMode;

/* =============================================================================
   Forward declarations (needed for circular references)
   ============================================================================= */
typedef struct Gauge Gauge;
typedef struct GaugePart GaugePart;

/**
 * Per-gauge update handler (tick logic + render all parts).
 * Selected once at init based on GaugeValueMode.
 */
typedef void GaugeTickAndRenderHandler(Gauge *gauge);

/**
 * Per-part render handler.
 * Selected once when the part is added, based on value mode and VRAM mode.
 */
typedef void GaugePartRenderHandler(GaugePart *part,
                                    u16 valuePixels,
                                    u16 trailPixelsRendered,
                                    u16 trailPixelsActual,
                                    u8 blinkOffActive,
                                    u8 blinkOnChanged,
                                    u8 trailMode,
                                    u8 trailModeChanged);


/* =============================================================================
   GaugeLayout -- Geometry + visual configuration
   =============================================================================

   A GaugeLayout describes what a gauge looks like. It is a static configuration
   set once at init time and never modified at runtime.

   It contains:
   - How many cells the gauge has
   - Which segment (tileset style) each cell uses
   - Pointers to all the pre-rendered tile strips in ROM
   - Visual properties (orientation, palette, flip flags)
   - Fill direction (forward or reverse)

   FIELD REFERENCE:
   ----------------

   length:
     Number of cells (tiles) in this gauge (1..GAUGE_MAX_LENGTH).
     Each cell = one 8x8 tile = 8 pixels of fill resolution.
     A 10-cell gauge has 10 * 8 = 80 pixels of fill resolution.

   segmentIdByCell[cellIndex]:
     Maps each cell to its segment. The segment ID determines which
     tileset is used for that cell. Example with 3 segments:
       segmentIdByCell = {0, 0, 0, 1, 1, 1, 2, 2, 2}
       Cells 0-2 use segment 0's tileset (e.g., green)
       Cells 3-5 use segment 1's tileset (e.g., yellow)
       Cells 6-8 use segment 2's tileset (e.g., red)

   fillIndexByCell[cellIndex]:
     Maps each cell to its position in fill order. In FORWARD mode,
     fillIndex == cellIndex. In REVERSE mode, the order is flipped so
     the last cell fills first.
     Set automatically by GaugeLayout_setFillForward/Reverse().

   cellIndexByFillIndex[fillIndex]:
     Inverse of fillIndexByCell. Given a fill position, returns the
     corresponding cellIndex. Used for O(1) lookups instead of scanning.

   fillOffset:
     Pixel offset that shifts where this part's cells start sampling
     the gauge value (default 0). Each cell computes its fill as:

       cellStartPixel = fillOffset + fillIndex * 8
       pixelsInCell   = clamp(valuePixels - cellStartPixel, 0, 8)

     With fillOffset=0 (default), cell 0 reacts to pixels 0..7,
     cell 1 to pixels 8..15, etc. Increasing fillOffset shifts
     the entire part further into the gauge value range.

     USE CASE: 2-lane gauge with a shorter bottom row (like Sample 7 with bevels).

       Part1: 12 cells, fillOffset = 0       (covers pixels 0..95)
       Part2:  3 cells, fillOffset = 8       (covers pixels 8..31)

       Screen layout (both parts share the same Gauge / same value):
         Y=23  Part1: [cell0|cell1|cell2|cell3|cell4|...|cell11]
         Y=24  Part2: [cell0|cell1|cell2]

       How Part2 samples the gauge value (fillOffset=8):
         Part2 cell 0 → pixels  8..15 (same range as Part1 cell 1)
         Part2 cell 1 → pixels 16..23 (same range as Part1 cell 2)
         Part2 cell 2 → pixels 24..31 (same range as Part1 cell 3)

       When value = 96 (full):
         Part1: [FULL |FULL |FULL |FULL |FULL |...|FULL ]
         Part2: [FULL |FULL |FULL ]    (all cells > 31px → full)

       When value = 20:
         Part1: [FULL |FULL |##..|....|....|...|....]
         Part2: [FULL |##..|....]
                   ^      ^
                   |      Part2 cell1: clamp(20 - 16) = 4px
                   Part2 cell0: clamp(20 - 8) = 8px → full

       When value = 5:
         Part1: [#####|....|....|....|....|...|....]
         Part2: [....|....|....]   (all cells < 8px → empty)

     The formula to calculate fillOffset for a bottom row:
       fillOffset = triggerPixel - (partLength * 8)
     where triggerPixel is the gauge pixel value at which the last
     cell of the bottom row should become completely full.

     Configure via GaugeLayout_setFillOffset().


   BODY TILESET -- tilesetBySegment[segmentId]:
     The main 45-tile strip for interior cells. This is the primary tileset
     that draws the gauge fill. Every segment must have at least a BODY
     tileset (or a gain variant). Contains one pre-rendered tile for every
     valid (valuePx, trailPx) combination.

   END TILESET -- tilesetEndBySegment[segmentId]:
     Optional 45-tile strip for the "termination" cell -- the cell where
     the value/trail edge falls. Typically drawn with a rounded or capped
     edge to make the gauge tip look polished.
     If NULL, the BODY tileset is used everywhere (no special edge tile).

     Example (E = END cell, F = full body, V = empty):
       value at cell 4:
       [  F  ][  F  ][  F  ][  F  ][  E  ][  V  ][  V  ][  V  ]
                                      ^ END tileset used here

   TRAIL TILESET -- tilesetTrailBySegment[segmentId]:
     Optional 64-tile strip for trail-specific rendering. Used for cells
     in the "trail break" zone (the cell where the trail edge falls).
     If NULL, trail rendering uses the BODY tileset with the standard
     tile index lookup.

   BRIDGE TILESET -- tilesetBridgeBySegment[segmentId]:
     Optional 45-tile strip for segment boundary transitions.
     See BRIDGE section in the file header for full explanation.
     Placed at the last cell of a segment before a different segment begins.
     The bridge tile shows the fill state of the NEXT cell (from the next
     segment) to create a smooth visual blend.
     If NULL, a forced BREAK (fully filled BODY tile) is used instead.

   CAP START TILESET -- tilesetCapStartBySegment[segmentId]:
     Optional 45-tile strip for the first cell in fill order.
     See CAPS section in the file header for full explanation.
     This cell always uses a cap tileset instead of the normal BODY.
     The cap start has 3 visual variants depending on gauge state:

       Gauge state at cell 0      | Tileset used
       ---------------------------|---------------------------
       Value edge is in this cell | capStart (normal fill)
       Trail zone covers cell     | capStartTrail
       Cell fully filled or empty | capStartBreak

   CAP END TILESET -- tilesetCapEndBySegment[segmentId]:
     Optional 45-tile strip for the last cell in fill order.
     Always rendered using this tileset regardless of gauge state.
     Typically shows a rounded/bordered end cap.
     Enabled per-segment via capEndBySegment[segmentId] = 1.

   CAP START BREAK -- tilesetCapStartBreakBySegment[segmentId]:
     Variant tileset for cap start when the cell is fully filled or
     the value hasn't arrived yet. Falls back to capStart if NULL.

   CAP START TRAIL -- tilesetCapStartTrailBySegment[segmentId]:
     Variant tileset for cap start when trail is visible in this cell.

   capEndBySegment[segmentId]:
     Set to 1 to enable the cap end tileset for the given segment.
     The cap end applies to the last cell in fill order only if that
     cell's segment has capEndBySegment set to 1.


   GAIN TILESETS -- gainTileset*BySegment[segmentId]:
     Alternative tilesets used when the trail mode is GAIN (value increasing).
     One gain variant exists for each of the 8 tileset types above.
     If NULL, the normal (damage) tileset is used as fallback.

   BLINK-OFF TILESETS -- blinkOffTileset*BySegment[segmentId]:
     Alternative tilesets used during blink OFF frames (damage trail).
     One blink-off variant exists for each of the 8 tileset types above.
     If NULL, default blink behavior applies (trail hidden).

   GAIN BLINK-OFF TILESETS -- gainBlinkOffTileset*BySegment[segmentId]:
     Alternative tilesets used during blink OFF frames of gain trail.
     One variant exists for each of the 8 tileset types above.
     If NULL, default blink behavior applies.


   VISUAL PROPERTIES:
     orientation:      HORIZONTAL (fills left-right) or VERTICAL (fills top-bottom)
     palette:          Palette line (0-3) for tile rendering
     priority:         Tile priority (0=behind sprites, 1=in front)
     verticalFlip:     Flip tiles vertically (for mirrored P2 gauges)
     horizontalFlip:   Flip tiles horizontally (for mirrored P2 gauges)

   ============================================================================= */
typedef struct
{ 
    /* --- Geometry (u16 first for 68000 word alignment) --- */
    u16 fillOffset;                              /* Pixel offset for fill calc */
    u8 length;                                   /* Number of cells (1..16) */
    u8 segmentCount;                             /* Number of segment entries actually allocated */

    u8 *segmentIdByCell;                         /* [length] Segment style per cell */
    u8 *fillIndexByCell;                         /* [length] Fill order per cell */
    u8 *cellIndexByFillIndex;                    /* [length] Inverse LUT: cell index for a given fill index (O(1) lookup) */

    /* --- Tilemap positions --- */
    Vect2D_u16 *tilemapPosByCell;                /* [length] Tilemap X,Y coordinates per cell */
 
    /* --- Tilesets --- */
    const u32 **tilesetBySegment;                 /* [segmentCount] BODY: 45-tile strips */
    const u32 **tilesetEndBySegment;              /* [segmentCount] END: optional 45-tile strips */
    const u32 **tilesetTrailBySegment;            /* [segmentCount] TRAIL: optional 64-tile strips */
    const u32 **tilesetBridgeBySegment;           /* [segmentCount] BRIDGE: optional 45-tile strips */
    const u32 **tilesetCapStartBySegment;         /* [segmentCount] CAP START: optional 45-tile strips */
    const u32 **tilesetCapEndBySegment;           /* [segmentCount] CAP END: optional 45-tile strips */
    const u32 **tilesetCapStartBreakBySegment;    /* [segmentCount] CAP START BREAK: optional 45-tile strips */
    const u32 **tilesetCapStartTrailBySegment;    /* [segmentCount] CAP START TRAIL: optional 45-tile strips */
    u8 *capEndBySegment;                          /* [segmentCount] CAP END: 1 if enabled for segment */

    /* --- Gain trail tilesets (optional, per segment) ---
     * Used during gain trail (value increasing).
     * NULL entries fall back to the normal tilesets.
     */
    const u32 **gainTilesetBySegment;             /* [segmentCount] BODY: 45-tile strips */
    const u32 **gainTilesetEndBySegment;          /* [segmentCount] END: 45-tile strips */
    const u32 **gainTilesetTrailBySegment;        /* [segmentCount] TRAIL: 64-tile strips */
    const u32 **gainTilesetBridgeBySegment;       /* [segmentCount] BRIDGE: 45-tile strips */
    const u32 **gainTilesetCapStartBySegment;     /* [segmentCount] CAP START: 45-tile strips */
    const u32 **gainTilesetCapEndBySegment;       /* [segmentCount] CAP END: 45-tile strips */
    const u32 **gainTilesetCapStartBreakBySegment;/* [segmentCount] CAP START BREAK: 45-tile strips */
    const u32 **gainTilesetCapStartTrailBySegment;/* [segmentCount] CAP START TRAIL: 45-tile strips */

    /* --- Blink-off tilesets (optional, per segment) ---
     * Used only during trail blink OFF frames (blinkFramesRemaining > 0).
     * If a blink-off tileset is NULL, rendering falls back to normal behavior
     * for that visual element (i.e., trail hidden).
     */
    const u32 **blinkOffTilesetBySegment;         /* [segmentCount] BODY: 45-tile strips */
    const u32 **blinkOffTilesetEndBySegment;      /* [segmentCount] END: 45-tile strips */
    const u32 **blinkOffTilesetTrailBySegment;    /* [segmentCount] TRAIL: 64-tile strips */
    const u32 **blinkOffTilesetBridgeBySegment;   /* [segmentCount] BRIDGE: 45-tile strips */
    const u32 **blinkOffTilesetCapStartBySegment; /* [segmentCount] CAP START: 45-tile strips */
    const u32 **blinkOffTilesetCapEndBySegment;   /* [segmentCount] CAP END: 45-tile strips */
    const u32 **blinkOffTilesetCapStartBreakBySegment; /* [segmentCount] CAP START BREAK: 45-tile strips */
    const u32 **blinkOffTilesetCapStartTrailBySegment; /* [segmentCount] CAP START TRAIL: 45-tile strips */

    /* --- Gain blink-off tilesets (optional, per segment) ---
     * Used only during gain trail blink OFF frames.
     * If a gain blink-off tileset is NULL, rendering falls back to normal behavior
     * for that visual element (i.e., trail hidden).
     */
    const u32 **gainBlinkOffTilesetBySegment;     /* [segmentCount] BODY: 45-tile strips */
    const u32 **gainBlinkOffTilesetEndBySegment;  /* [segmentCount] END: 45-tile strips */
    const u32 **gainBlinkOffTilesetTrailBySegment;/* [segmentCount] TRAIL: 64-tile strips */
    const u32 **gainBlinkOffTilesetBridgeBySegment;/* [segmentCount] BRIDGE: 45-tile strips */
    const u32 **gainBlinkOffTilesetCapStartBySegment;/* [segmentCount] CAP START: 45-tile strips */
    const u32 **gainBlinkOffTilesetCapEndBySegment;  /* [segmentCount] CAP END: 45-tile strips */
    const u32 **gainBlinkOffTilesetCapStartBreakBySegment;/* [segmentCount] CAP START BREAK: 45-tile strips */
    const u32 **gainBlinkOffTilesetCapStartTrailBySegment;/* [segmentCount] CAP START TRAIL: 45-tile strips */

    /* --- PIP compact style (optional, per segment) ---
     * Compact strip layout (width = pipWidthBySegment[segId]):
     *   state 0: EMPTY     [0 .. width-1]
     *   state 1: VALUE     [width .. 2*width-1]
     *   state 2: LOSS      [2*width .. 3*width-1]
     *   state 3: GAIN      [3*width .. 4*width-1]
     *   state 4: BLINK_OFF [4*width .. 5*width-1]
     */
    const u32 **pipTilesetBySegment;              /* [segmentCount] compact strip: 5*width tiles */
    u8 *pipWidthBySegment;                        /* [segmentCount] width in tiles per segment style */

    /* --- PIP metadata (auto-built from fill order + pipWidthBySegment) --- */
    u8 pipCount;                                   /* number of logical pips in this layout */
    u8 *pipIndexByFillIndex;                       /* [length] fillIndex -> pip index */
    u8 *pipLocalTileByFillIndex;                   /* [length] fillIndex -> local tile inside pip */
    u8 *pipWidthByPipIndex;                        /* [length] pip index -> width in tiles */

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
    u8 *bridgeEndByFillIndex;                      /* [length] 1 if fillIndex is last cell of a segment with bridge */
    u8 *bridgeBreakByFillIndex;                    /* [length] 1 if fillIndex is forced BREAK before a bridge */
    u8 *bridgeBreakBoundaryByFillIndex;            /* [length] fillIndex of the bridge cell this BREAK aligns to */

    /* --- Visual properties (all u8 for compact packing) --- */
    u8 orientation;                              /* GAUGE_ORIENT_HORIZONTAL=0 or GAUGE_ORIENT_VERTICAL=1 */
    u8 palette;                              /* Palette line (0-3) */
    u8 priority;                                 /* Tile priority (0-1) */
    u8 verticalFlip;                                    /* Vertical flip */
    u8 horizontalFlip;                                    /* Horizontal flip */

    /* --- Cached flags (pre-computed at init, avoid runtime scans) --- */
    u8 hasBlinkOff;                              /* 1 if any segment has blink-off tilesets (damage) */
    u8 hasGainBlinkOff;                          /* 1 if any segment has gain blink-off tilesets */
    u8 capStartEnabled;                          /* 1 if cap start tileset is configured */
    u8 capEndEnabled;                            /* 1 if cap end tileset is configured */
    u16 refCount;                                 /* Reference count (retained by parts) */

} GaugeLayout;


/* -----------------------------------------------------------------------------
   GaugeLayout init API (simplified initialization)
   ----------------------------------------------------------------------------- */

/**
 * Set of tilesets for one visual context.
 *
 * A GaugeSkinSet groups the 8 tileset pointers needed for one rendering
 * context. There are 4 contexts per segment (see GaugeSegmentStyle below).
 *
 * Each field points to a pre-rendered strip of tiles in ROM.
 * Set a field to NULL to disable that feature or fall back to the parent
 * context's tileset (e.g., if gain.body is NULL, the base.body is used).
 *
 *   body:          Main interior strip (45 tiles). Required for at least
 *                  one context (base or gain).
 *   end:           Edge/termination strip (45 tiles). Gives the value/trail
 *                  edge a polished look (rounded tip, etc.).
 *   trail:         Trail-specific strip (64 tiles). For cells in the trail
 *                  break zone (where the trail edge falls).
 *   bridge:        Segment transition strip (45 tiles). For the last cell of
 *                  a segment, blending into the next segment.
 *   capStart:      First cell border strip (45 tiles). Decorative cap at
 *                  the gauge start.
 *   capEnd:        Last cell border strip (45 tiles). Decorative cap at
 *                  the gauge end.
 *   capStartBreak: Cap start variant for full/empty state (45 tiles).
 *   capStartTrail: Cap start variant when trail is visible (45 tiles).
 *
 * Example (minimal -- only body):
 *   GaugeSkinSet base = { .body = myBodyStrip.tiles };
 *
 * Example (body + end):
 *   GaugeSkinSet base = { .body = myBodyStrip.tiles, .end = myEndStrip.tiles };
 */
typedef struct
{
    const u32 *body;
    const u32 *end;
    const u32 *trail;
    const u32 *bridge;
    const u32 *capStart;
    const u32 *capEnd;
    const u32 *capStartBreak;
    const u32 *capStartTrail;
} GaugeSkinSet;

/**
 * Complete style for one segment across all 4 visual contexts.
 *
 * Each segment can look different depending on the current trail state.
 * The renderer picks the right tileset automatically:
 *
 *   Trail state              | Context used
 *   -------------------------|------------------
 *   No trail / damage trail  | base (normal)
 *   Gain trail               | gain (falls back to base if NULL)
 *   Damage blink OFF frame   | blinkOff (falls back to hiding trail)
 *   Gain blink OFF frame     | gainBlinkOff (falls back to hiding trail)
 *
 * capEndEnabled: set to 1 if this segment should render a cap end tile
 *   when the last cell in fill order belongs to this segment.
 *
 * Example (segment with body only, no special features):
 *   GaugeSegmentStyle style = {
 *       .base = { .body = myStrip.tiles }
 *   };
 *
 * Example (segment with body + end + trail + blink-off):
 *   GaugeSegmentStyle style = {
 *       .base     = { .body = bodyStrip.tiles, .end = endStrip.tiles,
 *                     .trail = trailStrip.tiles },
 *       .blinkOff = { .body = blinkStrip.tiles }
 *   };
 */
typedef struct
{
    GaugeSkinSet base;         /* Normal tilesets (required) */
    GaugeSkinSet gain;         /* Gain trail tilesets (optional, falls back to base) */
    GaugeSkinSet blinkOff;     /* Blink-off tilesets for damage (optional) */
    GaugeSkinSet gainBlinkOff; /* Blink-off tilesets for gain (optional) */
    u8 capEndEnabled;          /* 1 = enable cap end tile for this segment */
} GaugeSegmentStyle;

/**
 * Full layout initialization config (preferred API for new code).
 *
 * Bundles all layout parameters into a single struct passed to GaugeLayout_build().
 * This is the easiest way to set up a layout -- it configures all tilesets
 * (body, end, trail, bridge, caps, gain, blink-off) for all segments at once
 * via the segmentStyles array.
 *
 * Example:
 *   GaugeSegmentStyle styles[3] = {
 *       { .base = { .body = greenStrip.tiles, .end = greenEnd.tiles } },
 *       { .base = { .body = yellowStrip.tiles, .end = yellowEnd.tiles } },
 *       { .base = { .body = redStrip.tiles, .end = redEnd.tiles } },
 *   };
 *   u8 segments[9] = { 0,0,0, 1,1,1, 2,2,2 };
 *
 *   GaugeLayoutInit init = {
 *       .length = 9,
 *       .fillDirection = GAUGE_FILL_FORWARD,
 *       .orientation = GAUGE_ORIENT_HORIZONTAL,
 *       .palette = PAL0,
 *       .priority = 1,
 *       .segmentIdByCell = segments,
 *       .segmentStyles = styles
 *   };
 *   GaugeLayout_build(&myLayout, &init);
 */
typedef struct
{
    u8 length;                               /* Number of cells (1..GAUGE_MAX_LENGTH) */
    GaugeFillDirection fillDirection;         /* GAUGE_FILL_FORWARD or GAUGE_FILL_REVERSE */
    GaugeOrientation orientation;             /* GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL */
    u8 palette;                              /* Palette line (0-3) */
    u8 priority;                             /* Tile priority (0 or 1) */
    u8 verticalFlip;                         /* Vertical flip flag */
    u8 horizontalFlip;                       /* Horizontal flip flag */
    const u8 *segmentIdByCell;               /* Segment assignment per cell (NULL => all segment 0) */
    const GaugeSegmentStyle *segmentStyles;  /* Style per segment (NULL => no tilesets) */
} GaugeLayoutInit;


/* -----------------------------------------------------------------------------
   GaugeLayout API
   ----------------------------------------------------------------------------- */

/**
 * Build a layout from an initialization config.
 * This is a convenience wrapper around GaugeLayout_initEx + optional setters.
 */
void GaugeLayout_build(GaugeLayout *layout, const GaugeLayoutInit *init);

/**
 * Initialize layout with complete configuration.
 *
 * @param layout        Layout to initialize
 * @param length        Number of cells (1..GAUGE_MAX_LENGTH)
 * @param fillDirection       GAUGE_FILL_FORWARD or GAUGE_FILL_REVERSE
 * @param tilesets      Array of GAUGE_MAX_SEGMENTS tileset pointers (45-tile strips)
 *                      Can be NULL, tiles will be set from this array
 * @param segmentIdByCell Array of length elements specifying segment ID per cell
 *                      Can be NULL, all cells will use segment 0
 * @param orientation   GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL
 * @param palette   Palette line (0-3, typically PAL0-PAL3)
 * @param priority      Tile priority (0 or 1)
 * @param verticalFlip         Vertical flip (0 or 1)
 * @param horizontalFlip         Horizontal flip (0 or 1)
 */
void GaugeLayout_init(GaugeLayout *layout,
                      u8 length,
                      GaugeFillDirection fillDirection,
                      const u32 * const *tilesets,
                      const u8 *segmentIdByCell,
                      GaugeOrientation orientation,
                      u8 palette,
                      u8 priority,
                      u8 verticalFlip,
                      u8 horizontalFlip);

/**
 * Initialize layout with BODY + optional END tilesets.
 *
 * Fallback rules:
 * - If END tileset is NULL for a segment, BODY is used everywhere.
 * - BREAK cells (logical transition) always use BODY tileset.
 *
 * @param layout        Layout to initialize
 * @param length        Number of cells (1..GAUGE_MAX_LENGTH)
 * @param fillDirection       GAUGE_FILL_FORWARD or GAUGE_FILL_REVERSE
 * @param bodyTilesets  Array of GAUGE_MAX_SEGMENTS BODY tileset pointers (45-tile strips)
 * @param endTilesets   Array of GAUGE_MAX_SEGMENTS END tileset pointers (optional, can be NULL)
 * @param trailTilesets Array of GAUGE_MAX_SEGMENTS TRAIL tileset pointers (optional, can be NULL)
 * @param bridgeTilesets Array of GAUGE_MAX_SEGMENTS BRIDGE tileset pointers (optional, can be NULL)
 * @param segmentIdByCell Array of length elements specifying segment ID per cell
 * @param orientation   GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL
 * @param palette   Palette line (0-3, typically PAL0-PAL3)
 * @param priority      Tile priority (0 or 1)
 * @param verticalFlip         Vertical flip (0 or 1)
 * @param horizontalFlip         Horizontal flip (0 or 1)
 */
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
                        u8 horizontalFlip);

/**
 * Set pixel fill offset for a layout.
 *
 * Shifts where this part's cells start sampling the gauge value.
 * See the fillOffset field reference in GaugeLayout for a detailed
 * visual explanation with examples.
 *
 * Quick formula for a shorter bottom row (like Sample 7):
 *   fillOffset = triggerPixel - (partLength * GAUGE_PIXELS_PER_TILE)
 *
 * Example: Part2 is 3 cells, should become fully filled at pixel 32:
 *   fillOffset = 32 - (3 * 8) = 8
 *
 * @param layout            Layout to configure
 * @param fillOffsetPixels  Pixel offset (0 = starts at gauge pixel 0)
 */
void GaugeLayout_setFillOffset(GaugeLayout *layout, u16 fillOffsetPixels);

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
 * Configure optional gain trail tilesets (used during gain trail when value increases).
 * Pass NULL for any array to fall back to normal tilesets for that element.
 *
 * @param layout                   Layout to configure (must be initialized)
 * @param gainBodyTilesets         BODY gain strips (45 tiles)
 * @param gainEndTilesets          END gain strips (45 tiles)
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
                                 const u32 * const *gainBlinkOffTrailTilesets,
                                 const u32 * const *gainBlinkOffBridgeTilesets,
                                 const u32 * const *gainBlinkOffCapStartTilesets,
                                 const u32 * const *gainBlinkOffCapEndTilesets,
                                 const u32 * const *gainBlinkOffCapStartBreakTilesets,
                                 const u32 * const *gainBlinkOffCapStartTrailTilesets);

/**
 * Configure compact PIP styles (used by GAUGE_VALUE_MODE_PIP renderer).
 * Call after GaugeLayout_init/initEx.
 *
 * @param layout            Layout to configure
 * @param pipTilesets       Array of GAUGE_MAX_SEGMENTS compact strips
 * @param pipWidthBySegment Array of GAUGE_MAX_SEGMENTS widths in tiles (0 => default 1)
 */
void GaugeLayout_setPipStyles(GaugeLayout *layout,
                              const u32 * const *pipTilesets,
                              const u8 *pipWidthBySegment);

/* =============================================================================
   GaugeLogic -- Value/trail state machine
   =============================================================================

   GaugeLogic is the "brain" of a gauge. It manages the current value, the
   trail position, blink timers, and animation. It is embedded inside the
   Gauge struct and ticked automatically by Gauge_update() every frame.

   All timing is frame-based (60fps on NTSC, 50fps on PAL).

   HOW VALUES WORK:
   ----------------
   The gauge has two representations of its value:
   - currentValue: the logical value (0..maxValue), e.g., 75 HP out of 100.
   - valuePixels:  the rendered pixel position (0..maxFillPixels), e.g., 48 px.

   maxFillPixels is auto-derived from the layout (layout->length * 8).
   When maxValue != maxFillPixels, a lookup table (valueToPixelsData)
   maps between them. When they're equal, it's a direct 1:1 mapping.
   The LUT buffer is allocated dynamically (maxValue+1 entries).

   VALUE BEHAVIOR ON DAMAGE (Gauge_decrease):
   -------------------------------------------
     1. currentValue decreases by the requested amount
     2. valueTargetPixels = value_to_pixels(currentValue)
     3. If valueAnimEnabled: valuePixels animates toward valueTargetPixels
        If !valueAnimEnabled: valuePixels jumps instantly
     4. Trail holds at the previous valuePixels position for holdFrames
     5. Trail blinks on/off for blinkFrames
     6. Trail shrinks toward the new valuePixels
     7. When trail reaches value, trailMode resets to NONE

   VALUE BEHAVIOR ON HEAL (Gauge_increase):
   -----------------------------------------
     1. currentValue increases by the requested amount
     2. valueTargetPixels = value_to_pixels(currentValue)
     3. If valueAnimEnabled AND trailEnabled:
        - Trail jumps immediately to new target (gain trail leads ahead)
        - Trail holds for holdFrames, blinks for blinkFrames
        - Value then animates up to catch the trail
        - When value reaches trail, trailMode resets to NONE
     4. If !valueAnimEnabled OR !trailEnabled:
        - Value changes instantly, trail follows, no blink/hold

   ANIMATION PARAMETERS:
   ---------------------
   valueAnimEnabled:
     0 = value changes are instant (no animation)
     1 = value animates smoothly toward target

   valueAnimShift:
     Controls how fast the value moves toward the target.
     Each frame: step = (distance >> valueAnimShift) + 1
     Higher shift = slower animation. The +1 ensures it always makes progress.
     Recommended: 3-6, default: 4

   trailAnimShift:
     Controls how fast the trail shrinks toward the value.
     Same formula as valueAnimShift.
     Recommended: 3-6, default: 4

   blinkShift:
     Controls how fast the trail blinks on/off.
     The blink timer increments each frame; toggling happens every
     2^blinkShift frames. Higher = slower blinking.
     Recommended: 2-4, default: 3 (~7.5 Hz @ 60fps)

   needUpdate:
     Optimization flag. Set to 1 when any animation is in progress.
     When 0, Gauge_update() returns immediately (saves ~80 cycles/frame).
     Automatically managed by the logic -- users don't need to touch this.

   ============================================================================= */
typedef struct
{
    /* --- 16-bit / pointer fields first (word-aligned on 68000) --- */

    u16 maxValue;                   /* Maximum logical value (e.g., 100 for "100 HP") */
    u16 currentValue;               /* Current logical value (0..maxValue) */
    u16 maxFillPixels;              /* Total gauge pixel width (e.g., 80 = 10 tiles) */
    const u16 *valueToPixelsLUT;    /* Points to valueToPixelsData when active, NULL for 1:1 */
    u16 *valueToPixelsData;         /* Dynamically allocated LUT (maxValue+1 entries) */

    u16 valueTargetPixels;          /* Where the value is heading (animation target) */
    u16 valuePixels;                /* Where the value is currently displayed (animated) */
    u16 trailPixels;                /* Where the trail is currently displayed */
    u16 blinkTimer;                 /* Increments each frame during blink phase */

    /* Render cache (for change detection -- skip rendering if nothing changed) */
    u16 lastValuePixels;            /* Value pixels from last rendered frame */
    u16 lastTrailPixelsRendered;    /* Trail pixels from last rendered frame */

    /* --- 8-bit fields (packed, no padding) --- */

    u8 holdFramesRemaining;         /* Frames the trail stays still before blinking */
    u8 blinkFramesRemaining;        /* Frames of blinking left after hold */
    u8 valueAnimEnabled;            /* 0=value jumps instantly, 1=value slides smoothly */
    u8 valueAnimShift;              /* Value slide speed: step = distance >> shift + 1 */
    u8 trailAnimShift;              /* Trail shrink speed: step = distance >> shift + 1 */
    u8 blinkShift;                  /* Blink rate: toggles every 2^shift frames */
    u8 trailEnabled;                /* 0=no trail effect, 1=trail with hold/blink/shrink */
    u8 trailMode;                   /* Current state: NONE / DAMAGE / GAIN */
    u8 lastBlinkOn;                 /* Blink ON/OFF state from last frame (for change detection) */
    u8 lastTrailMode;               /* Trail mode from last frame (for change detection) */
    u8 needUpdate;                  /* 1=needs rendering, 0=fully idle (Gauge_update returns early) */

} GaugeLogic;


/* =============================================================================
   Internal structures (used by GaugePart, exposed for sizeof)
   ============================================================================= */

/**
 * Per-cell streaming data for FIXED VRAM mode.
 *
 * In fixed mode, each cell has its own dedicated VRAM tile. When the gauge
 * value changes and a cell's fill is different, 32 bytes of new tile data
 * are DMA'd from the ROM strip to that cell's VRAM slot.
 *
 * The strip pointers are cached at init time (from the layout's segment
 * tilesets) to avoid looking up the segment ID and tileset every frame:
 *
 *   bodyFillStrip45:   BODY strip (45 tiles) - the main fill tileset
 *   endFillStrip45:    END strip (45 tiles) - for the value/trail edge cell
 *   trailFillStrip64:  TRAIL strip (64 tiles) - for trail-specific cells
 *   bridgeFillStrip45: BRIDGE strip (45 tiles) - for segment transitions
 *
 * Change detection:
 *   cachedStrip + cachedFillIndex track the last uploaded tile.
 *   If both match the desired tile, the DMA is skipped (saves ~200 cycles).
 */
typedef struct
{
    const u32 *bodyFillStrip45;     /* BODY strip (always set when cell is valid) */
    const u32 *endFillStrip45;      /* END strip (NULL if not supported) */
    const u32 *trailFillStrip64;    /* TRAIL strip (NULL if not supported) */
    const u32 *bridgeFillStrip45;   /* BRIDGE strip (NULL if not supported) */
    const u32 *cachedStrip;   /* Last strip uploaded (for cache) */
    u16 vramTileIndex;              /* VRAM tile index for this cell */
    u8 cachedFillIndex;               /* Last fill index uploaded (0xFF=none) */
    u8 cellIndex;                   /* Index in layout (for fill calculation) */
} GaugeStreamCell;


/**
 * Dynamic VRAM mode data.
 *
 * In dynamic mode, VRAM tiles are shared across cells to save VRAM space.
 * Instead of each cell having its own tile, cells point (via tilemap) to
 * a small pool of shared tiles:
 *
 * STANDARD TILES (per segment, never change after init):
 *   vramTileEmpty[seg]     -> empty tile (value=0, trail=0)
 *   vramTileFullValue[seg] -> full value tile (value=8, trail=8)
 *   vramTileFullTrail[seg] -> full trail tile (value=0, trail=8)
 *
 *   ~70-80% of cells use one of these 3 tiles. The tilemap just points
 *   them at the right one -- no DMA needed each frame.
 *
 * PARTIAL TILES (shared across all cells, streamed on demand):
 *   vramTilePartialValue   -> the cell where the value edge falls
 *   vramTilePartialTrail   -> the cell where the trail edge falls
 *   vramTilePartialEnd     -> the END cell (value/trail termination)
 *   vramTilePartialTrailSecond -> 2nd trail break (rare)
 *
 *   Only 1-2 cells at a time need partial tiles. When the value changes,
 *   new tile data is DMA'd to the partial VRAM slot, and the tilemap is
 *   updated to point the affected cell at it.
 *
 * BRIDGE/CAP TILES (dedicated per segment or per part):
 *   vramTileBridge[seg]    -> bridge transition tile per segment
 *   vramTileCapStart       -> cap start tile
 *   vramTileCapEnd         -> cap end tile
 *
 * Change detection caches (cachedFillIndex*, loadedSegment*) track what's
 * currently in each VRAM slot to avoid redundant DMA transfers.
 */
typedef struct
{
    /* --- Standard tiles per segment --- */
    u16 *vramTileEmpty;                           /* [segmentCount] Empty tile per segment (0,0) */
    u16 *vramTileFullValue;                       /* [segmentCount] Full value tile per segment (8,8) */
    u16 *vramTileFullTrail;                       /* [segmentCount] Full trail tile per segment (0,8) */
    u16 *vramTileBridge;                          /* [segmentCount] Bridge tile per segment (dynamic) */
    u16 vramTileCapStart;                        /* Cap start tile (per part) */
    u16 vramTileCapEnd;                          /* Cap end tile (per part) */

    /* --- Partial tiles (streamed on demand, scalars per GaugePart) --- */
    u16 vramTilePartialValue;       /* Partial value tile - also used for "both" case */
    u16 vramTilePartialTrail;       /* Partial trail tile (value=0, trail=1-7 px) */
    u16 vramTilePartialEnd;         /* End cap tile (value/trail frontier) */
    u16 vramTilePartialTrailSecond; /* Second trail-break tile (before END) */

    /* --- Cache for loaded partial tiles --- */
    u8 loadedSegmentPartialValue;   /* Which segment's partial value is loaded */
    u8 cachedFillIndexPartialValue;   /* Loaded fill index for partial value */
    u8 loadedSegmentPartialTrail;   /* Which segment's partial trail is loaded */
    u8 cachedFillIndexPartialTrail;   /* Loaded fill index for partial trail */
    u8 loadedSegmentPartialEnd;     /* Which segment's END tile is loaded */
    u8 cachedFillIndexPartialEnd;     /* Loaded fill index for END */
    u8 loadedSegmentPartialTrailSecond; /* Which segment's second trail break is loaded */
    u8 cachedFillIndexPartialTrailSecond; /* Loaded fill index for second trail break */
    u8 *cachedFillIndexBridge;                   /* [segmentCount] Loaded fill index for bridge tile */
    u8 cachedFillIndexCapStart;           /* Loaded fill index for cap start tile */
    u8 cachedFillIndexCapEnd;             /* Loaded fill index for cap end tile */
    u8 loadedCapStartUsesBreak;         /* 1 if cap start break strip is loaded */
    u8 loadedCapStartUsesTrail;         /* 1 if cap start trail strip is loaded */

    /* --- Tilemap cache (for change detection) --- */
    u16 *cellCurrentTileIndex;                   /* [cellCount] Currently displayed VRAM tile per cell */

    /* --- Cell validity (pre-computed for CPU optimization) ---
     * Avoids checking tilesetBySegment[segId] != NULL in render loop.
     * Trade-off: 16 bytes SRAM vs 2 memory accesses + 1 branch per cell per frame.
     */
    u8 *cellValid;                               /* [cellCount] 1 if cell has valid tileset, 0 otherwise */
    u8 segmentCount;                             /* Allocated segment array length */
    u8 cellCount;                                /* Allocated cell array length */

} GaugeDynamic;


/* =============================================================================
   GaugePart -- VRAM streaming engine
   =============================================================================

   A GaugePart is one visual instance of a gauge on screen. It takes care of
   the actual rendering: streaming tile data from ROM to VRAM, and updating
   the tilemap so the correct tiles appear at the correct screen position.

   Users don't interact with GaugePart directly -- it's managed internally
   by the Gauge container and allocated by Gauge_addPart().

   WHAT'S INSIDE A PART:
   - A retained GaugeLayout reference (shared safely via refcount)
   - Screen position (originX, originY in tilemap coordinates)
   - VRAM allocation (vramBase + reserved tiles)
   - Per-cell streaming data (for fixed mode) or shared VRAM data (for dynamic)
   - A render handler function pointer (selected at init, never changes)

   HOW RENDERING WORKS:
   Each frame, Gauge_update() calls the part's renderHandler with the current
   value/trail/blink state from the shared GaugeLogic. The handler then:
   1. Computes which cells need to change
   2. Selects the correct tile from the ROM strip
   3. Streams the tile to VRAM (DMA in fixed mode, or tilemap update in dynamic)

   FIXED MODE (cells[] array):
     Each cell has its own dedicated VRAM tile. When a cell's fill changes,
     32 bytes of tile data are DMA'd from the ROM strip to that cell's VRAM slot.
     The cells[] array caches the last uploaded strip+index per cell to avoid
     redundant DMA transfers.

   DYNAMIC MODE (dyn struct):
     All cells share a small pool of VRAM tiles. "Standard" tiles (empty, full,
     full-trail) are pre-loaded once. "Partial" tiles (the 1-2 cells where the
     value/trail edge falls) are streamed on demand. The tilemap is updated to
     point each cell at the correct shared tile.

   ============================================================================= */
typedef struct GaugePart
{
    /* --- Position --- */
    u16 originX;                    /* Tilemap X origin */
    u16 originY;                    /* Tilemap Y origin */

    /* --- VRAM --- */
    GaugeVramMode vramMode;         /* Fixed or dynamic mode */
    u16 vramBase;                   /* Base VRAM tile index */
    GaugePartRenderHandler *renderHandler; /* Resolved renderer for this part */

    /* --- Shared layout reference (retained) --- */
    const GaugeLayout *layout;

    /* --- Fixed mode data --- */
    GaugeStreamCell *cells;                     /* [layout->length] Per-cell streaming data */
    u8 cellCount;                               /* Active cell count */

    /* --- Dynamic mode data --- */
    GaugeDynamic dyn;

} GaugePart;


/* =============================================================================
   Gauge -- High-level gauge container (THE MAIN OBJECT)
   =============================================================================

   This is the struct you create and interact with in your game code.
   It binds together one GaugeLogic (the value state machine) and one or
   more GaugeParts (the rendering units).

   LIFECYCLE:
   ----------
   1. Allocate:
        static Gauge myGauge;

   2. Initialize:
        Gauge_init(&myGauge, &config);  // sets up logic, no rendering yet

   3. Configure animations (optional):
        Gauge_setValueAnim(&myGauge, 1, 4);   // enable smooth value changes
        Gauge_setTrailAnim(&myGauge, 1, 4, 3); // enable trail effect

   4. Add parts (each call allocates part + VRAM and sets up rendering):
        Gauge_addPart(&myGauge, &layout, x, y);
        Gauge_addPart(&myGauge, &layout, x, y+1); // 2-lane

   5. Game loop:
        Gauge_update(&myGauge);  // call once per frame (tick + render)

   6. Change value:
        Gauge_decrease(&myGauge, 10, 20, 60); // damage: -10, hold 20f, blink 60f
        Gauge_increase(&myGauge, 5, 20, 60);  // heal: +5, hold 20f, blink 60f
        Gauge_setValue(&myGauge, 50);          // instant set (no trail)

   WHY MULTIPLE PARTS?
   -------------------
   A single Gauge can drive multiple GaugeParts. All parts share the same
   GaugeLogic, so they always show the same value in perfect sync.

   Common use cases:
   - 2-lane gauge: 2 parts at Y and Y+1 for a 2-tile-tall bar
   - 2-lane with shorter bottom row: Part 1 uses fillOffset to sample
     a subset of the value (see fillOffset field reference in GaugeLayout)

   NOTE: For two independent gauges (e.g., P1 and P2 health bars), use
   two separate Gauge objects. A mirrored P2 layout is created with
   GaugeLayout_makeMirror() on the P2 Gauge's own layout.

   VRAM ALLOCATION:
   ----------------
   VRAM is auto-allocated sequentially. Each Gauge_addPart() call reserves
   the VRAM tiles needed for that part, starting from vramBase.
   The gauge tracks vramNextOffset internally -- you don't need to manage it.

   To know how many tiles a part will need (for manual planning):
     u16 size = Gauge_getVramSize(&gauge, &layout);

   ============================================================================= */
struct Gauge
{
    /* --- Logic (value state machine) --- */
    GaugeLogic logic;

    /* --- Parts (rendering units) --- */
    GaugePart **parts;          /* Dynamically allocated array of part pointers */
    GaugeTickAndRenderHandler *tickAndRenderHandler; /* Resolved update path */
    u8 partCount;               /* Number of active parts */
    u8 partCapacity;            /* Allocated part pointer capacity */

    /* --- VRAM allocation --- */
    u16 vramBase;                   /* Base VRAM tile index */
    u16 vramNextOffset;             /* Offset for next part allocation */
    GaugeVramMode vramMode;         /* Default VRAM mode for parts */
    GaugeValueMode valueMode;       /* Value quantization mode */
};


/* -----------------------------------------------------------------------------
   Gauge API
   ----------------------------------------------------------------------------- */

/**
 * Gauge initialization config.
 * Pass this to Gauge_init() to configure runtime mode and VRAM defaults.
 *
 * KEY FIELDS:
 *   maxValue:      The maximum logical value (e.g., 100 for "100 HP").
 *                  Values above GAUGE_LUT_CAPACITY are clamped at init.
 *   layout:        Main layout (must be fully configured before Gauge_init).
 *                  maxFillPixels is derived as layout->length * GAUGE_PIXELS_PER_TILE.
 *                  If maxValue != maxFillPixels, a LUT is auto-generated at init
 *                  to map between them (e.g., 100 HP -> 128 pixels).
 *                  Override with Gauge_setMaxFillPixels() after init if needed.
 *   initialValue:  Starting value (e.g., maxValue for "full health").
 *   vramBase:      First VRAM tile index reserved for this gauge.
 *                  Parts will be allocated sequentially from this base.
 *   vramMode:      Default VRAM mode for all parts added to this gauge.
 *                  Can be overridden per-part with Gauge_addPartEx().
 *   valueMode:     FILL (continuous) or PIP (discrete).
 *                  In PIP mode, maxValue is expected to equal layout pipCount;
 *                  Gauge_init adjusts and logs if mismatch.
 */
typedef struct
{
    u16 maxValue;                /* Maximum logical value (clamped to GAUGE_LUT_CAPACITY at init) */
    u16 initialValue;            /* Starting value (clamped to maxValue) */
    const GaugeLayout *layout;   /* Main layout (maxFillPixels = length * 8) */
    u16 vramBase;                /* First VRAM tile index for this gauge */
    GaugeVramMode vramMode;      /* GAUGE_VRAM_FIXED or GAUGE_VRAM_DYNAMIC */
    GaugeValueMode valueMode;    /* GAUGE_VALUE_MODE_FILL or GAUGE_VALUE_MODE_PIP */
} GaugeInit;

/**
 * Retain a layout reference (increments refcount).
 */
void GaugeLayout_retain(GaugeLayout *layout);

/**
 * Release a layout reference (decrements refcount and frees dynamic buffers at 0).
 */
void GaugeLayout_release(GaugeLayout *layout);

/**
 * Initialize gauge from GaugeInit configuration.
 * Trail and value animation are disabled by default.
 * Use Gauge_setTrailAnim() and Gauge_setValueAnim() to configure animations.
 * Call Gauge_release() before re-initializing an already-used Gauge.
 */
void Gauge_init(Gauge *gauge, const GaugeInit *init);

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
 * Override the maximum fill pixel count.
 * By default, maxFillPixels = layout->length * 8 (set at Gauge_init).
 * No-op if the new value equals the current maxFillPixels.
 * Can be called at any time (no ordering constraint with Gauge_addPart).
 *
 * If maxValue != new maxFillPixels, the embedded LUT is repopulated.
 * If maxValue == new maxFillPixels, the LUT is cleared (1:1 mapping).
 *
 * @param gauge         Gauge to modify
 * @param maxFillPixels New total pixel span
 */
void Gauge_setMaxFillPixels(Gauge *gauge, u16 maxFillPixels);

/**
 * Add a part to the gauge (simplified).
 * VRAM is allocated automatically from gauge's vramBase.
 *
 * @param gauge     Parent gauge
 * @param layout    Layout configuration (retained via refCount)
 * @param originX   Tilemap X position
 * @param originY   Tilemap Y position
 */
u8 Gauge_addPart(Gauge *gauge,
                 GaugeLayout *layout,
                 u16 originX,
                 u16 originY);

/**
 * Add a part with custom VRAM configuration.
 * Use when you need specific VRAM placement or different VRAM mode.
 *
 * @param gauge     Parent gauge
 * @param layout    Layout configuration (retained via refCount)
 * @param originX   Tilemap X position
 * @param originY   Tilemap Y position
 * @param vramBase  Custom VRAM base tile index
 * @param vramMode  Custom VRAM mode (overrides gauge default)
 */
u8 Gauge_addPartEx(Gauge *gauge,
                   GaugeLayout *layout,
                   u16 originX,
                   u16 originY,
                   u16 vramBase,
                   GaugeVramMode vramMode);

/**
 * Release all allocations owned by the gauge (parts, buffers, retained layouts, LUTs).
 * Safe to call multiple times.
 */
void Gauge_release(Gauge *gauge);

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
 * - gauge->valueMode
 *
 * @param gauge       Gauge configuration source (must be initialized)
 * @param layout      Layout configuration
 * @return Number of VRAM tiles needed
 */
u16 Gauge_getVramSize(const Gauge *gauge,
                      const GaugeLayout *layout);


#endif /* GAUGE_H */
