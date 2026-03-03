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

   PUBLIC API NOTE (V2):
   ---------------------
   Gauge initialization is done through the GaugeBuilder UX API:
     GaugeBuilder_init -> GaugeBuilder_addSegment -> (optional) GaugeBuilder_addSlave
     -> GaugeBuilder_build

   GaugeLayout/GaugePart are internal architecture details kept documented below
   to explain rendering behavior, but they are not part of the public init API.


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

   CAP strips are internal renderer concepts.
   In the public Builder V2 API, caps are auto-derived from segment assets
   (no dedicated cap strips are declared explicitly).


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
     blinkShift=3 -> toggles every  8 frames (default, 7.5 toggles/sec @ 60fps)
     blinkShift=4 -> toggles every 16 frames (slow)
   Note: a full ON+OFF cycle is twice the toggle period (e.g., 3.75 cycles/sec at shift=3).


   CAPS (CAP START / CAP END)
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   Caps are decorative border tiles at the ends of the gauge. They always
   stay in place but change their tileset depending on the gauge state.

   CAP END: the last cell in fill order.
     In Builder V2, it maps to the end segment:
     - uses segment.end when available, else segment.body.

   CAP START: the first cell in fill order. Has 3 visual variants:
     - capStart:      used when the value edge is in this cell (normal fill)
     - capStartBreak: used when the cell is fully filled or empty
     - capStartTrail: used when the trail zone covers this cell

     In Builder V2, these variants are auto-mapped from the start segment:
     - capStart      <- segment.end (fallback segment.body)
     - capStartBreak <- segment.body
     - capStartTrail <- segment.trail

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

   Builder V2:
   - bridge can be provided per visual state (normal / gain / blink-off).

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

   Runtime note:
   - Bridge/BREAK forcing is suppressed only inside the active zone.
   - The active zone starts at the current value edge and extends to trail/end.
   - Outside this zone, bridge visibility follows the runtime bridge gate.


   ============================================================================
   ARCHITECTURE -- THE 4 MAIN STRUCTURES
   ============================================================================

   GaugeLayout -- "What does the gauge look like?"
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Visual configuration that describes:
   - How many cells (tiles) the gauge has
   - Which segment each cell belongs to (for tileset selection)
   - All tileset pointers (body, end, trail, bridge, caps, blink-off, gain)
   - Visual properties (orientation, palette, flip flags, priority)
   - Fill direction (forward or reverse)
   - Optional feature buffers (allocated lazily when configured)

   Layouts are created internally by GaugeBuilder_build() (one layout per part).
   Each part retains its layout (refcounted), and Gauge_release() releases all
   retained references and builder-owned layout allocations.
   Direct layout rebuild/mutation is an internal concern of gauge.c.


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
   For a mirrored P2 gauge, use a separate Gauge configured with its own
   GaugeDescription (fill direction / flips / origin as needed).

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
   - VRAM allocation state (auto-increments as parts are built)

   This is the only struct most users need to interact with.
   Build it once via GaugeBuilder_build(), then call Gauge_update() every frame.


   ============================================================================
   VALUE MODES
   ============================================================================

   FILL MODE (default):
   Classic continuous gauge. Each tile fills pixel by pixel (0..8 px).
   Total fill resolution = (sum of segment cellCount) * 8 pixels.

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
   static GaugeBuilder builder;

   GaugeBuilder_init(&builder, &(GaugeDescription){
       .mode = GAUGE_VALUE_MODE_FILL,
       .orientation = GAUGE_ORIENT_HORIZONTAL,
       .fillDirection = GAUGE_FILL_FORWARD,
       .originX = 2,
       .originY = 5,
       .maxValue = 100,
       .capStartFixed = 1,
       .capEndFixed = 1,
       .palette = PAL0,
       .priority = 1
   });

   GaugeBuilder_addSegment(&builder, 0, &(GaugeSegmentDefinition){
       .cellCount = 8,
       .normal = { .body = stripBody, .trail = stripTrail, .end = stripEnd }
   });

   GaugeBuilder_build(&builder, &myGauge, VRAM_BASE, GAUGE_VRAM_DYNAMIC);

   while (1) {
       Gauge_update(&myGauge);
       SYS_doVBlankProcess();
   }
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

/* Compile-time trace switch:
 * 0 = trace code compiled out
 * 1 = trace code compiled in (runtime still requires Gauge_setDebugMode(..., 1)) */
#define GAUGE_ENABLE_TRACE 1

/* Maximum logical value (maxValue) supported by the LUT buffer.
 * Each Gauge allocates a value-to-pixels lookup table of
 * (GAUGE_LUT_CAPACITY + 1) entries when needed.
 *
 * Any GaugeDescription.maxValue must be <= GAUGE_LUT_CAPACITY.
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
 * PIP:  Discrete gauge. Each "pip" is 1 or more tiles that switch states
 *       without partial fill. Guaranteed states are EMPTY and VALUE.
 *       Optional states (LOSS, GAIN, BLINK_OFF) are available only if
 *       provided by the compact tileset.
 *       Example: [VALUE] [VALUE] [VALUE] [LOSS ] [EMPTY]  (3/5 pips filled)
 *
 *       PIP setup requires:
 *       - Provide pipTileset + pipWidth in each GaugeSegmentDefinition
 *       - stateCount is derived automatically from pipTileset size and
 *         stripCoverage (FULL / HALF / QUARTER)
 *       - Pip count is computed from segment geometry
 *       - maxValue is auto-adjusted to pip count during GaugeBuilder_build()
 */
typedef enum
{
    GAUGE_VALUE_MODE_FILL = 0,
    GAUGE_VALUE_MODE_PIP  = 1
} GaugeValueMode;

/**
 * Coverage mode for compact strips.
 *
 * FULL:
 *   Strip stores the full tile surface.
 * HALF:
 *   Strip stores half surface. Missing half is reconstructed with flip flags.
 * QUARTER:
 *   Strip stores quarter surface. Remaining quadrants are reconstructed with
 *   horizontal/vertical flips.
 *
 * In this version, stripCoverage is active in PIP mode only.
 */
typedef enum
{
    GAUGE_STRIP_COVERAGE_FULL    = 0,
    GAUGE_STRIP_COVERAGE_HALF    = 1,
    GAUGE_STRIP_COVERAGE_QUARTER = 2
} GaugeStripCoverage;

/**
 * Gauge trail behavior mode.
 *
 * DISABLED:
 *   No trail.
 *
 * FOLLOW:
 *   Classic damage behavior (hold/blink/shrink on decrease).
 *
 * STATIC_TRAIL:
 *   Persistent trail region. No blink, no shrink.
 *
 * STATIC_TRAIL_CRITICAL_BLINK:
 *   Persistent trail region like STATIC_TRAIL.
 *   Trail blinks continuously only when currentValue <= criticalValue.
 *
 * CRITICAL_TRAIL_BLINK:
 *   No trail above criticalValue. When currentValue <= criticalValue,
 *   trail spans max->value and blinks continuously.
 *
 * CRITICAL_VALUE_BLINK:
 *   No trail. When currentValue <= criticalValue, remaining value blinks.
 */
typedef enum
{
    GAUGE_TRAIL_MODE_DISABLED             = 0,
    GAUGE_TRAIL_MODE_FOLLOW               = 1,
    GAUGE_TRAIL_MODE_STATIC_TRAIL         = 2,
    GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK = 3,
    GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK = 4,
    GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK = 5
} GaugeTrailMode;

/**
 * Cap end enable flag for capEndBySegment arrays.
 */
typedef enum
{
    GAUGE_CAP_INACTIVE = 0,
    GAUGE_CAP_ACTIVE   = 1
} GaugeCapEndState;

/**
 * Gain behavior mode (used by Gauge_increase).
 *
 * DISABLED:
 *   No gain trail. Value changes according to current value animation config.
 *
 * FOLLOW:
 *   Gain trail leads on increase (hold/blink), then value catches up.
 *
 * RESERVED_*:
 *   Placeholders for future gain styles.
 */
typedef enum
{
    GAUGE_GAIN_MODE_DISABLED   = 0,
    GAUGE_GAIN_MODE_FOLLOW     = 1,
    GAUGE_GAIN_MODE_RESERVED_1 = 2,
    GAUGE_GAIN_MODE_RESERVED_2 = 3
} GaugeGainMode;

/* =============================================================================
   Forward declarations (needed for circular references)
   ============================================================================= */
typedef struct GaugeLogic GaugeLogic;
typedef struct Gauge Gauge;
typedef struct GaugePart GaugePart;

/**
 * Per-gauge update handler (tick logic + render all parts).
 * Selected once at init based on GaugeValueMode.
 */
typedef void GaugeTickAndRenderHandler(Gauge *gauge);

/**
 * Per-gauge logic tick handler (value + trail state machine).
 * Selected at configuration time from trail mode and kept stable at runtime.
 */
typedef void GaugeLogicTickHandler(GaugeLogic *logic);

/**
 * Per-part render handler.
 * Selected once when the part is created, based on value mode and VRAM mode.
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

   A GaugeLayout describes what a gauge looks like.
   Geometry is initialized once, and optional feature sets can be updated
   through dedicated setters.

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
     Auto-computed during build from fillDirection.

   cellIndexByFillIndex[fillIndex]:
     Inverse of fillIndexByCell. Given a fill position, returns the
     corresponding cellIndex. Used for O(1) lookups instead of scanning.

   fillOffset:
     Pixel offset used to project this layout onto the master PART span
     (default 0). In a multi-part gauge, decision type/class/index is computed
     once from the master PART, then each slave PART maps its local cells
     to a master fill index using:

       cellStartPixel = fillOffset + fillIndex * 8
       samplePixel    = cellStartPixel + 4

     This keeps all PARTs visually synchronized on the same decision timeline
     while still allowing shorter/shifted slave layouts.

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
          Part2 cell 0 -> pixels  8..15 (same range as Part1 cell 1)
          Part2 cell 1 -> pixels 16..23 (same range as Part1 cell 2)
          Part2 cell 2 -> pixels 24..31 (same range as Part1 cell 3)

       When value = 96 (full):
         Part1: [FULL |FULL |FULL |FULL |FULL |...|FULL ]
          Part2: [FULL |FULL |FULL ]    (all cells > 31px -> full)

       When value = 20:
         Part1: [FULL |FULL |##..|....|....|...|....]
         Part2: [FULL |##..|....]
                   ^      ^
                   |      Part2 cell1: clamp(20 - 16) = 4px
                   Part2 cell0: clamp(20 - 8) = 8px -> full

       When value = 5:
         Part1: [#####|....|....|....|....|...|....]
          Part2: [....|....|....]   (all cells < 8px -> empty)

     The formula to calculate fillOffset for a bottom row:
       fillOffset = triggerPixel - (partLength * 8)
     where triggerPixel is the gauge pixel value at which the last
     cell of the bottom row should become completely full.

     In Builder V2, this is provided per slave via GaugeSlaveDescription.fillOffset,
     so each slave can define its own projection window.

   localEndOverrideEnabled:
     Controls local terminal END override in multi-part projection.
     Default is enabled (1). Applied on slave PARTs only.
     When enabled, terminal local cell override rules are:
     - "Bridge context" is real master BRIDGE only. PARTIAL_TRAIL2 alone does
       not count as bridge.
     - A persistent bridge flag is maintained per slave PART:
         - set when master terminal is BRIDGE on visible frames
           (`blinkOffActive == 0`)
         - kept unchanged during blink-off frames
         - cleared when master terminal is not BRIDGE on visible frames
     - Bridge TERM_END applies while that flag is active and value has reached
       this slave layout start:
         valuePixels >= fillOffset
     - Outside bridge, TERM_END can still apply with the existing rule:
       valuePixels > fillOffset + length*8 and mapped master terminal type is
       STANDARD_FULL.
     Forced index policy:
     - blink OFF frame: idx 8
     - blink ON frame:  idx 44
     - no blink active: idx 44
     This is an internal rendering policy configured during build.


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
    u16 fillOffset;                              /* Projection offset to sample the master PART span */
    u8 length;                                   /* Number of cells (1..16) */
    u8 segmentCount;                             /* Number of active segment styles for this layout */

    u8 *segmentIdByCell;                         /* [length] Segment style per cell */
    u8 *fillIndexByCell;                         /* [length] Fill order per cell */
    u8 *cellIndexByFillIndex;                    /* [length] Inverse LUT: cell index for a given fill index (O(1) lookup) */

    /* --- Tilemap positions --- */
    Vect2D_u16 *tilemapPosByCell;                /* [length] Tilemap X,Y coordinates per cell */
 
    /* --- Tilesets ---
     * Optional arrays can either:
     * - point to heap buffers owned by the layout, or
     * - point to shared read-only sentinel views when feature is disabled.
     */
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
     * Compact strip packing order is: row -> state -> col (SGDK row-major atlas).
     * For one segment:
     *   index = row * (sourceWidth * stateCount) + state * sourceWidth + col
     *
     * stateCount/source surface are derived from tileset size + stripCoverage.
     * Stored metadata per segment:
     *   - pipSourceWidthBySegment / pipSourceHeightBySegment
     *   - pipStripCoverageBySegment / pipHalfAxisBySegment
     *
     * Offset is applied on transverse axis only:
     *   - horizontal gauge: Y += segmentOffsetTiles + row
     *   - vertical gauge:   X += segmentOffsetTiles + row
     */
    const u32 **pipTilesetBySegment;              /* [segmentCount] compact strip: stateCount*width*height tiles */
    u8 *pipWidthBySegment;                        /* [segmentCount] width in tiles per segment style */
    u8 *pipHeightBySegment;                       /* [segmentCount] height in tiles per segment style (1..4) */
    u8 *pipOffsetBySegment;                       /* [segmentCount] transverse offset in tiles (>=0) */
    u8 *pipStateCountBySegment;                   /* [segmentCount] number of states (>=2: EMPTY, VALUE) */
    u8 *pipStripCoverageBySegment;                /* [segmentCount] GaugeStripCoverage */
    u8 *pipHalfAxisBySegment;                     /* [segmentCount] HALF mode axis: 0=horizontal, 1=vertical */
    u8 *pipSourceWidthBySegment;                  /* [segmentCount] source surface width in tiles per state */
    u8 *pipSourceHeightBySegment;                 /* [segmentCount] source surface height in tiles per state */

    /* --- PIP metadata (auto-built from fill order + per-segment PIP styles) --- */
    u8 pipCount;                                   /* number of logical pips in this layout */
    u8 *pipIndexByFillIndex;                       /* [length] fillIndex -> pip index */
    u8 *pipLocalTileByFillIndex;                   /* [length] fillIndex -> local tile inside pip */
    u8 *pipWidthByPipIndex;                        /* [length] pip index -> width in tiles */
    u8 pipRenderCount;                             /* number of physical PIP tiles to render (height-expanded) */
    u8 *pipRenderFillIndexByRenderIndex;           /* [pipRenderCount] render index -> fillIndex */
    u8 *pipRenderRowByRenderIndex;                 /* [pipRenderCount] render index -> row inside segment (0..height-1) */
    u8 *pipRenderSourceColByRenderIndex;           /* [pipRenderCount] render index -> source col in strip */
    u8 *pipRenderSourceRowByRenderIndex;           /* [pipRenderCount] render index -> source row in strip */
    u8 *pipRenderExtraHFlipByRenderIndex;          /* [pipRenderCount] render index -> extra HFLIP from coverage */
    u8 *pipRenderExtraVFlipByRenderIndex;          /* [pipRenderCount] render index -> extra VFLIP from coverage */

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
    u8 localEndOverrideEnabled;                          /* 1=slave terminal END override enabled (real BRIDGE via persistent flag, or beyond-part + master STANDARD_FULL; blink idx: OFF=8 ON=44) */

    /* --- Cached flags (pre-computed at init, avoid runtime scans) --- */
    u8 hasBlinkOff;                              /* 1 if any segment has blink-off tilesets (damage) */
    u8 hasGainBlinkOff;                          /* 1 if any segment has gain blink-off tilesets */
    u8 capStartEnabled;                          /* 1 if cap start tileset is configured */
    u8 capEndEnabled;                            /* 1 if cap end tileset is configured */
    u16 refCount;                                 /* Reference count (retained by parts) */

} GaugeLayout;


/* -----------------------------------------------------------------------------
   Internal legacy layout style structs (for renderer internals)
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
 * Legacy internal layout initialization config.
 *
 * Kept for internal build plumbing in gauge.c.
 * Public initialization should use GaugeBuilder_* APIs instead.
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

   TRAIL MODE BEHAVIOR:
   --------------------
   FOLLOW:
     - Gauge_decrease starts classic damage trail (hold -> blink -> shrink).

   DISABLED:
     - No trail (trail equals value).

   STATIC_TRAIL:
     - Trail stays at maxFillPixels (persistent lost region), no blink.

   STATIC_TRAIL_CRITICAL_BLINK:
     - Same as STATIC_TRAIL, with blink only when currentValue <= criticalValue.

   CRITICAL_TRAIL_BLINK:
     - No trail above threshold.
     - At/below threshold: trail spans max->value and blinks continuously.

   CRITICAL_VALUE_BLINK:
     - No trail.
     - At/below threshold: remaining value blinks.

   GAIN MODE (configured separately via Gauge_setGainMode):
   --------------------------------------------------------
   FOLLOW:
     - Gauge_increase starts gain trail (hold -> blink -> value catch-up),
       independent from the damage trail mode.

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
struct GaugeLogic
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
    u16 criticalValue;              /* Critical threshold in logical value units */

    /* Render cache (for change detection -- skip rendering if nothing changed) */
    u16 lastValuePixels;            /* Value pixels from last rendered frame */
    u16 lastTrailPixelsRendered;    /* Trail pixels from last rendered frame */

    /* --- 8-bit fields (packed, no padding) --- */

    u8 holdFramesRemaining;         /* Frames the trail stays still before blinking */
    u8 blinkFramesRemaining;        /* Frames of blinking left after hold */
    u8 valueAnimEnabled;            /* 0=value jumps instantly, 1=value slides smoothly */
    u8 valueAnimShift;              /* Value slide speed: step = distance >> shift + 1 */
    u8 trailAnimShift;              /* Damage trail shrink speed: step = distance >> shift + 1 */
    u8 blinkShift;                  /* Damage blink rate: toggles every 2^shift frames */
    u8 gainAnimShift;               /* Gain catch-up speed: step = distance >> shift + 1 */
    u8 gainBlinkShift;              /* Gain blink rate: toggles every 2^shift frames */
    u8 trailEnabled;                /* 0=no trail rendering, 1=trail rendering enabled */
    u8 configuredTrailMode;         /* GaugeTrailMode (configured behavior) */
    u8 configuredGainMode;          /* GaugeGainMode (configured behavior) */
    u8 activeTrailState;            /* Internal state: NONE / DAMAGE / GAIN */
    u8 modeUsesCriticalThreshold;   /* 1 when trail mode uses criticalValue threshold */
    u8 modeUsesValueBlink;          /* 1 when critical mode blinks remaining value (not trail) */
    u8 modeKeepsStaticTrail;        /* 1 when trail stays fixed at maxFillPixels */
    u8 lastBlinkOn;                 /* Blink ON/OFF state from last frame (for change detection) */
    u8 lastActiveTrailState;        /* Internal trail state from last frame (for change detection) */
    u8 needUpdate;                  /* 1=needs rendering, 0=fully idle (Gauge_update returns early) */

};


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
 * Change detection:
 *   cachedStrip + cachedFillIndex track the last uploaded tile.
 *   If both match the desired tile, the DMA is skipped (saves ~200 cycles).
 */
typedef struct
{
    const u32 *cachedStrip;   /* Last strip uploaded (for cache) */
    u16 vramTileIndex;              /* VRAM tile index for this cell */
    u8 cachedFillIndex;               /* Last fill index uploaded (0xFF=none) */
    u8 cellIndex;                   /* Index in layout (for fill calculation) */
    u8 pipFillIndex;                /* PIP render: source fillIndex (CACHE_INVALID_U8 when unused) */
    u8 pipRow;                      /* PIP render: local row in segment (0..height-1) */
    u8 pipRenderIndex;              /* PIP render: physical render index in layout LUTs */
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
    u16 *vramTilePipBase;                         /* [segmentCount] PIP dynamic slot base per segment */
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
   by the Gauge container and allocated during GaugeBuilder_build().

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

    /* --- Master projection --- */
    u8 masterFillIndexByCell[GAUGE_MAX_LENGTH]; /* [layout->length] Local cell -> master fill index LUT */
    u8 terminalBridgeFlagActive;                /* Persistent bridge flag for slave-part terminal override */

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

   2. Build (initialization + parts allocation):
        GaugeBuilder_init(&builder, &description);
        GaugeBuilder_addSegment(&builder, 0, &segmentDef);
        GaugeBuilder_build(&builder, &myGauge, vramBase, GAUGE_VRAM_DYNAMIC);

   3. Configure runtime behavior (optional, before first Gauge_update):
        Gauge_setValueAnim(&myGauge, 1, 4);   // enable smooth value changes
        Gauge_setTrailMode(&myGauge, GAUGE_TRAIL_MODE_FOLLOW, 0, 4, 3); // damage mode
        Gauge_setGainMode(&myGauge, GAUGE_GAIN_MODE_FOLLOW, 4, 3);       // gain mode

   4. Game loop:
        Gauge_update(&myGauge);  // call once per frame (tick + render)

   5. Change value:
        Gauge_decrease(&myGauge, 10, 20, 60); // damage: -10, hold 20f, blink 60f
        Gauge_increase(&myGauge, 5, 20, 60);  // heal: +5, hold 20f, blink 60f
        Gauge_setValue(&myGauge, 50);          // instant set (no trail)

   WHY MULTIPLE PARTS?
   -------------------
   A single Gauge can drive multiple GaugeParts. All parts share the same
   GaugeLogic, so they always show the same value in perfect sync.
   Runtime decisions are master-driven:
   - The largest PART is automatically selected as the master PART.
   - The master computes render decisions (type/class/index).
   - Slave PARTs only project local cells to master fill indices and
     reuse those decisions with their own local asset strips.

   Common use cases:
   - 2-lane gauge: 2 parts at Y and Y+1 for a 2-tile-tall bar
   - 2-lane with shorter bottom row: the shorter row uses a different fillOffset
     to project a subset of the master gauge span (set this in
     GaugeSlaveDescription.fillOffset for that slave).

   NOTE: For two independent gauges (e.g., P1 and P2 health bars), use
   two separate Gauge objects, each with its own builder configuration.

   VRAM ALLOCATION:
   ----------------
   VRAM is auto-allocated sequentially during GaugeBuilder_build().
   The gauge tracks vramNextOffset internally.

   ============================================================================= */
struct Gauge
{
    /* --- Logic (value state machine) --- */
    GaugeLogic logic;

    /* --- Parts (rendering units) --- */
    GaugePart **parts;          /* Dynamically allocated array of part pointers */
    GaugeTickAndRenderHandler *tickAndRenderHandler;       /* Active update path */
    GaugeTickAndRenderHandler *steadyTickAndRenderHandler; /* Handler used after first update lock */
    GaugeLogicTickHandler *logicTickHandler;               /* Active logic tick path (trail mode specific) */
    u8 partCount;               /* Number of active parts */
    u8 partCapacity;            /* Allocated part pointer capacity */
    u8 masterPartIndex;         /* Master part used to compute shared decisions */
    u8 masterPartHasBridge;     /* 1 when master part has bridge topology */
    u8 runtimeLocked;           /* Runtime state: 0=open, 1=has parts, 2=closed after first update */
    u8 debugMode;               /* Runtime debug traces (0=off, 1=on) */
    u32 debugTraceId;           /* Monotonic trace block counter (increments per rendered frame) */
    u16 masterSpanPixels;       /* Master part logical span in pixels (length * 8) */

    /* --- Master decision cache (indexed by master fill index) --- */
    u8 masterDecisionTypeByFillIndex[GAUGE_MAX_LENGTH];
    u8 masterDecisionIdxByFillIndex[GAUGE_MAX_LENGTH];
    u8 masterDecisionCapStartBreakByFillIndex[GAUGE_MAX_LENGTH];
    u8 masterDecisionCapStartTrailByFillIndex[GAUGE_MAX_LENGTH];
    u8 masterDecisionUseBlinkVariantByFillIndex[GAUGE_MAX_LENGTH];
    u8 masterPipStateByFillIndex[GAUGE_MAX_LENGTH];

    /* --- Builder-owned layouts (allocated by GaugeBuilder_build) --- */
    GaugeLayout *ownedLayouts[GAUGE_MAX_PARTS];
    u8 ownedLayoutCount;

    /* --- VRAM allocation --- */
    u16 vramBase;                   /* Base VRAM tile index */
    u16 vramNextOffset;             /* Offset for next part allocation */
    GaugeVramMode vramMode;         /* Default VRAM mode for parts */
    GaugeValueMode valueMode;       /* Value quantization mode */
};


/* -----------------------------------------------------------------------------
   Gauge Builder UX API (public initialization path)
   ----------------------------------------------------------------------------- */

/**
 * Segment assets for one visual state.
 *
 * All strips are optional except where required by the selected value mode:
 * - FILL mode requires normal.body
 * - PIP mode requires pipTileset and pipWidth in GaugeSegmentDefinition
 *
 * Bridge strips can be supplied per visual state (normal / gain / blink-off).
 */
typedef struct
{
    const u32 *body;
    const u32 *trail;
    const u32 *end;
    const u32 *bridge;
} GaugeSegmentAssets;

/**
 * Segment definition in fill order.
 *
 * IMPORTANT:
 * segment index 0 is ALWAYS the start of fill order.
 * - Horizontal + forward: segment 0 is leftmost.
 * - Horizontal + reverse: segment 0 is rightmost.
 * - Vertical + forward:   segment 0 is topmost.
 * - Vertical + reverse:   segment 0 is bottommost.
 *
 * Cap mapping in Builder V2:
 * - cap-start uses start segment assets (end/body/trail mapping).
 * - cap-end uses end segment assets (end fallback body).
 * No dedicated cap tilesets are required in the public API.
 *
 * Segment geometry extensions:
 * - segmentHeightTiles controls rendered segment height (in tiles).
 * - segmentOffsetTiles applies a positive transverse offset (in tiles):
 *   horizontal gauges offset on Y, vertical gauges offset on X.
 * - stripCoverage controls compact strip coverage in PIP mode:
 *   FULL (default), HALF, or QUARTER.
 * In this version, these fields are active for PIP rendering.
 */
typedef struct
{
    u8 cellCount;                    /* Number of cells covered by this segment */
    u8 segmentHeightTiles;           /* Segment height in tiles (PIP active now, FILL reserved for later) */
    u8 segmentOffsetTiles;           /* Positive transverse offset in tiles (PIP active now, FILL reserved for later) */
    u8 stripCoverage;                /* GaugeStripCoverage (PIP only in this version) */
    GaugeSegmentAssets normal;       /* Base strips (FILL mode) */
    GaugeSegmentAssets gain;         /* Gain strips (FILL mode) */
    GaugeSegmentAssets blinkOff;     /* Blink-off strips (FILL mode) */
    const TileSet *pipTileset;       /* Compact pip tileset (PIP mode) */
    u8 pipWidth;                     /* Width in tiles for one pip (PIP mode) */
} GaugeSegmentDefinition;

/**
 * Builder convenience macros (SGDK-friendly).
 *
 * GAUGE_SEGMENT_TILESETS(...) converts TileSet pointers to GaugeSegmentAssets.
 * GAUGE_SEGMENT_ATTR(...) builds FILL-mode segments.
 * GAUGE_PIP_SEGMENT_TILESET(...) / GAUGE_PIP_SEGMENT_TILESET_EX(...) build PIP-mode segments.
 * Non-EX macros default to segmentHeightTiles=1, segmentOffsetTiles=0,
 * and stripCoverage=GAUGE_STRIP_COVERAGE_FULL.
 * _EX signature:
 *   (logicalCellCount, pipTilesetRef, pipWidth, heightTiles, offsetTiles, stripCoverage)
 *
 * Example:
 *   GaugeSegmentDefinition segment = GAUGE_SEGMENT_ATTR(
 *       8,
 *       GAUGE_SEGMENT_TILESETS(&gauge_h_straight_yellow_strip, NULL, NULL, NULL),
 *       GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
 *       GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL));
 */
#define GAUGE_SEGMENT_TILESETS(bodyTileset, trailTileset, endTileset, bridgeTileset) \
    ((GaugeSegmentAssets){ \
        .body = ((bodyTileset) ? ((const TileSet *)(bodyTileset))->tiles : NULL), \
        .trail = ((trailTileset) ? ((const TileSet *)(trailTileset))->tiles : NULL), \
        .end = ((endTileset) ? ((const TileSet *)(endTileset))->tiles : NULL), \
        .bridge = ((bridgeTileset) ? ((const TileSet *)(bridgeTileset))->tiles : NULL) \
    })

#define GAUGE_SEGMENT_ATTR(cellCountValue, normalAssets, gainAssets, blinkOffAssets) \
    ((GaugeSegmentDefinition){ \
        .cellCount = (cellCountValue), \
        .segmentHeightTiles = 1, \
        .segmentOffsetTiles = 0, \
        .stripCoverage = GAUGE_STRIP_COVERAGE_FULL, \
        .normal = (normalAssets), \
        .gain = (gainAssets), \
        .blinkOff = (blinkOffAssets), \
        .pipTileset = NULL, \
        .pipWidth = 0 \
    })

#define GAUGE_PIP_SEGMENT_TILESET(logicalCellCount, pipTilesetRef, pipWidthValue) \
    GAUGE_PIP_SEGMENT_TILESET_EX((logicalCellCount), (pipTilesetRef), (pipWidthValue), 1, 0, GAUGE_STRIP_COVERAGE_FULL)

#define GAUGE_PIP_SEGMENT_TILESET_EX(logicalCellCount, pipTilesetRef, pipWidthValue, heightTilesValue, offsetTilesValue, stripCoverageValue) \
    ((GaugeSegmentDefinition){ \
        .cellCount = (logicalCellCount), \
        .segmentHeightTiles = (heightTilesValue), \
        .segmentOffsetTiles = (offsetTilesValue), \
        .stripCoverage = (stripCoverageValue), \
        .normal = GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL), \
        .gain = GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL), \
        .blinkOff = GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL), \
        .pipTileset = (pipTilesetRef), \
        .pipWidth = (pipWidthValue) \
    })

/**
 * Master gauge description.
 * initialValue is implicitly set to maxValue when built.
 * In FILL mode, if maxValue is 0, build defaults it to
 * (master sum of segment cellCount * 8).
 * Segment count is implicit: one segment per GaugeBuilder_addSegment() call
 * on partIndex 0 before build.
 */
typedef struct
{
    GaugeValueMode mode;                 /* GAUGE_VALUE_MODE_FILL or GAUGE_VALUE_MODE_PIP */
    GaugeOrientation orientation;
    GaugeFillDirection fillDirection;
    u16 originX;                         /* Master tile X position */
    u16 originY;                         /* Master tile Y position */
    u16 maxValue;                        /* Logical max value (initial is maxValue) */
    u8 capStartFixed;                    /* Auto-cap on fill start */
    u8 capEndFixed;                      /* Auto-cap on fill end */
    u8 palette;                          /* Palette line (0..3) */
    u8 priority;                         /* Tile priority (0 or 1) */
    u8 verticalFlip;                     /* Vertical flip flag (0 or 1) */
    u8 horizontalFlip;                   /* Horizontal flip flag (0 or 1) */
} GaugeDescription;

/**
 * Slave part description.
 * Segment order follows the same "start of fill order = segment 0" rule.
 * Segment count is implicit: one segment per GaugeBuilder_addSegment() call
 * on the returned slave part index before build.
 */
typedef struct
{
    u16 originX;                         /* Slave tile X position */
    u16 originY;                         /* Slave tile Y position */
    u16 fillOffset;                      /* Projection offset in pixels (default 0) */
} GaugeSlaveDescription;

/**
 * GaugeBuilder opaque state (public storage, internal behavior).
 */
typedef struct
{
    GaugeDescription description;
    GaugeSlaveDescription slaveDescriptions[GAUGE_MAX_PARTS - 1];
    GaugeSegmentDefinition segmentDefinitions[GAUGE_MAX_PARTS][GAUGE_MAX_SEGMENTS];
    u8 partLength[GAUGE_MAX_PARTS];                 /* Derived during build from segment cellCount sums */
    u8 partSegmentCount[GAUGE_MAX_PARTS];           /* Derived from number of addSegment calls */
    u16 partOriginX[GAUGE_MAX_PARTS];
    u16 partOriginY[GAUGE_MAX_PARTS];
    u16 partFillOffset[GAUGE_MAX_PARTS];
    u8 partCount;
    u8 initialized;
} GaugeBuilder;

/**
 * Initialize builder from a master gauge description.
 * Resets any previous builder state.
 * Part lengths are not provided in descriptions: they are derived at build
 * time from the sum of segment cellCount for each part.
 * Segment count is also derived from the number of addSegment calls per part.
 */
u8 GaugeBuilder_init(GaugeBuilder *builder, const GaugeDescription *description);

/**
 * Append one segment definition to a target part.
 * partIndex = 0 for master, >=1 for slaves (in add order).
 * Segment order is call order (first add = segment 0).
 * In PIP mode:
 * - segmentHeightTiles must be in [1..4]
 * - stripCoverage geometry must match pipTileset size
 * - resulting stateCount must be >= 2.
 */
u8 GaugeBuilder_addSegment(GaugeBuilder *builder,
                           u8 partIndex,
                           const GaugeSegmentDefinition *definition);

/**
 * Add one slave part.
 * Slave length is derived from sum of segment cellCount during build.
 * Build fails if derived slave length exceeds derived master length.
 * Returns its part index via outPartIndex (>= 1).
 */
u8 GaugeBuilder_addSlave(GaugeBuilder *builder,
                         const GaugeSlaveDescription *description,
                         u8 *outPartIndex);

/**
 * Build gauge runtime + all parts from the builder configuration.
 *
 * Default runtime preset applied at build:
 * - Gauge_setValueAnim(..., 0, default)
 * - Gauge_setTrailMode(..., GAUGE_TRAIL_MODE_FOLLOW, ...)
 * - Gauge_setGainMode(..., GAUGE_GAIN_MODE_DISABLED, ...)
 *
 * In FILL mode, maxValue==0 defaults to (derived master length * 8).
 * In PIP mode, maxValue is auto-aligned to computed pipCount.
 */
u8 GaugeBuilder_build(GaugeBuilder *builder,
                      Gauge *gauge,
                      u16 vramBase,
                      GaugeVramMode vramMode);


/* -----------------------------------------------------------------------------
   Gauge Runtime API
   ----------------------------------------------------------------------------- */

/**
 * Configure value animation.
 *
 * Configuration-time only:
 * - allowed before first Gauge_update() call
 * - no-op after first Gauge_update() call (runtime closed)
 *
 * @param gauge    Gauge to configure
 * @param enabled  Enable animated value changes (0=instant, 1=animated)
 * @param shift    Animation speed divider (3-6 recommended, 0=use default 4)
 */
void Gauge_setValueAnim(Gauge *gauge, u8 enabled, u8 shift);

/**
 * Configure damage trail behavior mode and timing.
 *
 * Configuration-time only:
 * - allowed before first Gauge_update() call
 * - no-op after first Gauge_update() call (runtime closed)
 *
 * @param gauge         Gauge to configure
 * @param mode          Damage trail behavior mode
 * @param criticalValue Critical threshold in logical value units (CRITICAL_* modes)
 * @param shift         Damage trail shrink speed divider (3-6 recommended, 0=use default 4)
 * @param blinkShift    Damage blink frequency divider (2-4 recommended, 0=use default 3)
 */
void Gauge_setTrailMode(Gauge *gauge,
                        GaugeTrailMode mode,
                        u16 criticalValue,
                        u8 shift,
                        u8 blinkShift);

/**
 * Configure gain behavior mode and timing.
 *
 * Configuration-time only:
 * - allowed before first Gauge_update() call
 * - no-op after first Gauge_update() call (runtime closed)
 *
 * @param gauge      Gauge to configure
 * @param mode       Gain behavior mode
 * @param shift      Gain catch-up speed divider (3-6 recommended, 0=use default 4)
 * @param blinkShift Gain blink frequency divider (2-4 recommended, 0=use default 3)
 */
void Gauge_setGainMode(Gauge *gauge,
                       GaugeGainMode mode,
                       u8 shift,
                       u8 blinkShift);

/**
 * Enable or disable runtime debug traces for this gauge.
 *
 * This only has effect when GAUGE_ENABLE_TRACE is set to 1 at compile time.
 *
 * When enabled (and trace code compiled in), fill-mode render emits GAUGE_TRACE
 * logs on visual changes:
 * - one frame header
 * - one line per rendered cell decision
 *
 * @param gauge    Gauge to modify
 * @param enabled  0=off, non-zero=on
 */
void Gauge_setDebugMode(Gauge *gauge, u8 enabled);

/**
 * Get current debug mode state.
 *
 * @param gauge  Gauge to query
 * @return 1 if debug mode is enabled, 0 otherwise
 */
u8 Gauge_getDebugMode(const Gauge *gauge);

/**
 * Release all allocations owned by the gauge (parts, buffers, retained layouts, LUTs).
 * Safe to call multiple times.
 */
void Gauge_release(Gauge *gauge);

/**
 * Update gauge: tick logic + render all parts.
 * Call once per frame.
 * First call locks runtime configuration APIs.
 *
 * Performs:
 * - Value animation (if enabled)
 * - Trail mode processing (follow/static/critical)
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
 * If gain mode is FOLLOW and valueAnim is enabled,
 * triggers gain trail (lead blink then catch-up).
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

#endif /* GAUGE_H */
