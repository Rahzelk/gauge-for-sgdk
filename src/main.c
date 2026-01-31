/* =============================================================================
   GAUGE MODULE - USAGE GUIDE
   =============================================================================

   This file demonstrates how to use the Gauge module for SGDK.
   Each example is self-contained and shows a different use case.

   STEPS TO USE THE GAUGE MODULE:
   ------------------------------

   1. INCLUDE THE MODULE
      #include "gauge.h"

   2. CREATE A LAYOUT (describes the visual appearance)
      - Define tilesets array (pointers to 45-tile strips from your resources)
      - Define segments array (which tileset index for each cell/tile)
      - Call GaugeLayout_init() with orientation, palette, priority, flip flags

   3. CREATE A GAUGE (manages the value and animation logic)
      - Declare a Gauge struct and a GaugePart array
      - Call Gauge_init() with maxValue, maxFillPixels, initialValue
      - Optionally configure animations:
        * Gauge_setValueAnim() for smooth value transitions
        * Gauge_setTrailAnim() for damage trail effect

   4. ADD PARTS TO THE GAUGE (visual representation on screen)
      - Call Gauge_addPart() for each visual instance
      - One gauge can have multiple parts (they share the same logic/value)
      - Each part can be at a different screen position

   5. UPDATE EVERY FRAME
      - Call Gauge_update() once per frame for each Gauge
      - This handles animation ticks and rendering

   6. MODIFY VALUES
      - Gauge_setValue(gauge, value) : instant change, no animation
      - Gauge_increase(gauge, amount) : heal effect (smooth if valueAnim enabled)
      - Gauge_decrease(gauge, amount, holdFrames, blinkFrames) : damage with trail

   VRAM MODES:
   -----------
   - GAUGE_VRAM_DYNAMIC : Fewer VRAM tiles, more CPU (recommended for most cases)
   - GAUGE_VRAM_FIXED   : More VRAM tiles (1 per cell), less CPU (for many gauges)

   ============================================================================= */


#include <genesis.h>
#include "gauge.h"
#include "gauge_assets.h"


/* =============================================================================
   CONSTANTS
   ============================================================================= */

/* VRAM base address for all gauges (after system tiles) */
#define VRAM_BASE   TILE_USER_INDEX

/* Tile lengths for each example */
#define EX1_LENGTH      12      /* Example 1: 12-tile yellow gauge */
#define EX2_LENGTH      16      /* Example 2: 16-tile yellow gauge */
#define EX3_PART1_LEN   12      /* Example 3 Part1: 12 tiles (3 segments) */
#define EX3_PART2_LEN    4      /* Example 3 Part2: 4 tiles yellow */
#define EX3_BLUE_LEN     8      /* Example 3 Blue: 8 tiles */
#define EX4_LENGTH      12      /* Example 4: 12 tiles vertical (3 segments) */
#define EX5_LENGTH      16      /* Example 5: 16 tiles horizontal */

/* Screen positions (in tiles) */
#define EX1_X           2
#define EX1_Y           12
#define EX1_MIRROR_X    (EX1_X + EX1_LENGTH + 4)    /* Right of Ex1 with gap */
#define EX1_MIRROR_Y    EX1_Y                       /* Same row as Ex1 */

#define EX2_X           2
#define EX2_Y           17      /* Below Ex1 */

#define EX3_X           2
#define EX3_Y           21      /* Below Ex2 */
#define EX3_PART2_X     2
#define EX3_PART2_Y     22      /* Below Ex3 Part1 */
#define EX3_BLUE_X      (EX3_PART2_X + EX3_PART2_LEN)   /* Right of Part2 */
#define EX3_BLUE_Y      22

#define EX4_X           36      /* Right side of screen */
#define EX4_Y           11      /* Top, extends down to row 15 */

#define EX5_X           2
#define EX5_Y           26      /* Below all other examples */
#define EX5_MIRROR_X    (EX5_X + EX5_LENGTH + 4)    /* Right of Ex5 with gap */
#define EX5_MIRROR_Y    EX5_Y                       /* Same row as Ex5 */

/* Number of gauges for selection */
#define GAUGE_COUNT     7


/* =============================================================================
   GLOBAL VARIABLES
   ============================================================================= */

/* --- Layouts (define visual appearance) --- */
static GaugeLayout s_layoutEx1;         /* Yellow 12 tiles horizontal */
static GaugeLayout s_layoutEx1Mirror;   /* Yellow 12 tiles horizontal (mirror) */
static GaugeLayout s_layoutEx2;         /* Yellow 16 tiles horizontal */
static GaugeLayout s_layoutEx3Part1;    /* Multi-segment: b1(3) + b2(5) + yellow(4) */
static GaugeLayout s_layoutEx3Part2;    /* Yellow 4 tiles */
static GaugeLayout s_layoutEx3Blue;     /* Blue 8 tiles */
static GaugeLayout s_layoutEx4;         /* Multi-segment vertical */
static GaugeLayout s_layoutEx5;         /* Yellow 16px with transition + cap */
static GaugeLayout s_layoutEx5Mirror;   /* Mirror of Ex5 */

/* --- Gauges (manage value and logic) --- */
static Gauge g_gaugeEx1;
static Gauge g_gaugeEx1Mirror;  /* Separate gauge (not a mirror part of Ex1) */
static Gauge g_gaugeEx2;
static Gauge g_gaugeEx3;        /* Has 2 parts: Part1 + Part2 */
static Gauge g_gaugeEx3Blue;    /* Independent blue gauge */
static Gauge g_gaugeEx4;
static Gauge g_gaugeEx5;
static Gauge g_gaugeEx5Mirror;

/* --- Parts arrays (visual instances) --- */
static GaugePart g_partsEx1[1];
static GaugePart g_partsEx1Mirror[1];
static GaugePart g_partsEx2[1];
static GaugePart g_partsEx3[2];         /* Part1 + Part2 share same gauge */
static GaugePart g_partsEx3Blue[1];
static GaugePart g_partsEx4[1];
static GaugePart g_partsEx5[1];
static GaugePart g_partsEx5Mirror[1];

/* --- Input state --- */
static u16 g_prevPad = 0;
static u8 g_selectedGauge = 0;          /* Currently controlled gauge (0-6) */
static u8 g_holdA = 0;                  /* Hold counter for button A */
static u8 g_holdB = 0;                  /* Hold counter for button B */

/* --- Blue gauge auto-wrap state --- */
static u16 g_bluePercent = 0;           /* 0-100 */
static u16 g_frameCount = 0;

/* Debug/test helpers removed (returning control to manual inputs). */


/* =============================================================================
   HELPER FUNCTIONS
   ============================================================================= */

/**
 * Get pointer to currently selected gauge.
 * Used by input handling to modify the right gauge.
 */
static Gauge* getSelectedGauge(void)
{
    switch (g_selectedGauge)
    {
        case 0: return &g_gaugeEx1;
        case 1: return &g_gaugeEx1Mirror;
        case 2: return &g_gaugeEx2;
        case 3: return &g_gaugeEx3;
        case 4: return &g_gaugeEx4;
        case 5: return &g_gaugeEx5;
        case 6: return &g_gaugeEx5Mirror;
        default: return &g_gaugeEx1;
    }
}

/**
 * Get name of currently selected gauge for display.
 */
static const char* getSelectedGaugeName(void)
{
    switch (g_selectedGauge)
    {
        case 0: return "Sample 1          ";
        case 1: return "Sample 1 Mirrored     ";
        case 2: return "Sample 2 Smooth anim  ";
        case 3: return "Sample 3 Multi Segment";
        case 4: return "Sample 4 Vertical     ";
        case 5: return "Sample 5 Cap+Border   ";
        case 6: return "Sample 6 Mirror Ex5   ";
        default: return "Unknown               ";
    }
}

/**
 * Log VRAM usage for a gauge.
 * Displays the VRAM base address and number of tiles used.
 */
static void logVramUsage(const char* name, u16 vramBase, u16 tileCount)
{
    KLog((char*)name);
    KLog_U2(" VRAM base=", vramBase, " tiles=", tileCount);
}


/* =============================================================================
   SETUP FUNCTIONS
   ============================================================================= */

/**
 * Configure WINDOW plane for full-screen gauge display.
 */
static void setupWindowFullScreen(void)
{
    VDP_setPlaneSize(64, 32, TRUE);
    VDP_setWindowHPos(TRUE, 0);
    VDP_setWindowVPos(TRUE, 0);
    VDP_setBackgroundColor(0);
    VDP_setTextPalette(PAL0);
    VDP_setTextPlane(WINDOW);
}

/**
 * Draw header text with controls information.
 */
static void drawHeader(void)
{   
    VDP_drawText("A:increase gauge   B:decrease gauge", 2, 1);
    VDP_drawText("C:select gauge", 2, 3);
    VDP_drawText("-------------------------------------", 0, 5);
    VDP_drawText("| Selected:                         |", 0, 6);
    VDP_drawText("-------------------------------------", 0, 7);
}

/**
 * Update the display of currently selected gauge.
 */
static void updateSelectedDisplay(void)
{
    VDP_drawText(getSelectedGaugeName(), 12, 6);
}


/* =============================================================================
   EXAMPLE 1: Simple yellow gauge + separate mirror gauge
   =============================================================================

   This example shows the basic usage:
   - Single-segment gauge (all tiles use the same tileset)
   - Two separate gauges side by side (not mirror parts, but independent gauges)
   - Trail animation enabled for damage effect
*/

static void initExample1(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* -------------------------------------------------------------------------
       EXAMPLE 1A: Left yellow gauge
       ------------------------------------------------------------------------- */

    /* Step 1: Define the tileset array
       - Index 0 = horizontal yellow fill strip (45 tiles)
       - Index 1 = NULL (unused segment slots) */
    const u32 *ex1Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment1.tiles,    /* Segment 0: yellow */
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };

    /* Step 2: Define which segment each cell uses
       - All 12 cells use segment 0 (yellow) */
    const u8 ex1Segments[EX1_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 3: Initialize the layout
       - 12 tiles long
       - Fill direction: forward (left to right)
       - Horizontal orientation
       - Palette 0, priority 1, no flip */
    GaugeLayout_init(&s_layoutEx1,
                     EX1_LENGTH,                 /* length in tiles */
                     GAUGE_FILL_FORWARD,         /* fill direction */
                     ex1Tilesets,                /* tilesets array */
                     ex1Segments,                /* segment per cell */
                     GAUGE_ORIENT_HORIZONTAL,    /* orientation */
                     PAL0,                       /* palette line */
                     1,                          /* priority (1=high) */
                     0,                          /* vflip */
                     0);                         /* hflip */

    /* Step 4: Calculate pixel dimensions
       - maxValue = maxFillPixels for 1:1 mapping (no LUT needed) */
    const u16 ex1MaxPixels = (u16)(EX1_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* Step 5: Initialize the gauge
       - Starts at full value
       - Uses VRAM_DYNAMIC mode (fewer tiles, more CPU efficient for single gauge) */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx1,
               ex1MaxPixels,             /* maxValue */
               ex1MaxPixels,             /* maxFillPixels */
               ex1MaxPixels,             /* initialValue (full) */
               g_partsEx1,               /* parts array */
               vramBase,                 /* VRAM base */
               GAUGE_VRAM_DYNAMIC);      /* VRAM mode */

    /* Step 6: Enable trail animation for damage effect */
    Gauge_setTrailAnim(&g_gaugeEx1, 1, 0, 0);   /* enabled, default speeds */

    /* Step 7: Add the visual part at screen position */
    Gauge_addPart(&g_gaugeEx1, &g_partsEx1[0],
                  &s_layoutEx1,
                  EX1_X, EX1_Y);

    /* Step 8: Log VRAM usage */
    vramSize = Gauge_getVramSize(&s_layoutEx1, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 1", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Step 9: Draw label under gauge */
    VDP_drawText("Sample 1", EX1_X, EX1_Y + 1);

    /* -------------------------------------------------------------------------
       EXAMPLE 1B: Right mirror gauge (separate gauge, same layout)
       ------------------------------------------------------------------------- */

    /* Use GaugeLayout_makeMirror to create a mirrored version of the layout
       - Automatically sets hflip for horizontal gauges
       - Fill direction is reversed */
    GaugeLayout_makeMirror(&s_layoutEx1Mirror, &s_layoutEx1);

    /* Initialize as a completely separate gauge (not sharing logic with Ex1) */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx1Mirror,
               ex1MaxPixels,
               ex1MaxPixels,
               ex1MaxPixels,
               g_partsEx1Mirror,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    Gauge_setTrailAnim(&g_gaugeEx1Mirror, 1, 0, 0);

    Gauge_addPart(&g_gaugeEx1Mirror, &g_partsEx1Mirror[0],
                  &s_layoutEx1Mirror,
                  EX1_MIRROR_X, EX1_MIRROR_Y);

    vramSize = Gauge_getVramSize(&s_layoutEx1Mirror, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 1 Mirrored", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Mirrored", EX1_MIRROR_X, EX1_MIRROR_Y + 1);
}


/* =============================================================================
   EXAMPLE 2: 16-tile gauge with value animation
   =============================================================================

   This example shows:
   - Longer gauge (16 tiles)
   - Value animation enabled (smooth transitions when value changes)
   - maxValue = maxFillPixels (1:1 pixel mapping)
*/

static void initExample2(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define tilesets (same yellow tileset) */
    const u32 *ex2Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment1.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };

    /* Step 2: Define segments (all yellow) */
    const u8 ex2Segments[EX2_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 3: Initialize layout */
    GaugeLayout_init(&s_layoutEx2,
                     EX2_LENGTH,
                     GAUGE_FILL_FORWARD,
                     ex2Tilesets,
                     ex2Segments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);

    /* Step 4: Calculate dimensions */
    const u16 ex2MaxPixels = (u16)(EX2_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* Step 5: Initialize gauge */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx2,
               100,             /* maxValue = maxPixels (1:1 mapping) */
               ex2MaxPixels,
               ex2MaxPixels,             /* Start full */
               g_partsEx2,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    /* Step 6: Enable BOTH value animation AND trail animation
       - Value animation: smooth increase/decrease transitions
       - Trail animation: visual trail on damage */
    Gauge_setValueAnim(&g_gaugeEx2, 1, 2);      /* enabled, default speed */
    Gauge_setTrailAnim(&g_gaugeEx2, 1, 3, 2);   /* enabled, default speeds */

    /* Step 7: Add part */
    Gauge_addPart(&g_gaugeEx2, &g_partsEx2[0],
                  &s_layoutEx2,
                  EX2_X, EX2_Y);

    /* Step 8: Log VRAM */
    vramSize = Gauge_getVramSize(&s_layoutEx2, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 2", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 2", EX2_X, EX2_Y + 1);
}


/* =============================================================================
   EXAMPLE 3: Multi-segment gauge with two parts + independent blue gauge
   =============================================================================

   This example shows:
   - Multi-segment layout: different tilesets for different sections
     * Segment 0 (b1): 3 tiles - first color
     * Segment 1 (b2): 5 tiles - second color
     * Segment 2 (yellow): 4 tiles - third color
   - Two parts sharing the same gauge logic (synchronized value)
   - Independent blue gauge with auto-wrap demo
*/

static void initExample3(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* -------------------------------------------------------------------------
       EXAMPLE 3A: Multi-segment gauge (Part1 = 12 tiles, Part2 = 4 tiles)
       ------------------------------------------------------------------------- */

    /* Step 1: Define tilesets for 3 different segments
       - Segment 0: b1 (blue variant 1)
       - Segment 1: b2 (blue variant 2)
       - Segment 2: yellow */
    const u32 *ex3Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_b2_strip_h_segment1.tiles,        /* Segment 0: b1 */
        gauge_b1_strip_h_segment1.tiles,        /* Segment 1: b2 */
        gauge_yellow_strip_h_segment1.tiles,    /* Segment 2: yellow */
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL
    };

    /* Step 2: Define segment assignments for Part1 (12 tiles)
        - First 4 tiles: segment 2 (yellow) 
        - Last 3 tiles: segment 0 (b1)
        - Next 5 tiles: segment 1 (b2)
    */          
    const u8 ex3Part1Segments[EX3_PART1_LEN] = {
        2, 2, 2, 2,                 /* 4 tiles yellow */
        1, 1, 1, 1, 1, 1, 1,        /* 7 tiles b2 */
        0                           /* 1 tiles b1 */
    };

    GaugeLayout_init(&s_layoutEx3Part1,
                     EX3_PART1_LEN,
                     GAUGE_FILL_FORWARD,
                     ex3Tilesets,
                     ex3Part1Segments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);

    /* Step 3: Define layout for Part2 (4 tiles yellow only) */
    const u32 *ex3Part2Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment1.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 ex3Part2Segments[EX3_PART2_LEN] = { 0, 0, 0, 0 };

    GaugeLayout_init(&s_layoutEx3Part2,
                     EX3_PART2_LEN,
                     GAUGE_FILL_FORWARD,
                     ex3Part2Tilesets,
                     ex3Part2Segments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);

    /* Step 4: Initialize gauge (covers both parts)
       - maxValue = Part1 pixels (12 tiles = 96 pixels)
       - Part2 will show the "overflow" visually */
    const u16 ex3MaxPixels = (u16)(EX3_PART1_LEN * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx3,
               ex3MaxPixels,
               ex3MaxPixels,
               ex3MaxPixels,
               g_partsEx3,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    Gauge_setTrailAnim(&g_gaugeEx3, 1, 0, 0);

    /* Step 5: Add Part1 (main 12-tile gauge) */
    Gauge_addPart(&g_gaugeEx3, &g_partsEx3[0],
                  &s_layoutEx3Part1,
                  EX3_X, EX3_Y);

    vramSize = Gauge_getVramSize(&s_layoutEx3Part1, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 3 top part gauge", vramBase, vramSize);
    vramBase = (u16)(vramBase + vramSize);

    /* Step 6: Add Part2 (4-tile secondary display)
       - Shares the same gauge logic as Part1
       - Value changes are synchronized */
    Gauge_addPart(&g_gaugeEx3, &g_partsEx3[1],
                  &s_layoutEx3Part2,
                  EX3_PART2_X, EX3_PART2_Y);

    vramSize = Gauge_getVramSize(&s_layoutEx3Part2, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 3 bottom part gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* -------------------------------------------------------------------------
       EXAMPLE 3B: Independent blue gauge with auto-wrap
       ------------------------------------------------------------------------- */

    /* This gauge demonstrates:
       - Separate gauge with its own value
       - maxValue != maxFillPixels (uses internal LUT for mapping)
       - No trail animation */

    const u32 *blueTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_blue_strip_h_segment1.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 blueSegments[EX3_BLUE_LEN] = { 0, 0, 0, 0, 0, 0, 0, 0 };

    GaugeLayout_init(&s_layoutEx3Blue,
                     EX3_BLUE_LEN,
                     GAUGE_FILL_FORWARD,
                     blueTilesets,
                     blueSegments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);

    const u16 blueMaxPixels = (u16)(EX3_BLUE_LEN * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx3Blue,
               100,                      /* maxValue = 100 (percent) */
               blueMaxPixels,            /* maxFillPixels = 64 */
               0,                        /* initialValue = 0 (empty) */
               g_partsEx3Blue,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    /* No trail animation for blue gauge (disabled by default) */

    Gauge_addPart(&g_gaugeEx3Blue, &g_partsEx3Blue[0],
                  &s_layoutEx3Blue,
                  EX3_BLUE_X, EX3_BLUE_Y);

    vramSize = Gauge_getVramSize(&s_layoutEx3Blue, GAUGE_VRAM_DYNAMIC, 0);
    logVramUsage("Sample 3 mini-auto-incremental-blue gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw labels under gauges */
    VDP_drawText("Sample 3 Multi-Segment", EX3_X, EX3_PART2_Y + 1);
    VDP_drawText("    and  Multi-Part", EX3_X, EX3_PART2_Y + 2);
}


/* =============================================================================
   EXAMPLE 4: Vertical multi-segment gauge with FIXED VRAM mode
   =============================================================================

   This example shows:
   - Vertical orientation
   - Same multi-segment layout as Ex3 (b1 + b2 + yellow)
   - GAUGE_VRAM_FIXED mode (more VRAM, less CPU)
   - Reverse fill direction (bottom to top)
*/

static void initExample4(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define tilesets (same 3 segments as Ex3, but vertical versions) */
    const u32 *ex4Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_b1_strip_v_segment1.tiles,        /* Segment 0: b1 vertical */
        gauge_b2_strip_v_segment1.tiles,        /* Segment 1: b2 vertical */
        gauge_yellow_strip_v_segment1.tiles,    /* Segment 2: yellow vertical */
        NULL
    };

    /* Step 2: Define segments (same pattern as Ex3 Part1) */
    const u8 ex4Segments[EX4_LENGTH] = {
        0, 0, 0,                    /* 3 tiles b1 */
        1, 1, 1, 1, 1,              /* 5 tiles b2 */
        2, 2, 2, 2                  /* 4 tiles yellow */
    };

    /* Step 3: Initialize layout
       - Vertical orientation
       - Reverse fill (bottom to top, typical for vertical gauges) */
    GaugeLayout_init(&s_layoutEx4,
                     EX4_LENGTH,
                     GAUGE_FILL_REVERSE,         /* Fill from bottom to top */
                     ex4Tilesets,
                     ex4Segments,
                     GAUGE_ORIENT_VERTICAL,      /* Vertical */
                     PAL0, 1, 0, 0);

    /* Step 4: Initialize gauge with FIXED VRAM mode
       - FIXED mode: allocates 1 VRAM tile per cell (12 tiles)
       - More VRAM usage, but less CPU per frame
       - Good for many gauges or when CPU is tight */
    const u16 ex4MaxPixels = (u16)(EX4_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx4,
               ex4MaxPixels,
               ex4MaxPixels,
               ex4MaxPixels,
               g_partsEx4,
               vramBase,
               GAUGE_VRAM_FIXED);        /* FIXED mode */

    Gauge_setTrailAnim(&g_gaugeEx4, 1, 0, 0);

    /* Step 5: Add part */
    Gauge_addPart(&g_gaugeEx4, &g_partsEx4[0],
                  &s_layoutEx4,
                  EX4_X, EX4_Y);

    /* Step 6: Log VRAM (FIXED mode uses more tiles) */
    vramSize = Gauge_getVramSize(&s_layoutEx4, GAUGE_VRAM_FIXED, 1);
    logVramUsage("Sample 4 (VRAM FIXED)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge (vertical gauge: label below bottom) */
    VDP_drawText("Sample 4", EX4_X-5, EX4_Y + EX4_LENGTH);
    VDP_drawText("Vertical", EX4_X-5, EX4_Y + EX4_LENGTH+1);
}


/* =============================================================================
   EXAMPLE 5: 16-tile horizontal gauge with transition + cap
   =============================================================================

   This example shows:
   - Horizontal gauge (16 tiles)
   - Custom transition tileset before the terminaison
   - Custom termination tileset (cap) at the terminaison
   - Trail enabled (uses red trail pixels from tileset)
*/

static void initExample5(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define BODY/BREAK/END/TRAIL tilesets (yellow) */
    const u32 *ex5BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_body.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5BreakTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_body.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };

    /* Step 2: Define segments (all yellow) */
    const u8 ex5Segments[EX5_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 3: Initialize layout (BODY + BREAK + END) */
    GaugeLayout_initEx(&s_layoutEx5,
                       EX5_LENGTH,
                       GAUGE_FILL_FORWARD,
                       ex5BodyTilesets,
                       ex5EndTilesets,
                       ex5BreakTilesets,
                       ex5TrailTilesets,
                       ex5Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);

#if GAUGE_DEBUG
    /* Debug: ensure each strip has exactly 45 tiles (required by gauge logic). */
    KLog("EX5 tileset sizes:");
    KLog_U1("  body numTile=", gauge_yellow_strip_h_segment_body.numTile);
    KLog_U1("  break numTile=", gauge_yellow_strip_h_segment_body.numTile);
    KLog_U1("  end numTile=", gauge_yellow_strip_h_segment_end.numTile);
    KLog_U1("  trail numTile=", gauge_yellow_strip_h_segment_trail.numTile);
#endif


    /* Step 4: Initialize gauge (16px max) */
    const u16 ex5MaxPixels = (u16)(EX5_LENGTH * GAUGE_PIXELS_PER_TILE); /* 128 px */
 
    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx5,
               ex5MaxPixels,
               ex5MaxPixels,
               ex5MaxPixels,
               g_partsEx5,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    Gauge_setTrailAnim(&g_gaugeEx5, 1, 0, 0);

    /* Step 5: Add part */
    Gauge_addPart(&g_gaugeEx5, &g_partsEx5[0],
                  &s_layoutEx5,
                  EX5_X, EX5_Y);

    /* Step 6: Log VRAM */
    vramSize = Gauge_getVramSize(&s_layoutEx5, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 5 (Cap+Border)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 5", EX5_X, EX5_Y + 1);
}

/* =============================================================================
   EXAMPLE 6: Mirror of Example 5 (VRAM FIXED)
   =============================================================================

   This example shows:
   - Mirror layout created from Example 5
   - Positioned facing Example 5
   - VRAM FIXED mode for verification
*/

static void initExample6(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Create mirror layout from Example 5 */
    GaugeLayout_makeMirror(&s_layoutEx5Mirror, &s_layoutEx5);

    /* Step 2: Initialize gauge (same size as Example 5) */
    const u16 ex5MaxPixels = (u16)(EX5_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx5Mirror,
               ex5MaxPixels,
               ex5MaxPixels,
               ex5MaxPixels,
               g_partsEx5Mirror,
               vramBase,
               GAUGE_VRAM_FIXED);       /* FIXED mode */

    Gauge_setTrailAnim(&g_gaugeEx5Mirror, 1, 0, 0);

    /* Step 3: Add part (mirrored layout) */
    Gauge_addPart(&g_gaugeEx5Mirror, &g_partsEx5Mirror[0],
                  &s_layoutEx5Mirror,
                  EX5_MIRROR_X, EX5_MIRROR_Y);

    /* Step 4: Log VRAM */
    vramSize = Gauge_getVramSize(&s_layoutEx5Mirror, GAUGE_VRAM_FIXED, 1);
    logVramUsage("Sample 6 (Mirror Ex5, VRAM FIXED)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 6", EX5_MIRROR_X, EX5_MIRROR_Y + 1);
}


/* =============================================================================
   INPUT HANDLING
   ============================================================================= */

/**
 * Handle button press events.
 */
static void handleInput(u16 pressed, u16 held)
{
    /* Button C: cycle through gauges */
    if (pressed & BUTTON_C)
    {
        g_selectedGauge = (u8)((g_selectedGauge + 1) % GAUGE_COUNT);
        updateSelectedDisplay();
    }

    /* Button A: increase (on press) */
    if (pressed & BUTTON_A)
    {
        Gauge_increase(getSelectedGauge(), 4);
        g_holdA = 0;
    }

    /* Button B: decrease with trail effect (on press) */
    if (pressed & BUTTON_B)
    {
        Gauge_decrease(getSelectedGauge(), 4, 20, 190);
        g_holdB = 0;
    }

    /* Hold repeat for A and B */
    if (held & BUTTON_A)
    {
        g_holdA++;
        if (g_holdA >= 12 && (g_frameCount & 3) == 0)
        {
            Gauge_increase(getSelectedGauge(), 1);
        }
    }
    else
    {
        g_holdA = 0;
    }

    if (held & BUTTON_B)
    {
        g_holdB++;
        if (g_holdB >= 12 && (g_frameCount & 3) == 0)
        {
            /* Continuous drain without re-triggering blink */
            Gauge_decrease(getSelectedGauge(), 1, 0, 0);
        }
    }
    else
    {
        g_holdB = 0;
    }
}


/* =============================================================================
   UPDATE FUNCTIONS
   ============================================================================= */

/**
 * Auto-wrap blue gauge 0->100->0 for demo.
 */
static void tickBlueAutoWrap(void)
{
    /* Increment every 2 frames (~3.3 seconds per full cycle at 60fps) */
    if ((g_frameCount & 1) == 0)
    {
        g_bluePercent++;
        if (g_bluePercent > 100)
            g_bluePercent = 0;

        Gauge_setValue(&g_gaugeEx3Blue, g_bluePercent);
    }
}

/* Targeted debug tests removed. */

/**
 * Update all gauges (tick logic + render).
 * Call this once per frame.
 */
static void updateAllGauges(void)
{
    Gauge_update(&g_gaugeEx1);
    Gauge_update(&g_gaugeEx1Mirror);
    Gauge_update(&g_gaugeEx2);
    Gauge_update(&g_gaugeEx3);
    //Gauge_update(&g_gaugeEx3Blue);
    Gauge_update(&g_gaugeEx4);
    Gauge_update(&g_gaugeEx5);
    Gauge_update(&g_gaugeEx5Mirror);
}


/* =============================================================================
   MAIN ENTRY POINT
   ============================================================================= */

int main(bool hardReset)
{
    /* Enlarge DMA queue if needed (for streaming many tiles) */
    if (DMA_getMaxQueueSize() < 160)
        DMA_setMaxQueueSize(160);

    /* Initialize joypad and display */
    JOY_init();
    setupWindowFullScreen();

    /* Load palette (shared by all gauges) */
    PAL_setPalette(PAL0, gauge_palette.data, DMA);

    /* Draw header */
    drawHeader();
    updateSelectedDisplay();

    /* -------------------------------------------------------------------------
       Initialize all examples
       Each example is self-contained and uses the next available VRAM slot
       ------------------------------------------------------------------------- */
    u16 nextVram = VRAM_BASE;

    KLog("=== GAUGE MODULE DEMO - VRAM ALLOCATION ===");

    initExample1(&nextVram);    /* Ex1 + Ex1Mirror */
    initExample2(&nextVram);    /* Ex2 (16 tiles, valueAnim) */
    initExample3(&nextVram);    /* Ex3 (multi-segment + blue) */
    initExample4(&nextVram);    /* Ex4 (vertical, FIXED mode) */
    initExample5(&nextVram);    /* Ex5 (16 tiles with transition + cap) */
    initExample6(&nextVram);    /* Ex6 (mirror of Ex5, FIXED mode) */

    KLog_U1("Total tiles used in VRAM: ", (u16)(nextVram - VRAM_BASE));
    KLog("============================================");
        
    /* -------------------------------------------------------------------------
       Main loop
       ------------------------------------------------------------------------- */
    while (TRUE)
    { 
        g_frameCount++;

        /* Read input */
        const u16 padState = JOY_readJoypad(JOY_1);
        const u16 pressed = (u16)((padState ^ g_prevPad) & padState);
        g_prevPad = padState;

        /* Handle input */
        handleInput(pressed, padState);

        /* Blue gauge auto-wrap demo */
        tickBlueAutoWrap();

        /* Update all gauges (tick + render) */
        updateAllGauges();

        /* Wait for VBlank and process DMA queue */
        SYS_doVBlankProcess();
    }

    return 0;
}
