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
#define EX2_LENGTH      16      /* Example 2: 16 tiles cap+border */
#define EX3_LENGTH      16      /* Example 3: 16 tiles multi-bridge */
#define EX4_LENGTH      12      /* Example 4: 12 tiles vertical (3 segments) */
#define EX5_PART1_LEN   12      /* Example 5 Part1: 12 tiles (3 segments) */
#define EX5_PART2_LEN    4      /* Example 5 Part2: 4 tiles yellow */
#define EX5_BLUE_LEN     8      /* Example 5 Blue: 8 tiles */

/* Screen positions (in tiles) */
#define EX1_X           2
#define EX1_Y           12
#define EX1_MIRROR_X    (EX1_X + EX1_LENGTH + 4)    /* Right of Ex1 with gap */
#define EX1_MIRROR_Y    EX1_Y                       /* Same row as Ex1 */

#define EX2_X           2
#define EX2_Y           17      /* Below Ex1 */

#define EX3_X           2
#define EX3_Y           21      /* Below Ex2 */

#define EX4_X           36      /* Right side of screen */
#define EX4_Y           11      /* Top, extends down to row 15 */

#define EX5_X           2
#define EX5_Y           26      /* Below Ex3 */
#define EX5_PART2_X     2
#define EX5_PART2_Y     (EX5_Y + 1)      /* Below Ex5 Part1 */
#define EX5_BLUE_X      (EX5_PART2_X + EX5_PART2_LEN)   /* Right of Part2 */
#define EX5_BLUE_Y      EX5_PART2_Y

/* Number of gauges for selection */
#define GAUGE_COUNT     6


/* =============================================================================
   GLOBAL VARIABLES
   ============================================================================= */

/* --- Layouts (define visual appearance) --- */
static GaugeLayout s_layoutEx1;         /* Yellow 12 tiles horizontal */
static GaugeLayout s_layoutEx1Mirror;   /* Yellow 12 tiles horizontal (mirror) */
static GaugeLayout s_layoutEx2;         /* Yellow 16 tiles cap+border */
static GaugeLayout s_layoutEx3;         /* Multi-bridge (ciel -> blue -> yellow) */
static GaugeLayout s_layoutEx5Part1;    /* Multi-segment: b1(3) + b2(5) + yellow(4) */
static GaugeLayout s_layoutEx5Part2;    /* Yellow 4 tiles */
static GaugeLayout s_layoutEx5Blue;     /* Blue 8 tiles */
static GaugeLayout s_layoutEx4;         /* Multi-segment vertical */

/* --- Gauges (manage value and logic) --- */
static Gauge g_gaugeEx1;
static Gauge g_gaugeEx1Mirror;  /* Separate gauge (not a mirror part of Ex1) */
static Gauge g_gaugeEx2;
static Gauge g_gaugeEx3;
static Gauge g_gaugeEx4;
static Gauge g_gaugeEx5;        /* Has 2 parts: Part1 + Part2 */
static Gauge g_gaugeEx5Blue;    /* Independent blue gauge */

/* --- Parts arrays (visual instances) --- */
static GaugePart g_partsEx1[1];
static GaugePart g_partsEx1Mirror[1];
static GaugePart g_partsEx2[1];
static GaugePart g_partsEx3[1];
static GaugePart g_partsEx4[1];
static GaugePart g_partsEx5[2];         /* Part1 + Part2 share same gauge */
static GaugePart g_partsEx5Blue[1];

/* --- Input state --- */
static u16 g_prevPad = 0;
static u8 g_selectedGauge = 0;          /* Currently controlled gauge (0-5) */
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
        case 2: return "Sample 2 Cap+Border   ";
        case 3: return "Sample 3 Bridge       ";
        case 4: return "Sample 4 Vertical     ";
        case 5: return "Sample 5 Multi Segment";
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

/**
 * Log dynamic VRAM tiles for a part (debugging overlaps).
 */
static void logDynamicVramTiles(const char* name, const GaugePart* part)
{
    KLog((char*)name);
    KLog_U1("  partial V=", part->dyn.vramTilePartialValue);
    KLog_U1("  partial T=", part->dyn.vramTilePartialTrail);
    KLog_U1("  partial E=", part->dyn.vramTilePartialEnd);
    KLog_U1("  partial T2=", part->dyn.vramTilePartialTrailSecond);
    KLog_U3("  seg0 E=", part->dyn.vramTileEmpty[0],
            " F=", part->dyn.vramTileFullValue[0],
            " T=", part->dyn.vramTileFullTrail[0]);
    KLog_U3("  seg1 E=", part->dyn.vramTileEmpty[1],
            " F=", part->dyn.vramTileFullValue[1],
            " T=", part->dyn.vramTileFullTrail[1]);
    KLog_U3("  seg2 E=", part->dyn.vramTileEmpty[2],
            " F=", part->dyn.vramTileFullValue[2],
            " T=", part->dyn.vramTileFullTrail[2]);
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
   EXAMPLE 2: 16-tile gauge with cap+border
   ============================================================================= */

static void initExample2(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define BODY/BREAK/END/TRAIL tilesets (yellow) */
    const u32 *ex2BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_body.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex2EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex2BreakTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_body.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex2TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };

    /* Step 2: Define segments (all yellow) */
    const u8 ex2Segments[EX2_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 3: Initialize layout (BODY + BREAK + END) */
    GaugeLayout_initEx(&s_layoutEx2,
                       EX2_LENGTH,
                       GAUGE_FILL_FORWARD,
                       ex2BodyTilesets,
                       ex2EndTilesets,
                       ex2BreakTilesets,
                       ex2TrailTilesets,
                       NULL,
                       ex2Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);

    /* Step 4: Initialize gauge (16px max) */
    const u16 ex2MaxPixels = (u16)(EX2_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx2,
               ex2MaxPixels,
               ex2MaxPixels,
               ex2MaxPixels,
               g_partsEx2,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    Gauge_setTrailAnim(&g_gaugeEx2, 1, 0, 0);

    /* Step 5: Add part */
    Gauge_addPart(&g_gaugeEx2, &g_partsEx2[0],
                  &s_layoutEx2,
                  EX2_X, EX2_Y);

    /* Step 6: Log VRAM */
    vramSize = Gauge_getVramSize(&s_layoutEx2, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 2 (Cap+Border)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 2", EX2_X, EX2_Y + 1);
}


/* =============================================================================
   EXAMPLE 3: 16-tile gauge with multi-bridge segments
   ============================================================================= */

static void initExample3(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define tilesets (ciel -> blue -> yellow) */
    const u32 *ex3BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_ciel_strip_h_segment_body.tiles,     /* Segment 0: ciel body */
        gauge_blue_strip_h_segment_body.tiles,     /* Segment 1: blue body */
        gauge_yellow_strip_h_segment_body.tiles,   /* Segment 2: yellow body */
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL
    };
    const u32 *ex3EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_ciel_strip_h_segment_end.tiles,      /* Segment 0: ciel end */
        gauge_blue_strip_h_segment_end.tiles,      /* Segment 1: blue end */
        gauge_yellow_strip_h_segment_end.tiles,    /* Segment 2: yellow end */
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL
    };
    const u32 *ex3BreakTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_ciel_strip_h_segment_body.tiles,     /* Segment 0: ciel break */
        gauge_blue_strip_h_segment_body.tiles,     /* Segment 1: blue break */
        gauge_yellow_strip_h_segment_body.tiles,   /* Segment 2: yellow break */
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL
    };
    const u32 *ex3TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_ciel_strip_h_segment_trail.tiles,    /* Segment 0: ciel trail */
        gauge_blue_strip_h_segment_trail.tiles,    /* Segment 1: blue trail */
        gauge_yellow_strip_h_segment_trail.tiles,  /* Segment 2: yellow trail */
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL
    };
    const u32 *ex3BridgeTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_ciel_strip_h_segment_bridge.tiles,   /* Segment 0: ciel->blue bridge */
        gauge_blue_strip_h_segment_bridge.tiles,   /* Segment 1: blue->yellow bridge */
        NULL,                                      /* Segment 2: no bridge */
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL
    };

    /* Step 2: Define segments (5 ciel + 5 blue + 6 yellow) */
    const u8 ex3Segments[EX3_LENGTH] = {
        0, 0, 0, 0, 0,
        1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2
    };

    /* Step 3: Initialize layout */
    GaugeLayout_initEx(&s_layoutEx3,
                       EX3_LENGTH,
                       GAUGE_FILL_FORWARD,
                       ex3BodyTilesets,
                       ex3EndTilesets,
                       ex3BreakTilesets,
                       ex3TrailTilesets,
                       ex3BridgeTilesets,
                       ex3Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);

    /* Step 4: Calculate dimensions */
    const u16 ex3MaxPixels = (u16)(EX3_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* Step 5: Initialize gauge */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx3,
               100,             /* maxValue = 100 (LUT maps to pixels) */
               ex3MaxPixels,
               ex3MaxPixels,             /* Start full */
               g_partsEx3,
               vramBase,
               GAUGE_VRAM_DYNAMIC);      /* DYNAMIC mode for bridge demo */

    /* Step 6: Enable BOTH value animation AND trail animation */
    Gauge_setValueAnim(&g_gaugeEx3, 1, 2);      /* enabled, default speed */
    Gauge_setTrailAnim(&g_gaugeEx3, 1, 3, 2);   /* enabled, default speeds */

    /* Step 7: Add part */
    Gauge_addPart(&g_gaugeEx3, &g_partsEx3[0],
                  &s_layoutEx3,
                  EX3_X, EX3_Y);

    /* Step 8: Log VRAM */
    vramSize = Gauge_getVramSize(&s_layoutEx3, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 3 (VRAM DYNAMIC)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 3", EX3_X, EX3_Y + 1);
    VDP_drawText("Bridge", EX3_X, EX3_Y + 2);
}

/* =============================================================================
   EXAMPLE 4: Vertical multi-segment gauge with FIXED VRAM mode
   =============================================================================

   This example shows:
   - Vertical orientation
   - Same multi-segment layout as Ex5 (b1 + b2 + yellow)
   - GAUGE_VRAM_FIXED mode (more VRAM, less CPU)
   - Reverse fill direction (bottom to top)
*/

static void initExample4(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define tilesets (same 3 segments as Ex5, but vertical versions) */
    const u32 *ex4Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_b1_strip_v_segment1.tiles,        /* Segment 0: b1 vertical */
        gauge_b2_strip_v_segment1.tiles,        /* Segment 1: b2 vertical */
        gauge_yellow_strip_v_segment1.tiles,    /* Segment 2: yellow vertical */
        NULL
    };

    /* Step 2: Define segments (same pattern as Ex5 Part1) */
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
   EXAMPLE 5: Multi-segment gauge with two parts + independent blue gauge
   =============================================================================

   This example shows:
   - Multi-segment layout: different tilesets for different sections
     * Segment 0 (b1): 3 tiles - first color
     * Segment 1 (b2): 5 tiles - second color
     * Segment 2 (yellow): 4 tiles - third color
   - Two parts sharing the same gauge logic (synchronized value)
   - Independent blue gauge with auto-wrap demo
*/

static void initExample5(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* -------------------------------------------------------------------------
       EXAMPLE 5A: Multi-segment gauge (Part1 = 12 tiles, Part2 = 4 tiles)
       ------------------------------------------------------------------------- */

    /* Step 1: Define tilesets for 3 different segments
       - Segment 0: b1 (blue variant 1)
       - Segment 1: b2 (blue variant 2)
       - Segment 2: yellow */
    const u32 *ex5Tilesets[GAUGE_MAX_SEGMENTS] = {
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
    const u8 ex5Part1Segments[EX5_PART1_LEN] = {
        2, 2, 2, 2,                 /* 4 tiles yellow */
        1, 1, 1, 1, 1, 1, 1,        /* 7 tiles b2 */
        0                           /* 1 tiles b1 */
    };

    GaugeLayout_init(&s_layoutEx5Part1,
                     EX5_PART1_LEN,
                     GAUGE_FILL_FORWARD,
                     ex5Tilesets,
                     ex5Part1Segments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);

    /* Step 3: Define layout for Part2 (4 tiles yellow only) */
    const u32 *ex5Part2Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_yellow_strip_h_segment1.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 ex5Part2Segments[EX5_PART2_LEN] = { 0, 0, 0, 0 };

    GaugeLayout_init(&s_layoutEx5Part2,
                     EX5_PART2_LEN,
                     GAUGE_FILL_FORWARD,
                     ex5Part2Tilesets,
                     ex5Part2Segments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);

    /* Step 4: Initialize gauge (max pixels = Part1 only)
       - maxValue = Part1 pixels (12 tiles = 96 pixels)
       - Part2 will show the "overflow" visually */
    const u16 ex5MaxPixels = (u16)(EX5_PART1_LEN * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx5,
               ex5MaxPixels,
               ex5MaxPixels,
               ex5MaxPixels,
               g_partsEx5,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    Gauge_setTrailAnim(&g_gaugeEx5, 1, 0, 0);

    /* Step 5: Add Part1 (main 12-tile gauge) */
    Gauge_addPart(&g_gaugeEx5, &g_partsEx5[0],
                  &s_layoutEx5Part1,
                  EX5_X, EX5_Y);

    /* Debug: log dynamic VRAM tiles to detect overlaps */
    logDynamicVramTiles("Sample 5 part1 dyn tiles", &g_partsEx5[0]);

    vramSize = Gauge_getVramSize(&s_layoutEx5Part1, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 5 top part gauge", vramBase, vramSize);
    vramBase = (u16)(vramBase + vramSize);

    /* Step 6: Add Part2 (4-tile secondary display)
       - Shares the same gauge logic as Part1
       - Value changes are synchronized */
    Gauge_addPart(&g_gaugeEx5, &g_partsEx5[1],
                  &s_layoutEx5Part2,
                  EX5_PART2_X, EX5_PART2_Y);

    /* Debug: log dynamic VRAM tiles to detect overlaps */
    logDynamicVramTiles("Sample 5 part2 dyn tiles", &g_partsEx5[1]);

    vramSize = Gauge_getVramSize(&s_layoutEx5Part2, GAUGE_VRAM_DYNAMIC, 1);
    logVramUsage("Sample 5 bottom part gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* -------------------------------------------------------------------------
       EXAMPLE 5B: Independent blue gauge with auto-wrap
       ------------------------------------------------------------------------- */

    /* This gauge demonstrates:
       - Separate gauge with its own value
       - maxValue != maxFillPixels (uses internal LUT for mapping)
       - No trail animation */

    const u32 *blueTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_blue_strip_h_segment1.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 blueSegments[EX5_BLUE_LEN] = { 0, 0, 0, 0, 0, 0, 0, 0 };

    GaugeLayout_init(&s_layoutEx5Blue,
                     EX5_BLUE_LEN,
                     GAUGE_FILL_FORWARD,
                     blueTilesets,
                     blueSegments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);

    const u16 blueMaxPixels = (u16)(EX5_BLUE_LEN * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx5Blue,
               100,                      /* maxValue = 100 (percent) */
               blueMaxPixels,            /* maxFillPixels = 64 */
               0,                        /* initialValue = 0 (empty) */
               g_partsEx5Blue,
               vramBase,
               GAUGE_VRAM_DYNAMIC);

    /* No trail animation for blue gauge (disabled by default) */

    Gauge_addPart(&g_gaugeEx5Blue, &g_partsEx5Blue[0],
                  &s_layoutEx5Blue,
                  EX5_BLUE_X, EX5_BLUE_Y);

    vramSize = Gauge_getVramSize(&s_layoutEx5Blue, GAUGE_VRAM_DYNAMIC, 0);
    logVramUsage("Sample 5 mini-auto-incremental-blue gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw labels under gauges */
    VDP_drawText("Sample 5 Multi-Segment", EX5_X, EX5_PART2_Y + 1);
    VDP_drawText("    and  Multi-Part", EX5_X, EX5_PART2_Y + 2);
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

        Gauge_setValue(&g_gaugeEx5Blue, g_bluePercent);
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
    //Gauge_update(&g_gaugeEx5Blue);
    Gauge_update(&g_gaugeEx4);
    Gauge_update(&g_gaugeEx5);
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
    initExample2(&nextVram);    /* Ex2 (cap+border) */
    initExample3(&nextVram);    /* Ex3 (multi-bridge) */
    initExample4(&nextVram);    /* Ex4 (vertical, FIXED mode) */
    initExample5(&nextVram);    /* Ex5 (multi-segment + blue) */

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
