/* =============================================================================
   GAUGE MODULE - USAGE GUIDE
   =============================================================================

   This file demonstrates how to use the Gauge module for SGDK.
   Each sample is self-contained and shows a different use case.

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
      - Fill a GaugeInit and call Gauge_init(&gauge, &gaugeInit)
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
      - Gauge_increase(gauge, amount, holdFrames, blinkFrames) : gain trail if valueAnim enabled
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

/* Tile lengths for each sample */
#define EX1_LENGTH      12      /* Sample 1: 12-tile yellow gauge */
#define EX1_PIP_LENGTH  14      /* Sample 2: 7 pips, 2 tiles each */
#define EX2_LENGTH      16      /* Sample 3: 16 tiles cap+border */
#define EX3_LENGTH      16      /* Sample 5: 16 tiles multi-bridge */
#define EX4_LENGTH      12      /* Sample 6: 12 tiles vertical (3 segments) */
#define EX5_PART1_LEN   12      /* Sample 7 Part1: 12 tiles (3 segments) */
#define EX5_PART2_LEN    3      /* Sample 7 Part2: 3 tiles lightblue bevel */
#define EX5_BLUE_LEN     8      /* Sample 7 Blue: 8 tiles */
#define EX6_LENGTH      14      /* Sample 4: cap start/end + blue segment demo */

/* Screen positions (in tiles) */
#define EX1_X           2
#define EX1_Y           9
#define EX1_PIP_X       (EX1_X + EX1_LENGTH + 4)    /* Right of Ex1 with gap */
#define EX1_PIP_Y       EX1_Y                       /* Same row as Ex1 */

#define EX2_X           2
#define EX2_Y           14      /* Below Ex1 */

#define EX3_X           2
#define EX3_Y           18      /* Below Ex2 */

#define EX4_X           36      /* Right side of screen */
#define EX4_Y           8       /* Top, extends down to row 19 */

#define EX5_X           2
#define EX5_Y           23      /* Below Ex3 */
#define EX5_PART2_X     2
#define EX5_PART2_Y     (EX5_Y + 1)      /* Below Ex5 Part1 */
#define EX5_BLUE_X      (EX5_PART2_X + EX5_PART2_LEN)   /* Right of Part2 */
#define EX5_BLUE_Y      EX5_PART2_Y

#define EX6_X           (EX3_X + EX3_LENGTH + 4)     /* Right of Ex3 (same row) */
#define EX6_Y           EX3_Y

/* Number of gauges for selection */
#define GAUGE_COUNT     7


/* =============================================================================
   GLOBAL VARIABLES
   ============================================================================= */

/* --- Layouts (define visual appearance) --- */
static GaugeLayout s_layoutEx1;         /* Yellow 12 tiles h_bevel */
static GaugeLayout s_layoutEx1Pip;      /* 7-point PIP gauge (14 tiles) */
static GaugeLayout s_layoutEx2;         /* Yellow 16 tiles cap+border */
static GaugeLayout s_layoutEx3;         /* Multi-bridge (ciel -> blue -> yellow) */
static GaugeLayout s_layoutEx5Part1;    /* Multi-segment: lightblue(4) + blue(4) + yellow(4) */
static GaugeLayout s_layoutEx5Part2;    /* Lightblue bevel 3 tiles */
static GaugeLayout s_layoutEx5Blue;     /* Blue 8 tiles */
static GaugeLayout s_layoutEx4;         /* Multi-segment vertical */
static GaugeLayout s_layoutEx6;         /* Cap start/end demo */
static GaugeLayout s_layoutEx6Mirror;   /* Cap start/end demo (mirror) */

/* --- Gauges (manage value and logic) --- */
static Gauge g_gaugeEx1;
static Gauge g_gaugeEx1Pip;     /* Separate gauge (PIP mode) */
static Gauge g_gaugeEx2;
static Gauge g_gaugeEx3;
static Gauge g_gaugeEx4;
static Gauge g_gaugeEx5;        /* Has 2 parts: Part1 + Part2 */
static Gauge g_gaugeEx5Blue;    /* Independent blue gauge */
static Gauge g_gaugeEx6;

/* --- Parts arrays (visual instances) --- */
static GaugePart g_partsEx1[1];
static GaugePart g_partsEx1Pip[1];
static GaugePart g_partsEx2[1];
static GaugePart g_partsEx3[1];
static GaugePart g_partsEx4[1];
static GaugePart g_partsEx5[2];         /* Part1 + Part2 share same gauge */
static GaugePart g_partsEx5Blue[1];
static GaugePart g_partsEx6[1];

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
        case 1: return &g_gaugeEx1Pip;
        case 2: return &g_gaugeEx2;
        case 3: return &g_gaugeEx6;
        case 4: return &g_gaugeEx3;
        case 5: return &g_gaugeEx4;
        case 6: return &g_gaugeEx5;
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
        case 1: return "Sample 2 PIP (7 pts)  ";
        case 2: return "Sample 3 Cap+Border   ";
        case 3: return "Sample 4 Cap Start/End";
        case 4: return "Sample 5 Bridge       ";
        case 5: return "Sample 6 Vertical     ";
        case 6: return "Sample 7 Multi Segment";
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
   SAMPLE 1 + SAMPLE 2: Simple yellow gauge + PIP gauge (7 points)
   =============================================================================

   This sample shows the basic usage:
   - Single-segment gauge (all tiles use the same tileset)
   - Two separate gauges side by side (independent logic instances)
   - Trail animation enabled for damage effect
*/

static void initExample1(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* -------------------------------------------------------------------------
       SAMPLE 1A: Left yellow gauge
       ------------------------------------------------------------------------- */

    /* Step 1: Define the tileset array
       - Index 0 = h_bevel yellow fill strip (45 tiles)
       - Index 1 = NULL (unused segment slots) */
    const u32 *ex1Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_straight_yellow_strip.tiles,    /* Segment 0: yellow */
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
       - h_bevel orientation
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
    Gauge_init(&g_gaugeEx1, &(GaugeInit){
        .maxValue = ex1MaxPixels,
        .maxFillPixels = ex1MaxPixels,
        .initialValue = ex1MaxPixels,
        .parts = g_partsEx1,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    /* Step 6: Enable trail animation for damage effect */
    Gauge_setTrailAnim(&g_gaugeEx1, 1, 0, 0);   /* enabled, default speeds */
 
    /* Step 7: Add the visual part at screen position */
    Gauge_addPart(&g_gaugeEx1, &g_partsEx1[0],
                  &s_layoutEx1,
                  EX1_X, EX1_Y);

    /* Step 8: Log VRAM usage */
    vramSize = Gauge_getVramSize(&g_gaugeEx1, &s_layoutEx1);
    logVramUsage("Sample 1", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Step 9: Draw label under gauge */
    VDP_drawText("Sample 1", EX1_X, EX1_Y + 1);

    /* -------------------------------------------------------------------------
       SAMPLE 2: Right PIP gauge (7 value points, 2 tiles per point)
       ------------------------------------------------------------------------- */
    const u32 *ex1PipBodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_straight_yellow_strip.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex1PipCompactTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_pip_basic_strip.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 ex1PipWidths[GAUGE_MAX_SEGMENTS] = {
        2, /* segment 0: 2 tiles per pip */
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    };
    const u8 ex1PipSegments[EX1_PIP_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0
    };

    GaugeLayout_init(&s_layoutEx1Pip,
                     EX1_PIP_LENGTH,
                     GAUGE_FILL_FORWARD,
                     ex1PipBodyTilesets,
                     ex1PipSegments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0,
                     1,
                     0,
                     0);
    GaugeLayout_setPipStyles(&s_layoutEx1Pip, ex1PipCompactTilesets, ex1PipWidths);

    const u16 ex1PipMaxPixels = (u16)(EX1_PIP_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* maxValue=7 with 14 tiles in PIP mode => 2 tiles per value point */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx1Pip, &(GaugeInit){
        .maxValue = 7,
        .maxFillPixels = ex1PipMaxPixels,
        .initialValue = 7,
        .parts = g_partsEx1Pip,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_PIP
    });

    Gauge_setValueAnim(&g_gaugeEx1Pip, 1, 2);
    Gauge_setTrailAnim(&g_gaugeEx1Pip, 1, 0, 0);

    Gauge_addPart(&g_gaugeEx1Pip, &g_partsEx1Pip[0],
                  &s_layoutEx1Pip,
                  EX1_PIP_X, EX1_PIP_Y);

    vramSize = Gauge_getVramSize(&g_gaugeEx1Pip, &s_layoutEx1Pip);
    logVramUsage("Sample 2 (PIP)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 2 PIP", EX1_PIP_X, EX1_PIP_Y + 1);
}


/* =============================================================================
   SAMPLE 3: 16-tile gauge with cap+border
   ============================================================================= */

static void initExample2(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define BODY/END/TRAIL tilesets (yellow) */
    const u32 *ex2BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex2EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex2TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };

    /* Step 2: Define segments (all yellow) */
    const u8 ex2Segments[EX2_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 3: Initialize layout (BODY + END) */
    GaugeLayout_initEx(&s_layoutEx2,
                       EX2_LENGTH,
                       GAUGE_FILL_FORWARD,
                       ex2BodyTilesets,
                       ex2EndTilesets,
                       ex2TrailTilesets,
                       NULL,
                       ex2Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);

    /* Step 4: Initialize gauge (16px max) */
    const u16 ex2MaxPixels = (u16)(EX2_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx2, &(GaugeInit){
        .maxValue = ex2MaxPixels,
        .maxFillPixels = ex2MaxPixels,
        .initialValue = ex2MaxPixels,
        .parts = g_partsEx2,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeEx2, 1, 0, 0);

    /* Step 5: Add part */
    Gauge_addPart(&g_gaugeEx2, &g_partsEx2[0],
                  &s_layoutEx2,
                  EX2_X, EX2_Y);

    /* Step 6: Log VRAM */
    vramSize = Gauge_getVramSize(&g_gaugeEx2, &s_layoutEx2);
    logVramUsage("Sample 3 (Cap+Border)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 3", EX2_X, EX2_Y + 1);
}


/* =============================================================================
   SAMPLE 5: 16-tile gauge with multi-bridge segments
   ============================================================================= */

static void initExample3(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define segment styles (ciel -> blue -> yellow) */
    const GaugeSegmentStyle ex3SegmentStyles[GAUGE_MAX_SEGMENTS] = {
        /* Segment 0: lightblue */
        {
            .base = {
                .body = gauge_h_bevel_lightblue_strip_break.tiles,
                .end = gauge_h_bevel_lightblue_strip_end.tiles,
                .trail = gauge_h_bevel_lightblue_strip_trail.tiles,
                .bridge = gauge_h_bevel_lightblue_to_blue_strip_bridge.tiles
            },
            .gain = {
                .body = gauge_h_bevel_lightblue_gain_strip_break.tiles,
                .end = gauge_h_bevel_lightblue_gain_strip_end.tiles,
                .trail = gauge_h_bevel_lightblue_gain_strip_trail.tiles,
                .bridge = gauge_h_bevel_lightblue_gain_to_blue_strip_bridge.tiles
            }
        },
        /* Segment 1: blue */
        {
            .base = {
                .body = gauge_h_bevel_blue_strip_break.tiles,
                .end = gauge_h_bevel_blue_strip_end.tiles,
                .trail = gauge_h_bevel_blue_strip_trail.tiles,
                .bridge = gauge_h_bevel_blue_to_yellow_strip_bridge.tiles
            },
            .gain = {
                .body = gauge_h_bevel_blue_gain_strip_break.tiles,
                .end = gauge_h_bevel_blue_gain_strip_end.tiles,
                .trail = gauge_h_bevel_blue_gain_strip_trail.tiles,
                .bridge = gauge_h_bevel_blue_to_yellow_gain_strip_bridge.tiles
            }
        },
        /* Segment 2: yellow */
        {
            .base = {
                .body = gauge_h_bevel_yellow_strip_break.tiles,
                .end = gauge_h_bevel_yellow_strip_end.tiles,
                .trail = gauge_h_bevel_yellow_strip_trail.tiles
            },
            .gain = {
                .body = gauge_h_bevel_yellow_gain_strip_break.tiles,
                .end = gauge_h_bevel_yellow_gain_strip_end.tiles,
                .trail = gauge_h_bevel_yellow_gain_strip_trail.tiles
            }
        }
    };

    /* Step 2: Define segments (5 ciel + 5 blue + 6 yellow) */
    const u8 ex3Segments[EX3_LENGTH] = {
        0, 0, 0, 0, 0,
        1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2
    };

    /* Step 3: Initialize layout from init config */
    const GaugeLayoutInit ex3LayoutInit = {
        .length = EX3_LENGTH,
        .fillDir = GAUGE_FILL_FORWARD,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .paletteLine = PAL0,
        .priority = 1,
        .vflip = 0,
        .hflip = 0,
        .segmentIdByCell = ex3Segments,
        .segmentStyles = ex3SegmentStyles
    };
    GaugeLayout_build(&s_layoutEx3, &ex3LayoutInit);

    /* Step 4: Calculate dimensions */
    const u16 ex3MaxPixels = (u16)(EX3_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* Step 5: Initialize gauge */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx3, &(GaugeInit){
        .maxValue = 100,
        .maxFillPixels = ex3MaxPixels,
        .initialValue = ex3MaxPixels,
        .parts = g_partsEx3,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    /* Step 6: Enable BOTH value animation AND trail animation */
    Gauge_setValueAnim(&g_gaugeEx3, 1, 2);      /* enabled, default speed */
    Gauge_setTrailAnim(&g_gaugeEx3, 1, 3, 2);   /* enabled, default speeds */

    /* Step 7: Add part */
    Gauge_addPart(&g_gaugeEx3, &g_partsEx3[0],
                  &s_layoutEx3,
                  EX3_X, EX3_Y);

    /* Step 8: Log VRAM */
    vramSize = Gauge_getVramSize(&g_gaugeEx3, &s_layoutEx3);
    logVramUsage("Sample 5 (VRAM DYNAMIC)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 5", EX3_X, EX3_Y + 1);
    VDP_drawText("Bridge", EX3_X, EX3_Y + 2);
}

/* =============================================================================
   SAMPLE 6: Vertical multi-segment gauge with FIXED VRAM mode
   =============================================================================

   This sample shows:
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
        gauge_v_straight_blue_strip.tiles,        /* Segment 0: b1 vertical */
        gauge_v_straight_lightblue_strip.tiles,        /* Segment 1: b2 vertical */
        gauge_v_straight_yellow_strip.tiles,    /* Segment 2: yellow vertical */
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
    Gauge_init(&g_gaugeEx4, &(GaugeInit){
        .maxValue = ex4MaxPixels,
        .maxFillPixels = ex4MaxPixels,
        .initialValue = ex4MaxPixels,
        .parts = g_partsEx4,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_FIXED,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeEx4, 1, 0, 0);

    /* Step 5: Add part */
    Gauge_addPart(&g_gaugeEx4, &g_partsEx4[0],
                  &s_layoutEx4,
                  EX4_X, EX4_Y);

    /* Step 6: Log VRAM (FIXED mode uses more tiles) */
    vramSize = Gauge_getVramSize(&g_gaugeEx4, &s_layoutEx4);
    logVramUsage("Sample 6 (VRAM FIXED)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge (vertical gauge: label below bottom) */
    VDP_drawText("Sample 6", EX4_X-5, EX4_Y + EX4_LENGTH);
    VDP_drawText("Vertical", EX4_X-5, EX4_Y + EX4_LENGTH+1);
}


/* =============================================================================
   SAMPLE 7: Multi-segment bevel gauge with two parts + independent blue gauge
   =============================================================================

   This sample shows:
   - Bevel styles from Sample 5 bridge set (lightblue -> blue -> yellow)
   - Part1: 4 lightblue + 4 blue + 4 yellow
   - Part2: 4 lightblue
   - Two parts sharing the same gauge logic (synchronized value)
   - Independent blue gauge with auto-wrap demo
*/

static void initExample5(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* -------------------------------------------------------------------------
       SAMPLE 7A: Multi-segment bevel gauge (Part1 = 12 tiles, Part2 = 4 tiles)
       ------------------------------------------------------------------------- */

    /* Step 1: Define bevel segment styles (same family as Sample 5 bridge demo). */
    const GaugeSegmentStyle ex5SegmentStyles[GAUGE_MAX_SEGMENTS] = {
        /* Segment 0: lightblue */
        {
            .base = {
                .body = gauge_h_bevel_lightblue_strip_break.tiles,
                .end = gauge_h_bevel_lightblue_strip_end.tiles,
                .trail = gauge_h_bevel_lightblue_strip_trail.tiles,
                .bridge = gauge_h_bevel_lightblue_to_blue_strip_bridge.tiles
            },
            .gain = {
                .body = gauge_h_bevel_lightblue_gain_strip_break.tiles,
                .end = gauge_h_bevel_lightblue_gain_strip_end.tiles,
                .trail = gauge_h_bevel_lightblue_gain_strip_trail.tiles,
                .bridge = gauge_h_bevel_lightblue_gain_to_blue_strip_bridge.tiles
            }
        },
        /* Segment 1: blue */
        {
            .base = {
                .body = gauge_h_bevel_blue_strip_break.tiles,
                .end = gauge_h_bevel_blue_strip_end.tiles,
                .trail = gauge_h_bevel_blue_strip_trail.tiles,
                .bridge = gauge_h_bevel_blue_to_yellow_strip_bridge.tiles
            },
            .gain = {
                .body = gauge_h_bevel_blue_gain_strip_break.tiles,
                .end = gauge_h_bevel_blue_gain_strip_end.tiles,
                .trail = gauge_h_bevel_blue_gain_strip_trail.tiles,
                .bridge = gauge_h_bevel_blue_to_yellow_gain_strip_bridge.tiles
            }
        },
        /* Segment 2: yellow */
        {
            .base = {
                .body = gauge_h_bevel_yellow_strip_break.tiles,
                .end = gauge_h_bevel_yellow_strip_end.tiles,
                .trail = gauge_h_bevel_yellow_strip_trail.tiles
            },
            .gain = {
                .body = gauge_h_bevel_yellow_gain_strip_break.tiles,
                .end = gauge_h_bevel_yellow_gain_strip_end.tiles,
                .trail = gauge_h_bevel_yellow_gain_strip_trail.tiles
            }
        }
    };

    /* Step 2: Define segment assignments for Part1 (12 tiles).
       4 lightblue + 4 blue + 4 yellow. */
    const u8 ex5Part1Segments[EX5_PART1_LEN] = {
        0, 0, 0, 0,
        1, 1, 1, 1,
        2, 2, 2, 2
    };
    const GaugeLayoutInit ex5Part1LayoutInit = {
        .length = EX5_PART1_LEN,
        .fillDir = GAUGE_FILL_FORWARD,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .paletteLine = PAL0,
        .priority = 1,
        .vflip = 0,
        .hflip = 0,
        .segmentIdByCell = ex5Part1Segments,
        .segmentStyles = ex5SegmentStyles
    };
    GaugeLayout_build(&s_layoutEx5Part1, &ex5Part1LayoutInit);

    /* Step 3: Define layout for Part2 (3 tiles lightblue bevel with END). */
    const u32 *ex5Part2BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5Part2EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5Part2TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5Part2GainBodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_gain_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5Part2GainEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_gain_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex5Part2GainTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_gain_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 ex5Part2Segments[EX5_PART2_LEN] = { 0, 0, 0 };

    GaugeLayout_initEx(&s_layoutEx5Part2,
                       EX5_PART2_LEN,
                       GAUGE_FILL_FORWARD,
                       ex5Part2BodyTilesets,
                       ex5Part2EndTilesets,
                       ex5Part2TrailTilesets,
                       NULL,
                       ex5Part2Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);
    /* Render only the tail window of the shared gauge (last 3 cells). */
    s_layoutEx5Part2.fillOffset = (u16)((EX5_PART1_LEN - EX5_PART2_LEN) * GAUGE_PIXELS_PER_TILE);

    GaugeLayout_setGainTrail(&s_layoutEx5Part2,
                             ex5Part2GainBodyTilesets,
                             ex5Part2GainEndTilesets,
                             ex5Part2GainTrailTilesets,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL);

    /* Step 4: Initialize gauge (max pixels = Part1 only)
       - maxValue = Part1 pixels (12 tiles = 96 pixels)
       - Part2 will show the "overflow" visually */
    const u16 ex5MaxPixels = (u16)(EX5_PART1_LEN * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx5, &(GaugeInit){
        .maxValue = ex5MaxPixels,
        .maxFillPixels = ex5MaxPixels,
        .initialValue = ex5MaxPixels,
        .parts = g_partsEx5,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeEx5, 1, 0, 0);

    /* Step 5: Add Part1 (main 12-tile gauge) */
    Gauge_addPart(&g_gaugeEx5, &g_partsEx5[0],
                  &s_layoutEx5Part1,
                  EX5_X, EX5_Y);

    /* Debug: log dynamic VRAM tiles to detect overlaps */
    logDynamicVramTiles("Sample 7 part1 dyn tiles", &g_partsEx5[0]);
    vramSize = Gauge_getVramSize(&g_gaugeEx5, &s_layoutEx5Part1);
    logVramUsage("Sample 7 top part gauge", vramBase, vramSize);
    vramBase = (u16)(vramBase + vramSize);

    /* Step 6: Add Part2 (3-tile secondary display)
       - Shares the same gauge logic as Part1
       - Value changes are synchronized */
    Gauge_addPart(&g_gaugeEx5, &g_partsEx5[1],
                  &s_layoutEx5Part2,
                  EX5_PART2_X, EX5_PART2_Y);

    /* Debug: log dynamic VRAM tiles to detect overlaps */
    logDynamicVramTiles("Sample 7 part2 dyn tiles", &g_partsEx5[1]);
    vramSize = Gauge_getVramSize(&g_gaugeEx5, &s_layoutEx5Part2);
    logVramUsage("Sample 7 bottom part gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* -------------------------------------------------------------------------
       SAMPLE 7B: Independent blue gauge with auto-wrap
       ------------------------------------------------------------------------- */

    /* This gauge demonstrates:
       - Separate gauge with its own value
       - maxValue != maxFillPixels (uses internal LUT for mapping)
       - No trail animation */

    const u32 *blueTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_straight_small_blue_strip.tiles,
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
    Gauge_init(&g_gaugeEx5Blue, &(GaugeInit){
        .maxValue = 100,
        .maxFillPixels = blueMaxPixels,
        .initialValue = 0,
        .parts = g_partsEx5Blue,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    /* No trail animation for blue gauge (disabled by default) */

    Gauge_addPart(&g_gaugeEx5Blue, &g_partsEx5Blue[0],
                  &s_layoutEx5Blue,
                  EX5_BLUE_X, EX5_BLUE_Y);

    vramSize = Gauge_getVramSize(&g_gaugeEx5Blue, &s_layoutEx5Blue);
    logVramUsage("Sample 7 mini-auto-incremental-blue gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw labels under gauges */
    VDP_drawText("Sample 7 Multi-Segment", EX5_X, EX5_PART2_Y + 1);
    VDP_drawText("    and  Multi-Part", EX5_X, EX5_PART2_Y + 2);
}

/* =============================================================================
   SAMPLE 4: Cap start/end guide (single segment)

   Goals:
   - Demonstrate CAP START (cell 0) and CAP END (last cell) behavior.
   - CAP START behaves like a classic cell:
       * If this cell is END -> cap tileset + END LUT
       * If this cell is TRAIL_BREAK/TRAIL_BREAK2/TRAIL_FULL -> cap_trail tileset + trail LUT
       * If next cell is END -> cap_break tileset + END index
       * Else -> cap_break tileset + index 44 (full)
   - CAP END always uses its own tileset (cap_end) and follows the END LUT.

   Assets used:
   - yellow cap start:     gauge_h_bevel_yellow_with_border_cap_start_strip_end
   - yellow cap start break: gauge_h_bevel_yellow_with_border_cap_start_strip_break
   - yellow cap start trail: gauge_h_bevel_yellow_with_border_cap_start_strip_trail
   - yellow end:           gauge_h_bevel_yellow_with_border_strip_end
   - yellow cap end:       gauge_h_bevel_yellow_with_border_cap_end_strip_end
   - yellow body/trail:    gauge_h_bevel_yellow_with_border_strip_break / trail
   - blue body/trail:      gauge_h_bevel_blue_with_border_strip_break / trail
   - blue end:             gauge_h_bevel_blue_with_border_strip_end
   - bridges:              gauge_h_bevel_yellow_to_blue_with_border_strip_bridge
                           gauge_h_bevel_blue_to_yellow_with_border_strip_bridge
   - yellow blink off cap start:  gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end
   - yellow blink off cap end:    gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end
   - yellow blink off cap start trail:  gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail
   - yellow blink off cap start break:  gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_break
   - yellow blink off end:        gauge_h_bevel_yellow_with_border_blink_off_strip_end
   - yellow blink off break:      gauge_h_bevel_yellow_with_border_blink_off_strip_break
   - yellow blink off trail:      gauge_h_bevel_yellow_with_border_blink_off_strip_trail
   - blue blink off body/trail:   gauge_h_bevel_blue_with_border_blink_off_strip_break / trail
   - blue blink off end:          gauge_h_bevel_blue_with_border_blink_off_strip_end
   - blink off bridges:           gauge_h_bevel_yellow_to_blue_with_border_blink_off_strip_bridge
                           gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge
   ============================================================================= */

static void initExample6Layout(void)
{
    /* Step 1: Define tilesets (cap style) */
    const u32 *ex6BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_strip_break.tiles,
        gauge_h_bevel_blue_with_border_strip_break.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_strip_end.tiles,
        gauge_h_bevel_blue_with_border_strip_end.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_strip_trail.tiles,
        gauge_h_bevel_blue_with_border_strip_trail.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BridgeTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_to_blue_with_border_strip_bridge.tiles,
        gauge_h_bevel_blue_to_yellow_with_border_strip_bridge.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6CapStartTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6CapEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_end_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6CapStartBreakTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6CapStartTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffBodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_blink_off_strip_break.tiles,
        gauge_h_bevel_blue_with_border_blink_off_strip_break.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_blink_off_strip_end.tiles,
        gauge_h_bevel_blue_with_border_blink_off_strip_end.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_blink_off_strip_trail.tiles,
        gauge_h_bevel_blue_with_border_blink_off_strip_trail.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffBridgeTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_to_blue_with_border_blink_off_strip_bridge.tiles,
        gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffCapStartTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffCapEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffCapStartBreakTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *ex6BlinkOffCapStartTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 ex6CapEndBySegment[GAUGE_MAX_SEGMENTS] = {
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 2: Define segments (all cap style) */
    const u8 ex6Segments[EX6_LENGTH] = {
        0, 0, 0, 0,
        1, 1, 1, 1, 1, 1,
        0, 0, 0, 0
    };

    /* Step 3: Initialize layout */
    GaugeLayout_initEx(&s_layoutEx6,
                       EX6_LENGTH,
                       GAUGE_FILL_FORWARD,
                       ex6BodyTilesets,
                       ex6EndTilesets,
                       ex6TrailTilesets,
                       ex6BridgeTilesets,
                       ex6Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);

    /* Step 4: Configure caps (start tileset + end flag)
       Cap behavior summary:
       - Cap start behaves like a classic cell (END, TRAIL, BREAK or FULL)
       - Cap end always uses its own tileset and follows the END LUT */
    GaugeLayout_setCaps(&s_layoutEx6,
                        ex6CapStartTilesets,
                        ex6CapEndTilesets,
                        ex6CapStartBreakTilesets,
                        ex6CapStartTrailTilesets,
                        ex6CapEndBySegment);

    GaugeLayout_setBlinkOff(&s_layoutEx6,
                            ex6BlinkOffBodyTilesets,
                            ex6BlinkOffEndTilesets,
                            ex6BlinkOffTrailTilesets,
                            ex6BlinkOffBridgeTilesets,
                            ex6BlinkOffCapStartTilesets,
                            ex6BlinkOffCapEndTilesets,
                            ex6BlinkOffCapStartBreakTilesets,
                            ex6BlinkOffCapStartTrailTilesets);

    GaugeLayout_makeMirror(&s_layoutEx6Mirror, &s_layoutEx6);
}

static void initExample6(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    initExample6Layout();

    /* Step 5: Initialize gauge */
    const u16 ex6MaxPixels = (u16)(EX6_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeEx6, &(GaugeInit){
        .maxValue = ex6MaxPixels,
        .maxFillPixels = ex6MaxPixels,
        .initialValue = ex6MaxPixels,
        .parts = g_partsEx6,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeEx6, 1, 0, 0);

    /* Step 6: Add part */
    Gauge_addPart(&g_gaugeEx6, &g_partsEx6[0],
                  &s_layoutEx6Mirror,
                  EX6_X, EX6_Y);

    /* Step 7: Log VRAM */
    vramSize = Gauge_getVramSize(&g_gaugeEx6, &s_layoutEx6Mirror);
    logVramUsage("Sample 4 (Cap Start/End Mirror)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 4 Mirror", EX6_X, EX6_Y + 1);
    VDP_drawText("Cap Start/End", EX6_X, EX6_Y + 2);
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
        Gauge *selectedGauge = getSelectedGauge();
        const u16 increaseAmount = (g_selectedGauge == 1) ? 1 : 4; /* Sample 2 PIP only */
        Gauge_increase(selectedGauge, increaseAmount, 40, 20);
        g_holdA = 0;
    }

    /* Button B: decrease with trail effect (on press) */
    if (pressed & BUTTON_B)
    {
        Gauge *selectedGauge = getSelectedGauge();
        const u16 decreaseAmount = (g_selectedGauge == 1) ? 1 : 4; /* Sample 2 PIP only */
        Gauge_decrease(selectedGauge, decreaseAmount, 20, 60);
        g_holdB = 0;
    }

    /* Hold repeat for A and B */
    if (held & BUTTON_A)
    {
        g_holdA++;
        if (g_holdA >= 12 && (g_frameCount & 3) == 0)
        {
            Gauge_increase(getSelectedGauge(), 1, 0, 0);
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
    Gauge_update(&g_gaugeEx1Pip);
    Gauge_update(&g_gaugeEx2);
    Gauge_update(&g_gaugeEx6);
    Gauge_update(&g_gaugeEx3);
    Gauge_update(&g_gaugeEx4);
    Gauge_update(&g_gaugeEx5);
    Gauge_update(&g_gaugeEx5Blue);
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
       Initialize all samples
       Each sample is self-contained and uses the next available VRAM slot
       ------------------------------------------------------------------------- */
    u16 nextVram = VRAM_BASE;

    KLog("=== GAUGE MODULE DEMO - VRAM ALLOCATION ===");

    initExample1(&nextVram);    /* Sample 1 + Sample 2 PIP */
    initExample2(&nextVram);    /* Sample 3 (cap+border) */
    initExample6(&nextVram);    /* Sample 4 (cap start/end) */
    initExample3(&nextVram);    /* Sample 5 (multi-bridge) */
    initExample4(&nextVram);    /* Sample 6 (vertical, FIXED mode) */
    initExample5(&nextVram);    /* Sample 7 (multi-segment + blue) */

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
        //tickBlueAutoWrap();

        /* Update all gauges (tick + render) */
        updateAllGauges();

        /* Wait for VBlank and process DMA queue */
        SYS_doVBlankProcess();
    }

    return 0;
}
