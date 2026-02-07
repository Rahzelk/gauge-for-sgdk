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
#define SAMPLE1_LENGTH      12      /* Sample 1: 12-tile yellow gauge */
#define SAMPLE2_LENGTH  14      /* Sample 2: 7 pips, 2 tiles each */
#define SAMPLE3_LENGTH      15      /* Sample 3: 15 tiles cap+border */
#define SAMPLE5_LENGTH      16      /* Sample 5: 16 tiles multi-bridge */
#define SAMPLE6_LENGTH      12      /* Sample 6: 12 tiles vertical (3 segments) */
#define SAMPLE7_PART1_LEN   12      /* Sample 7 Part1: 12 tiles (3 segments) */
#define SAMPLE7_PART2_LEN    3      /* Sample 7 Part2: 3 tiles lightblue bevel */
#define SAMPLE7_PIP_LEN      8      /* Sample 7 mini PIP: 4 pips, 2 tiles each */
#define SAMPLE4_LENGTH      14      /* Sample 4: cap start/end + blue segment demo */

/* Screen positions (in tiles) */
#define SAMPLE1_X           2
#define SAMPLE1_Y           9
#define SAMPLE2_X       (SAMPLE1_X + SAMPLE1_LENGTH + 4)    /* Right of Sample1 with gap */
#define SAMPLE2_Y       SAMPLE1_Y                       /* Same row as Sample1 */

#define SAMPLE3_X           2
#define SAMPLE3_Y           14      /* Below Sample1 */

#define SAMPLE5_X           2
#define SAMPLE5_Y           18      /* Below Sample3 */

#define SAMPLE6_X           36      /* Right side of screen */
#define SAMPLE6_Y           10      /* Top, extends down to row 21 */

#define SAMPLE7_X           2
#define SAMPLE7_Y           23      /* Below Sample5 */
#define SAMPLE7_PART2_X     2
#define SAMPLE7_PART2_Y     (SAMPLE7_Y + 1)      /* Below Sample7 Part1 */
#define SAMPLE7_PIP_X       (SAMPLE7_PART2_X + SAMPLE7_PART2_LEN)   /* Right of Part2 */
#define SAMPLE7_PIP_Y       SAMPLE7_PART2_Y

#define SAMPLE4_X           (SAMPLE3_X + SAMPLE3_LENGTH + 3)     /* Right of Sample3, shifted 2 tiles left */
#define SAMPLE4_Y           SAMPLE3_Y

/* Number of gauges for selection */
#define GAUGE_COUNT     7


/* =============================================================================
   GLOBAL VARIABLES
   ============================================================================= */

/* --- Layouts (define visual appearance) --- */
static GaugeLayout s_layoutSample1;         /* Yellow 12 tiles h_bevel */
static GaugeLayout s_layoutSample2;      /* 7-point PIP gauge (14 tiles) */
static GaugeLayout s_layoutSample3;         /* Yellow 15 tiles cap+border */
static GaugeLayout s_layoutSample5;         /* Multi-bridge (ciel -> blue -> yellow) */
static GaugeLayout s_layoutSample7Part1;    /* Multi-segment: lightblue(4) + blue(4) + yellow(4) */
static GaugeLayout s_layoutSample7Part2;    /* Lightblue bevel 3 tiles */
static GaugeLayout s_layoutSample7Pip;      /* Mini PIP 4 points */
static GaugeLayout s_layoutSample6;         /* Multi-segment vertical */
static GaugeLayout s_layoutSample4;         /* Cap start/end demo */
static GaugeLayout s_layoutSample4Mirror;   /* Cap start/end demo (mirror) */

/* --- Gauges (manage value and logic) --- */
static Gauge g_gaugeSample1;
static Gauge g_gaugeSample2;     /* Separate gauge (PIP mode) */
static Gauge g_gaugeSample3;
static Gauge g_gaugeSample5;
static Gauge g_gaugeSample6;
static Gauge g_gaugeSample7;        /* Has 2 parts: Part1 + Part2 */
static Gauge g_gaugeSample7Pip;     /* Independent mini PIP gauge */
static Gauge g_gaugeSample4;

/* --- Parts arrays (visual instances) --- */
static GaugePart g_partsSample1[1];
static GaugePart g_partsSample2[1];
static GaugePart g_partsSample3[1];
static GaugePart g_partsSample5[1];
static GaugePart g_partsSample6[1];
static GaugePart g_partsSample7[2];         /* Part1 + Part2 share same gauge */
static GaugePart g_partsSample7Pip[1];
static GaugePart g_partsSample4[1];

/* --- Input state --- */
static u16 g_prevPad = 0;
static u8 g_selectedGauge = 0;          /* Currently controlled gauge (0-6) */
static u8 g_holdA = 0;                  /* Hold counter for button A */
static u8 g_holdB = 0;                  /* Hold counter for button B */

/* --- Sample 7 mini PIP auto-wrap state --- */
static u16 g_sample7PipValue = 0;       /* 0-4 */
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
        case 0: return &g_gaugeSample1;
        case 1: return &g_gaugeSample2;
        case 2: return &g_gaugeSample3;
        case 3: return &g_gaugeSample4;
        case 4: return &g_gaugeSample5;
        case 5: return &g_gaugeSample6;
        case 6: return &g_gaugeSample7;
        default: return &g_gaugeSample1;
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

static void initSample1(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* -------------------------------------------------------------------------
       SAMPLE 1A: Left yellow gauge
       ------------------------------------------------------------------------- */

    /* Step 1: Define the tileset array
       - Index 0 = h_bevel yellow fill strip (45 tiles)
       - Index 1 = NULL (unused segment slots) */
    const u32 *sample1Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_straight_yellow_strip.tiles,    /* Segment 0: yellow */
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };

    /* Step 2: Define which segment each cell uses
       - All 12 cells use segment 0 (yellow) */
    const u8 sample1Segments[SAMPLE1_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 3: Initialize the layout
       - 12 tiles long
       - Fill direction: forward (left to right)
       - h_bevel orientation
       - Palette 0, priority 1, no flip */
    GaugeLayout_init(&s_layoutSample1,
                     SAMPLE1_LENGTH,                 /* length in tiles */
                     GAUGE_FILL_FORWARD,         /* fill direction */
                     sample1Tilesets,                /* tilesets array */
                     sample1Segments,                /* segment per cell */
                     GAUGE_ORIENT_HORIZONTAL,    /* orientation */
                     PAL0,                       /* palette line */
                     1,                          /* priority (1=high) */
                     0,                          /* verticalFlip */
                     0);                         /* horizontalFlip */

    /* Step 4: Calculate pixel dimensions
       - maxValue = maxFillPixels for 1:1 mapping (no LUT needed) */
    const u16 sample1MaxPixels = (u16)(SAMPLE1_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* Step 5: Initialize the gauge
       - Starts at full value
       - Uses VRAM_DYNAMIC mode (fewer tiles, more CPU efficient for single gauge) */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample1, &(GaugeInit){
        .maxValue = sample1MaxPixels,
        .maxFillPixels = sample1MaxPixels,
        .initialValue = sample1MaxPixels,
        .parts = g_partsSample1,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    /* Step 6: Enable trail animation for damage effect */
    Gauge_setTrailAnim(&g_gaugeSample1, 1, 0, 0);   /* enabled, default speeds */
 
    /* Step 7: Add the visual part at screen position */
    Gauge_addPart(&g_gaugeSample1, &g_partsSample1[0],
                  &s_layoutSample1,
                  SAMPLE1_X, SAMPLE1_Y);

    /* Step 8: Log VRAM usage */
    vramSize = Gauge_getVramSize(&g_gaugeSample1, &s_layoutSample1);
    logVramUsage("Sample 1", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Step 9: Draw label under gauge */
    VDP_drawText("Sample 1", SAMPLE1_X, SAMPLE1_Y + 1);

    /* -------------------------------------------------------------------------
       SAMPLE 2: Right PIP gauge (7 value points, 2 tiles per point)
       ------------------------------------------------------------------------- */
    const u32 *sample2BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_straight_yellow_strip.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample2CompactTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_pip_basic_strip.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 sample2Widths[GAUGE_MAX_SEGMENTS] = {
        2, /* segment 0: 2 tiles per pip */
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    };
    const u8 sample2Segments[SAMPLE2_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0
    };

    GaugeLayout_init(&s_layoutSample2,
                     SAMPLE2_LENGTH,
                     GAUGE_FILL_FORWARD,
                     sample2BodyTilesets,
                     sample2Segments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0,
                     1,
                     0,
                     0);
    GaugeLayout_setPipStyles(&s_layoutSample2, sample2CompactTilesets, sample2Widths);

    const u16 sample2MaxPixels = (u16)(SAMPLE2_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* maxValue=7 with 14 tiles in PIP mode => 2 tiles per value point */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample2, &(GaugeInit){
        .maxValue = 7,
        .maxFillPixels = sample2MaxPixels,
        .initialValue = 7,
        .parts = g_partsSample2,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_PIP
    });

    Gauge_setValueAnim(&g_gaugeSample2, 1, 2);
    Gauge_setTrailAnim(&g_gaugeSample2, 1, 0, 0);

    Gauge_addPart(&g_gaugeSample2, &g_partsSample2[0],
                  &s_layoutSample2,
                  SAMPLE2_X, SAMPLE2_Y);

    vramSize = Gauge_getVramSize(&g_gaugeSample2, &s_layoutSample2);
    logVramUsage("Sample 2 (PIP)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 2 PIP", SAMPLE2_X, SAMPLE2_Y + 1);
}


/* =============================================================================
   SAMPLE 3: 16-tile gauge with cap+border
   ============================================================================= */

static void initSample3(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define BODY/END/TRAIL tilesets (yellow) */
    const u32 *sample3BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample3EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample3TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };

    /* Step 2: Define segments (all yellow) */
    const u8 sample3Segments[SAMPLE3_LENGTH] = {
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0
    };

    /* Step 3: Initialize layout (BODY + END) */
    GaugeLayout_initEx(&s_layoutSample3,
                       SAMPLE3_LENGTH,
                       GAUGE_FILL_FORWARD,
                       sample3BodyTilesets,
                       sample3EndTilesets,
                       sample3TrailTilesets,
                       NULL,
                       sample3Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);

    /* Step 4: Initialize gauge (15 tiles max) */
    const u16 sample3MaxPixels = (u16)(SAMPLE3_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample3, &(GaugeInit){
        .maxValue = sample3MaxPixels,
        .maxFillPixels = sample3MaxPixels,
        .initialValue = sample3MaxPixels,
        .parts = g_partsSample3,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeSample3, 1, 0, 0);

    /* Step 5: Add part */
    Gauge_addPart(&g_gaugeSample3, &g_partsSample3[0],
                  &s_layoutSample3,
                  SAMPLE3_X, SAMPLE3_Y);

    /* Step 6: Log VRAM */
    vramSize = Gauge_getVramSize(&g_gaugeSample3, &s_layoutSample3);
    logVramUsage("Sample 3 (Cap+Border)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 3", SAMPLE3_X, SAMPLE3_Y + 1);
}


/* =============================================================================
   SAMPLE 5: 16-tile gauge with multi-bridge segments
   ============================================================================= */

static void initSample5(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define segment styles (ciel -> blue -> yellow) */
    const GaugeSegmentStyle sample5SegmentStyles[GAUGE_MAX_SEGMENTS] = {
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
    const u8 sample5Segments[SAMPLE5_LENGTH] = {
        0, 0, 0, 0, 0,
        1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2
    };

    /* Step 3: Initialize layout from init config */
    const GaugeLayoutInit sample5LayoutInit = {
        .length = SAMPLE5_LENGTH,
        .fillDirection = GAUGE_FILL_FORWARD,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0,
        .segmentIdByCell = sample5Segments,
        .segmentStyles = sample5SegmentStyles
    };
    GaugeLayout_build(&s_layoutSample5, &sample5LayoutInit);

    /* Step 4: Calculate dimensions */
    const u16 sample5MaxPixels = (u16)(SAMPLE5_LENGTH * GAUGE_PIXELS_PER_TILE);

    /* Step 5: Initialize gauge */
    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample5, &(GaugeInit){
        .maxValue = 100,
        .maxFillPixels = sample5MaxPixels,
        .initialValue = sample5MaxPixels,
        .parts = g_partsSample5,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    /* Step 6: Enable BOTH value animation AND trail animation */
    Gauge_setValueAnim(&g_gaugeSample5, 1, 2);      /* enabled, default speed */
    Gauge_setTrailAnim(&g_gaugeSample5, 1, 3, 2);   /* enabled, default speeds */

    /* Step 7: Add part */
    Gauge_addPart(&g_gaugeSample5, &g_partsSample5[0],
                  &s_layoutSample5,
                  SAMPLE5_X, SAMPLE5_Y);

    /* Step 8: Log VRAM */
    vramSize = Gauge_getVramSize(&g_gaugeSample5, &s_layoutSample5);
    logVramUsage("Sample 5 (VRAM DYNAMIC)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 5", SAMPLE5_X, SAMPLE5_Y + 1);
    VDP_drawText("Bridge", SAMPLE5_X, SAMPLE5_Y + 2);
}

/* =============================================================================
   SAMPLE 6: Vertical multi-segment gauge with FIXED VRAM mode
   =============================================================================

   This sample shows:
   - Vertical orientation
   - Same multi-segment layout as Sample7 (b1 + b2 + yellow)
   - GAUGE_VRAM_FIXED mode (more VRAM, less CPU)
   - Reverse fill direction (bottom to top)
*/

static void initSample6(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* Step 1: Define tilesets (same 3 segments as Sample7, but vertical versions) */
    const u32 *sample6Tilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_v_straight_blue_strip.tiles,        /* Segment 0: b1 vertical */
        gauge_v_straight_lightblue_strip.tiles,        /* Segment 1: b2 vertical */
        gauge_v_straight_yellow_strip.tiles,    /* Segment 2: yellow vertical */
        NULL
    };

    /* Step 2: Define segments (same pattern as Sample7 Part1) */
    const u8 sample6Segments[SAMPLE6_LENGTH] = {
        0, 0, 0,                    /* 3 tiles b1 */
        1, 1, 1, 1, 1,              /* 5 tiles b2 */
        2, 2, 2, 2                  /* 4 tiles yellow */
    };

    /* Step 3: Initialize layout
       - Vertical orientation
       - Reverse fill (bottom to top, typical for vertical gauges) */
    GaugeLayout_init(&s_layoutSample6,
                     SAMPLE6_LENGTH,
                     GAUGE_FILL_REVERSE,         /* Fill from bottom to top */
                     sample6Tilesets,
                     sample6Segments,
                     GAUGE_ORIENT_VERTICAL,      /* Vertical */
                     PAL0, 1, 0, 0);

    /* Step 4: Initialize gauge with FIXED VRAM mode
       - FIXED mode: allocates 1 VRAM tile per cell (12 tiles)
       - More VRAM usage, but less CPU per frame
       - Good for many gauges or when CPU is tight */
    const u16 sample6MaxPixels = (u16)(SAMPLE6_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample6, &(GaugeInit){
        .maxValue = sample6MaxPixels,
        .maxFillPixels = sample6MaxPixels,
        .initialValue = sample6MaxPixels,
        .parts = g_partsSample6,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_FIXED,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeSample6, 1, 0, 0);

    /* Step 5: Add part */
    Gauge_addPart(&g_gaugeSample6, &g_partsSample6[0],
                  &s_layoutSample6,
                  SAMPLE6_X, SAMPLE6_Y);

    /* Step 6: Log VRAM (FIXED mode uses more tiles) */
    vramSize = Gauge_getVramSize(&g_gaugeSample6, &s_layoutSample6);
    logVramUsage("Sample 6 (VRAM FIXED)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge (vertical gauge: label below bottom) */
    VDP_drawText("Sample 6", SAMPLE6_X-5, SAMPLE6_Y + SAMPLE6_LENGTH);
    VDP_drawText("Vertical", SAMPLE6_X-5, SAMPLE6_Y + SAMPLE6_LENGTH+1);
}


/* =============================================================================
   SAMPLE 7: Multi-segment bevel gauge with two parts + independent mini PIP
   =============================================================================

   This sample shows:
   - Bevel styles from Sample 5 bridge set (lightblue -> blue -> yellow)
   - Part1: 4 lightblue + 4 blue + 4 yellow
   - Part2: 4 lightblue
   - Two parts sharing the same gauge logic (synchronized value)
   - Independent mini PIP gauge with auto-wrap demo
*/

static void initSample7(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    /* -------------------------------------------------------------------------
       SAMPLE 7A: Multi-segment bevel gauge (Part1 = 12 tiles, Part2 = 4 tiles)
       ------------------------------------------------------------------------- */

    /* Step 1: Define bevel segment styles (same family as Sample 5 bridge demo). */
    const GaugeSegmentStyle sample7SegmentStyles[GAUGE_MAX_SEGMENTS] = {
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
    const u8 sample7Part1Segments[SAMPLE7_PART1_LEN] = {
        0, 0, 0, 0,
        1, 1, 1, 1,
        2, 2, 2, 2
    };
    const GaugeLayoutInit sample7Part1LayoutInit = {
        .length = SAMPLE7_PART1_LEN,
        .fillDirection = GAUGE_FILL_FORWARD,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0,
        .segmentIdByCell = sample7Part1Segments,
        .segmentStyles = sample7SegmentStyles
    };
    GaugeLayout_build(&s_layoutSample7Part1, &sample7Part1LayoutInit);

    /* Step 3: Define layout for Part2 (3 tiles lightblue bevel with END). */
    const u32 *sample7Part2BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample7Part2EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample7Part2TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample7Part2GainBodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_gain_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample7Part2GainEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_gain_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample7Part2GainTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_lightblue_gain_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 sample7Part2Segments[SAMPLE7_PART2_LEN] = { 0, 0, 0 };

    GaugeLayout_initEx(&s_layoutSample7Part2,
                       SAMPLE7_PART2_LEN,
                       GAUGE_FILL_FORWARD,
                       sample7Part2BodyTilesets,
                       sample7Part2EndTilesets,
                       sample7Part2TrailTilesets,
                       NULL,
                       sample7Part2Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);
    /* Visual trigger tweak for bevel part: start reacting around valuePx ~= 32.
     * Formula: fillOffset = triggerPx - partWidthPx => 32 - (3*8) = 8. */
    GaugeLayout_setFillOffset(&s_layoutSample7Part2, 8);
    GaugeLayout_setGainTrail(&s_layoutSample7Part2,
                             sample7Part2GainBodyTilesets,
                             sample7Part2GainEndTilesets,
                             sample7Part2GainTrailTilesets,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL);

    /* Step 4: Initialize gauge (max pixels = Part1 only)
       - maxValue = Part1 pixels (12 tiles = 96 pixels)
       - Part2 will show the "overflow" visually */
    const u16 sample7MaxPixels = (u16)(SAMPLE7_PART1_LEN * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample7, &(GaugeInit){
        .maxValue = sample7MaxPixels,
        .maxFillPixels = sample7MaxPixels,
        .initialValue = sample7MaxPixels,
        .parts = g_partsSample7,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeSample7, 1, 0, 0);

    /* Step 5: Add Part1 (main 12-tile gauge) */
    Gauge_addPart(&g_gaugeSample7, &g_partsSample7[0],
                  &s_layoutSample7Part1,
                  SAMPLE7_X, SAMPLE7_Y);

    /* Debug: log dynamic VRAM tiles to detect overlaps */
    logDynamicVramTiles("Sample 7 part1 dyn tiles", &g_partsSample7[0]);
    vramSize = Gauge_getVramSize(&g_gaugeSample7, &s_layoutSample7Part1);
    logVramUsage("Sample 7 top part gauge", vramBase, vramSize);
    vramBase = (u16)(vramBase + vramSize);

    /* Step 6: Add Part2 (3-tile secondary display)
       - Shares the same gauge logic as Part1
       - Value changes are synchronized */
    Gauge_addPart(&g_gaugeSample7, &g_partsSample7[1],
                  &s_layoutSample7Part2,
                  SAMPLE7_PART2_X, SAMPLE7_PART2_Y);

    /* Debug: log dynamic VRAM tiles to detect overlaps */
    logDynamicVramTiles("Sample 7 part2 dyn tiles", &g_partsSample7[1]);
    vramSize = Gauge_getVramSize(&g_gaugeSample7, &s_layoutSample7Part2);
    logVramUsage("Sample 7 bottom part gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* -------------------------------------------------------------------------
       SAMPLE 7B: Independent mini PIP gauge (4 points)
       ------------------------------------------------------------------------- */

    /* This gauge reuses Sample 2 PIP definition, but compacted to 4 points. */
    const u32 *sample7PipBodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_straight_yellow_strip.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample7PipCompactTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_pip_mini_bar_strip.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 sample7PipWidths[GAUGE_MAX_SEGMENTS] = {
        2, /* segment 0: 2 tiles per pip */
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    };
    const u8 sample7PipSegments[SAMPLE7_PIP_LEN] = { 0, 0, 0, 0, 0, 0, 0, 0 };

    GaugeLayout_init(&s_layoutSample7Pip,
                     SAMPLE7_PIP_LEN,
                     GAUGE_FILL_FORWARD,
                     sample7PipBodyTilesets,
                     sample7PipSegments,
                     GAUGE_ORIENT_HORIZONTAL,
                     PAL0, 1, 0, 0);
    GaugeLayout_setPipStyles(&s_layoutSample7Pip, sample7PipCompactTilesets, sample7PipWidths);

    const u16 sample7PipMaxPixels = (u16)(SAMPLE7_PIP_LEN * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample7Pip, &(GaugeInit){
        .maxValue = 4,
        .maxFillPixels = sample7PipMaxPixels,
        .initialValue = 0,
        .parts = g_partsSample7Pip,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_PIP
    });

    /* Keep this mini bar simple: no trail animation. */

    Gauge_addPart(&g_gaugeSample7Pip, &g_partsSample7Pip[0],
                  &s_layoutSample7Pip,
                  SAMPLE7_PIP_X, SAMPLE7_PIP_Y);

    vramSize = Gauge_getVramSize(&g_gaugeSample7Pip, &s_layoutSample7Pip);
    logVramUsage("Sample 7 mini PIP gauge", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw labels under gauges */
    VDP_drawText("Sample 7 Multi-Segment", SAMPLE7_X, SAMPLE7_PART2_Y + 1);
    VDP_drawText("    and  Multi-Part", SAMPLE7_X, SAMPLE7_PART2_Y + 2);
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

static void initSample4Layout(void)
{
    /* Step 1: Define tilesets (cap style) */
    const u32 *sample4BodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_strip_break.tiles,
        gauge_h_bevel_blue_with_border_strip_break.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4EndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_strip_end.tiles,
        gauge_h_bevel_blue_with_border_strip_end.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4TrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_strip_trail.tiles,
        gauge_h_bevel_blue_with_border_strip_trail.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BridgeTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_to_blue_with_border_strip_bridge.tiles,
        gauge_h_bevel_blue_to_yellow_with_border_strip_bridge.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4CapStartTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4CapEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_end_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4CapStartBreakTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4CapStartTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffBodyTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_blink_off_strip_break.tiles,
        gauge_h_bevel_blue_with_border_blink_off_strip_break.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_blink_off_strip_end.tiles,
        gauge_h_bevel_blue_with_border_blink_off_strip_end.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_blink_off_strip_trail.tiles,
        gauge_h_bevel_blue_with_border_blink_off_strip_trail.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffBridgeTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_to_blue_with_border_blink_off_strip_bridge.tiles,
        gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge.tiles,
        NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffCapStartTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffCapEndTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffCapStartBreakTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_break.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u32 *sample4BlinkOffCapStartTrailTilesets[GAUGE_MAX_SEGMENTS] = {
        gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail.tiles,
        NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL, NULL,NULL, NULL
    };
    const u8 sample4CapEndBySegment[GAUGE_MAX_SEGMENTS] = {
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };

    /* Step 2: Define segments (all cap style) */
    const u8 sample4Segments[SAMPLE4_LENGTH] = {
        0, 0, 0, 0,
        1, 1, 1, 1, 1, 1,
        0, 0, 0, 0
    };

    /* Step 3: Initialize layout */
    GaugeLayout_initEx(&s_layoutSample4,
                       SAMPLE4_LENGTH,
                       GAUGE_FILL_FORWARD,
                       sample4BodyTilesets,
                       sample4EndTilesets,
                       sample4TrailTilesets,
                       sample4BridgeTilesets,
                       sample4Segments,
                       GAUGE_ORIENT_HORIZONTAL,
                       PAL0, 1, 0, 0);

    /* Step 4: Configure caps (start tileset + end flag)
       Cap behavior summary:
       - Cap start behaves like a classic cell (END, TRAIL, BREAK or FULL)
       - Cap end always uses its own tileset and follows the END LUT */
    GaugeLayout_setCaps(&s_layoutSample4,
                        sample4CapStartTilesets,
                        sample4CapEndTilesets,
                        sample4CapStartBreakTilesets,
                        sample4CapStartTrailTilesets,
                        sample4CapEndBySegment);

    GaugeLayout_setBlinkOff(&s_layoutSample4,
                            sample4BlinkOffBodyTilesets,
                            sample4BlinkOffEndTilesets,
                            sample4BlinkOffTrailTilesets,
                            sample4BlinkOffBridgeTilesets,
                            sample4BlinkOffCapStartTilesets,
                            sample4BlinkOffCapEndTilesets,
                            sample4BlinkOffCapStartBreakTilesets,
                            sample4BlinkOffCapStartTrailTilesets);

    GaugeLayout_makeMirror(&s_layoutSample4Mirror, &s_layoutSample4);
}

static void initSample4(u16 *nextVram)
{
    u16 vramBase;
    u16 vramSize;

    initSample4Layout();

    /* Step 5: Initialize gauge */
    const u16 sample4MaxPixels = (u16)(SAMPLE4_LENGTH * GAUGE_PIXELS_PER_TILE);

    vramBase = *nextVram;
    Gauge_init(&g_gaugeSample4, &(GaugeInit){
        .maxValue = sample4MaxPixels,
        .maxFillPixels = sample4MaxPixels,
        .initialValue = sample4MaxPixels,
        .parts = g_partsSample4,
        .vramBase = vramBase,
        .vramMode = GAUGE_VRAM_DYNAMIC,
        .valueMode = GAUGE_VALUE_MODE_FILL
    });

    Gauge_setTrailAnim(&g_gaugeSample4, 1, 0, 0);

    /* Step 6: Add part */
    Gauge_addPart(&g_gaugeSample4, &g_partsSample4[0],
                  &s_layoutSample4Mirror,
                  SAMPLE4_X, SAMPLE4_Y);

    /* Step 7: Log VRAM */
    vramSize = Gauge_getVramSize(&g_gaugeSample4, &s_layoutSample4Mirror);
    logVramUsage("Sample 4 (Cap Start/End Mirror)", vramBase, vramSize);
    *nextVram = (u16)(vramBase + vramSize);

    /* Draw label under gauge */
    VDP_drawText("Sample 4 Mirror", SAMPLE4_X, SAMPLE4_Y + 1);
    VDP_drawText("Cap Start/End", SAMPLE4_X, SAMPLE4_Y + 2);
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
 * Auto-wrap mini PIP gauge 0->4->0 for demo.
 */
static void tickBlueAutoWrap(void)
{
    /* Increment every 2 frames (~3.3 seconds per full cycle at 60fps) */
    if ((g_frameCount & 55) == 0)
    {
        g_sample7PipValue++;
        if (g_sample7PipValue > 4)
            g_sample7PipValue = 0;

        Gauge_setValue(&g_gaugeSample7Pip, g_sample7PipValue);
    }
}

/* Targeted debug tests removed. */

/**
 * Update all gauges (tick logic + render).
 * Call this once per frame.
 */
static void updateAllGauges(void)
{
    Gauge_update(&g_gaugeSample1);
    Gauge_update(&g_gaugeSample2);
    Gauge_update(&g_gaugeSample3);
    Gauge_update(&g_gaugeSample4);
    Gauge_update(&g_gaugeSample5);
    Gauge_update(&g_gaugeSample6);
    Gauge_update(&g_gaugeSample7);
    Gauge_update(&g_gaugeSample7Pip);
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

    initSample1(&nextVram);    /* Sample 1 + Sample 2 PIP */
    initSample3(&nextVram);    /* Sample 3 (cap+border) */
    initSample4(&nextVram);    /* Sample 4 (cap start/end) */
    initSample5(&nextVram);    /* Sample 5 (multi-bridge) */
    initSample6(&nextVram);    /* Sample 6 (vertical, FIXED mode) */
    initSample7(&nextVram);    /* Sample 7 (multi-segment + blue) */

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




