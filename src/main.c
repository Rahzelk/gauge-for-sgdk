#include <genesis.h>
#include "gauge.h"
#include "gauge_assets.h"

/* =============================================================================
   Gauge Builder V2 demo (all samples use GaugeBuilder API only)
   ============================================================================= */

#define VRAM_BASE            TILE_USER_INDEX

#define SAMPLE1_LENGTH       12
#define SAMPLE2_LENGTH       14
#define SAMPLE3_LENGTH       14
#define SAMPLE4_LENGTH       14
#define SAMPLE5_LENGTH       16
#define SAMPLE6_LENGTH       12
#define SAMPLE7_LENGTH       12
#define SAMPLE7_SLAVE_LENGTH  3
#define SAMPLE7_PIP_LENGTH    8

#define SAMPLE1_X  2
#define SAMPLE1_Y  9
#define SAMPLE2_X  (SAMPLE1_X + SAMPLE1_LENGTH + 4)
#define SAMPLE2_Y  SAMPLE1_Y

#define SAMPLE3_X  2
#define SAMPLE3_Y 14

#define SAMPLE4_X  (SAMPLE3_X + SAMPLE3_LENGTH + 3)
#define SAMPLE4_Y  SAMPLE3_Y

#define SAMPLE5_X  2
#define SAMPLE5_Y 18

#define SAMPLE6_X 36
#define SAMPLE6_Y 10

#define SAMPLE7_X       2
#define SAMPLE7_Y      23
#define SAMPLE7_SLAVE_X 2
#define SAMPLE7_SLAVE_Y (SAMPLE7_Y + 1)
#define SAMPLE7_PIP_X   (SAMPLE7_SLAVE_X + SAMPLE7_SLAVE_LENGTH)
#define SAMPLE7_PIP_Y   SAMPLE7_SLAVE_Y

#define GAUGE_COUNT 7

/* -----------------------------------------------------------------------------
   Runtime objects
   ----------------------------------------------------------------------------- */
static Gauge g_sample1Gauge;
static Gauge g_sample2Gauge;
static Gauge g_sample3Gauge;
static Gauge g_sample4Gauge;
static Gauge g_sample5Gauge;
static Gauge g_sample6Gauge;
static Gauge g_sample7Gauge;
static Gauge g_sample7PipGauge;

/* Builder workspace reused across all sample initializations.
 * Keeping a single instance avoids large static RAM usage. */
static GaugeBuilder s_builder;

/* Input state */
static u16 g_previousPad = 0;
static u8 g_selectedGauge = 0;
static u8 g_holdA = 0;
static u8 g_holdB = 0;

/* Mini PIP auto wrap */
static u16 g_sample7PipValue = 0;
static u16 g_frameCount = 0;

/* -----------------------------------------------------------------------------
   Small helpers
   ----------------------------------------------------------------------------- */
static void logVramUsage(const char *name, u16 vramBase, u16 tileCount)
{
    KLog((char *)name);
    KLog_U2(" VRAM base=", vramBase, " tiles=", tileCount);
}

static u8 buildGaugeFromBuilder(const char *name,
                                GaugeBuilder *builder,
                                Gauge *gauge,
                                u16 *nextVram,
                                GaugeVramMode vramMode)
{
    const u16 vramBase = *nextVram;
    if (!GaugeBuilder_build(builder, gauge, vramBase, vramMode))
    {
        KLog((char *)"GaugeBuilder_build failed");
        KLog((char *)name);
        return 0;
    }

    const u16 usedTiles = gauge->vramNextOffset;
    logVramUsage(name, vramBase, usedTiles);
    *nextVram = (u16)(vramBase + usedTiles);
    return 1;
}

static Gauge *getSelectedGauge(void)
{
    switch (g_selectedGauge)
    {
        case 0: return &g_sample1Gauge;
        case 1: return &g_sample2Gauge;
        case 2: return &g_sample3Gauge;
        case 3: return &g_sample4Gauge;
        case 4: return &g_sample5Gauge;
        case 5: return &g_sample6Gauge;
        case 6: return &g_sample7Gauge;
        default: return &g_sample1Gauge;
    }
}

static const char *getSelectedGaugeName(void)
{
    switch (g_selectedGauge)
    {
        case 0: return "Sample 1";
        case 1: return "Sample 2 PIP";
        case 2: return "Sample 3 Caps";
        case 3: return "Sample 4 Border";
        case 4: return "Sample 5 Bridges";
        case 5: return "Sample 6 Vertical";
        case 6: return "Sample 7 MultiPart";
        default: return "Sample 1";
    }
}

/* -----------------------------------------------------------------------------
   Display helpers
   ----------------------------------------------------------------------------- */
static void setupWindowFullScreen(void)
{
    VDP_setPlaneSize(64, 32, TRUE);
    VDP_setWindowHPos(TRUE, 0);
    VDP_setWindowVPos(TRUE, 0);
    VDP_setBackgroundColor(0);
    VDP_setTextPalette(PAL0);
    VDP_setTextPlane(WINDOW);
}

static void drawHeader(void)
{
    VDP_drawText("A: increase   B: decrease", 2, 1);
    VDP_drawText("C: select gauge", 2, 3);
    VDP_drawText("------------------------------", 0, 5);
    VDP_drawText("Selected:", 0, 6);
    VDP_drawText("------------------------------", 0, 7);
}

static void updateSelectedDisplay(void)
{
    VDP_drawText("                ", 10, 6);
    VDP_drawText(getSelectedGaugeName(), 10, 6);
}

/* -----------------------------------------------------------------------------
   Samples
   ----------------------------------------------------------------------------- */
static void initSample1(u16 *nextVram)
{
    GaugeDescription description = {
        .mode = GAUGE_VALUE_MODE_FILL,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .fillDirection = GAUGE_FILL_FORWARD,
        .originX = SAMPLE1_X,
        .originY = SAMPLE1_Y,
        .maxValue = (u16)(SAMPLE1_LENGTH * GAUGE_PIXELS_PER_TILE),
        .capStartFixed = 0,
        .capEndFixed = 0,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &description);

    GaugeSegmentDefinition segment = GAUGE_SEGMENT_ATTR(
        SAMPLE1_LENGTH,
        GAUGE_SEGMENT_TILESETS(&gauge_h_straight_yellow_strip, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );
    GaugeBuilder_addSegment(&s_builder, 0, &segment);

    if (!buildGaugeFromBuilder("Sample 1", &s_builder, &g_sample1Gauge, nextVram, GAUGE_VRAM_DYNAMIC))
        return;

    /* Original Sample 1 behavior (historical mode). */
    Gauge_setValueAnim(&g_sample1Gauge, 0, 0);
    Gauge_setTrailMode(&g_sample1Gauge,
                       GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
                       30,
                       5,
                       8);
    Gauge_setGainMode(&g_sample1Gauge, GAUGE_GAIN_MODE_DISABLED, 0, 0);

    VDP_drawText("Sample 1", SAMPLE1_X, SAMPLE1_Y + 1);
}

static void initSample2(u16 *nextVram)
{
    GaugeDescription description = {
        .mode = GAUGE_VALUE_MODE_PIP,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .fillDirection = GAUGE_FILL_FORWARD,
        .originX = SAMPLE2_X,
        .originY = SAMPLE2_Y,
        .maxValue = 7,
        .capStartFixed = 0,
        .capEndFixed = 0,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &description);

    GaugeSegmentDefinition segment = GAUGE_SEGMENT_ATTR(
        SAMPLE2_LENGTH,
        GAUGE_SEGMENT_TILESETS(&gauge_h_straight_yellow_strip, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );
    segment.pipStrip = gauge_h_pip_basic_strip.tiles;
    segment.pipWidth = 2;
    GaugeBuilder_addSegment(&s_builder, 0, &segment);

    if (!buildGaugeFromBuilder("Sample 2 (PIP)", &s_builder, &g_sample2Gauge, nextVram, GAUGE_VRAM_DYNAMIC))
        return;

    Gauge_setValueAnim(&g_sample2Gauge, 1, 2);
    Gauge_setTrailMode(&g_sample2Gauge, GAUGE_TRAIL_MODE_FOLLOW, 0, 0, 0);
    Gauge_setGainMode(&g_sample2Gauge, GAUGE_GAIN_MODE_FOLLOW, 0, 0);

    VDP_drawText("Sample 2 PIP", SAMPLE2_X, SAMPLE2_Y + 1);
}

static void initSample3(u16 *nextVram)
{
    GaugeDescription description = {
        .mode = GAUGE_VALUE_MODE_FILL,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .fillDirection = GAUGE_FILL_FORWARD,
        .originX = SAMPLE3_X,
        .originY = SAMPLE3_Y,
        .maxValue = (u16)(SAMPLE3_LENGTH * GAUGE_PIXELS_PER_TILE),
        .capStartFixed = 1,
        .capEndFixed = 1,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &description);

    GaugeSegmentDefinition segment0 = GAUGE_SEGMENT_ATTR(
        1,
        GAUGE_SEGMENT_TILESETS(&gauge_seg0h_capstart_body, &gauge_seg0h_capstart_trail,
                   &gauge_seg0h_capstart_end, NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_seg0l_capstart_body, &gauge_seg0l_capstart_trail,
                   &gauge_seg0l_capstart_end, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition segment1 = GAUGE_SEGMENT_ATTR(
        1,
        GAUGE_SEGMENT_TILESETS(&gauge_seg1h_body, &gauge_seg1h_trail, &gauge_seg1h_end, NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_seg1l_body, &gauge_seg1l_trail, &gauge_seg1l_end, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition segment2 = GAUGE_SEGMENT_ATTR(
        12,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_strip_break,
                   &gauge_h_bevel_yellow_strip_trail,
                   &gauge_h_bevel_yellow_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_gain_strip_break,
                   &gauge_h_bevel_yellow_gain_strip_trail,
                   &gauge_h_bevel_yellow_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeBuilder_addSegment(&s_builder, 0, &segment0);
    GaugeBuilder_addSegment(&s_builder, 0, &segment1);
    GaugeBuilder_addSegment(&s_builder, 0, &segment2);

    if (!buildGaugeFromBuilder("Sample 3", &s_builder, &g_sample3Gauge, nextVram, GAUGE_VRAM_DYNAMIC))
        return;

    Gauge_setTrailMode(&g_sample3Gauge, GAUGE_TRAIL_MODE_FOLLOW, 0, 0, 0);

    VDP_drawText("Sample 3 Caps", SAMPLE3_X, SAMPLE3_Y + 1);
}

static void initSample4(u16 *nextVram)
{
    GaugeDescription description = {
        .mode = GAUGE_VALUE_MODE_FILL,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .fillDirection = GAUGE_FILL_FORWARD,
        .originX = SAMPLE4_X,
        .originY = SAMPLE4_Y,
        .maxValue = (u16)(SAMPLE4_LENGTH * GAUGE_PIXELS_PER_TILE),
        .capStartFixed = 1,
        .capEndFixed = 1,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &description);

    GaugeSegmentDefinition segment0 = GAUGE_SEGMENT_ATTR(
        7,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_blue_with_border_strip_break,
                   &gauge_h_bevel_blue_with_border_strip_trail,
                   &gauge_h_bevel_blue_with_border_strip_end,
                   &gauge_h_bevel_blue_to_yellow_with_border_strip_bridge),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_blue_with_border_blink_off_strip_break,
                   &gauge_h_bevel_blue_with_border_blink_off_strip_trail,
                   &gauge_h_bevel_blue_with_border_blink_off_strip_end,
                   NULL)
    );

    GaugeSegmentDefinition segment1 = GAUGE_SEGMENT_ATTR(
        7,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_with_border_strip_break,
                   &gauge_h_bevel_yellow_with_border_strip_trail,
                   &gauge_h_bevel_yellow_with_border_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_with_border_blink_off_strip_break,
                   &gauge_h_bevel_yellow_with_border_blink_off_strip_trail,
                   &gauge_h_bevel_yellow_with_border_blink_off_strip_end,
                   NULL)
    );

    GaugeBuilder_addSegment(&s_builder, 0, &segment0);
    GaugeBuilder_addSegment(&s_builder, 0, &segment1);

    if (!buildGaugeFromBuilder("Sample 4", &s_builder, &g_sample4Gauge, nextVram, GAUGE_VRAM_DYNAMIC))
        return;

    Gauge_setTrailMode(&g_sample4Gauge, GAUGE_TRAIL_MODE_FOLLOW, 0, 0, 0);

    VDP_drawText("Sample 4 Border", SAMPLE4_X, SAMPLE4_Y + 1);
}

static void initSample5(u16 *nextVram)
{
    GaugeDescription description = {
        .mode = GAUGE_VALUE_MODE_FILL,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .fillDirection = GAUGE_FILL_FORWARD,
        .originX = SAMPLE5_X,
        .originY = SAMPLE5_Y,
        .maxValue = 100,
        .capStartFixed = 0,
        .capEndFixed = 0,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &description);

    GaugeSegmentDefinition segment0 = GAUGE_SEGMENT_ATTR(
        4,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_lightblue_strip_break,
                   &gauge_h_bevel_lightblue_strip_trail,
                   &gauge_h_bevel_lightblue_strip_end,
                   &gauge_h_bevel_lightblue_to_blue_strip_bridge),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_lightblue_gain_strip_break,
                   &gauge_h_bevel_lightblue_gain_strip_trail,
                   &gauge_h_bevel_lightblue_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition segment1 = GAUGE_SEGMENT_ATTR(
        6,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_blue_strip_break,
                   &gauge_h_bevel_blue_strip_trail,
                   &gauge_h_bevel_blue_strip_end,
                   &gauge_h_bevel_blue_to_yellow_strip_bridge),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_blue_gain_strip_break,
                   &gauge_h_bevel_blue_gain_strip_trail,
                   &gauge_h_bevel_blue_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition segment2 = GAUGE_SEGMENT_ATTR(
        6,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_strip_break,
                   &gauge_h_bevel_yellow_strip_trail,
                   &gauge_h_bevel_yellow_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_gain_strip_break,
                   &gauge_h_bevel_yellow_gain_strip_trail,
                   &gauge_h_bevel_yellow_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeBuilder_addSegment(&s_builder, 0, &segment0);
    GaugeBuilder_addSegment(&s_builder, 0, &segment1);
    GaugeBuilder_addSegment(&s_builder, 0, &segment2);

    if (!buildGaugeFromBuilder("Sample 5", &s_builder, &g_sample5Gauge, nextVram, GAUGE_VRAM_DYNAMIC))
        return;

    Gauge_setValueAnim(&g_sample5Gauge, 1, 2);
    Gauge_setTrailMode(&g_sample5Gauge,
                       GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK,
                       30,
                       3,
                       2);
    Gauge_setGainMode(&g_sample5Gauge, GAUGE_GAIN_MODE_FOLLOW, 3, 2);

    VDP_drawText("Sample 5 Bridges", SAMPLE5_X, SAMPLE5_Y + 1);
}

static void initSample6(u16 *nextVram)
{
    GaugeDescription description = {
        .mode = GAUGE_VALUE_MODE_FILL,
        .orientation = GAUGE_ORIENT_VERTICAL,
        .fillDirection = GAUGE_FILL_REVERSE,
        .originX = SAMPLE6_X,
        .originY = SAMPLE6_Y,
        .maxValue = (u16)(SAMPLE6_LENGTH * GAUGE_PIXELS_PER_TILE),
        .capStartFixed = 0,
        .capEndFixed = 0,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &description);

    GaugeSegmentDefinition segment0 = GAUGE_SEGMENT_ATTR(
        3,
        GAUGE_SEGMENT_TILESETS(&gauge_v_straight_blue_strip, &gauge_v_straight_blue_strip,
                   &gauge_v_straight_blue_strip, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition segment1 = GAUGE_SEGMENT_ATTR(
        5,
        GAUGE_SEGMENT_TILESETS(&gauge_v_straight_lightblue_strip, &gauge_v_straight_lightblue_strip,
                   &gauge_v_straight_lightblue_strip, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition segment2 = GAUGE_SEGMENT_ATTR(
        4,
        GAUGE_SEGMENT_TILESETS(&gauge_v_straight_yellow_strip, &gauge_v_straight_yellow_strip,
                   &gauge_v_straight_yellow_strip, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeBuilder_addSegment(&s_builder, 0, &segment0);
    GaugeBuilder_addSegment(&s_builder, 0, &segment1);
    GaugeBuilder_addSegment(&s_builder, 0, &segment2);

    if (!buildGaugeFromBuilder("Sample 6 (Fixed)", &s_builder, &g_sample6Gauge, nextVram, GAUGE_VRAM_FIXED))
        return;

    Gauge_setTrailMode(&g_sample6Gauge, GAUGE_TRAIL_MODE_FOLLOW, 0, 0, 0);

    VDP_drawText("Sample 6 Vertical", SAMPLE6_X - 8, SAMPLE6_Y + SAMPLE6_LENGTH + 1);
}

static void initSample7(u16 *nextVram)
{
    GaugeDescription description = {
        .mode = GAUGE_VALUE_MODE_FILL,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .fillDirection = GAUGE_FILL_FORWARD,
        .originX = SAMPLE7_X,
        .originY = SAMPLE7_Y,
        .maxValue = (u16)(SAMPLE7_LENGTH * GAUGE_PIXELS_PER_TILE),
        .capStartFixed = 1,
        .capEndFixed = 1,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &description);

    GaugeSegmentDefinition master0 = GAUGE_SEGMENT_ATTR(
        4,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_lightblue_strip_break,
                   &gauge_h_bevel_lightblue_strip_trail,
                   &gauge_h_bevel_lightblue_strip_end,
                   &gauge_h_bevel_lightblue_to_blue_strip_bridge),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_lightblue_gain_strip_break,
                   &gauge_h_bevel_lightblue_gain_strip_trail,
                   &gauge_h_bevel_lightblue_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition master1 = GAUGE_SEGMENT_ATTR(
        4,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_blue_strip_break,
                   &gauge_h_bevel_blue_strip_trail,
                   &gauge_h_bevel_blue_strip_end,
                   &gauge_h_bevel_blue_to_yellow_strip_bridge),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_blue_gain_strip_break,
                   &gauge_h_bevel_blue_gain_strip_trail,
                   &gauge_h_bevel_blue_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeSegmentDefinition master2 = GAUGE_SEGMENT_ATTR(
        4,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_strip_break,
                   &gauge_h_bevel_yellow_strip_trail,
                   &gauge_h_bevel_yellow_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_yellow_gain_strip_break,
                   &gauge_h_bevel_yellow_gain_strip_trail,
                   &gauge_h_bevel_yellow_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeBuilder_addSegment(&s_builder, 0, &master0);
    GaugeBuilder_addSegment(&s_builder, 0, &master1);
    GaugeBuilder_addSegment(&s_builder, 0, &master2);

    GaugeSlaveDescription slave = {
        .originX = SAMPLE7_SLAVE_X,
        .originY = SAMPLE7_SLAVE_Y,
        .fillOffset = 8
    };

    u8 slaveIndex = 0;
    if (!GaugeBuilder_addSlave(&s_builder, &slave, &slaveIndex))
        return;

    GaugeSegmentDefinition slaveSegment = GAUGE_SEGMENT_ATTR(
        SAMPLE7_SLAVE_LENGTH,
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_lightblue_strip_break,
                   &gauge_h_bevel_lightblue_strip_trail,
                   &gauge_h_bevel_lightblue_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(&gauge_h_bevel_lightblue_gain_strip_break,
                   &gauge_h_bevel_lightblue_gain_strip_trail,
                   &gauge_h_bevel_lightblue_gain_strip_end,
                   NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );

    GaugeBuilder_addSegment(&s_builder, slaveIndex, &slaveSegment);

    if (!buildGaugeFromBuilder("Sample 7 MultiPart", &s_builder, &g_sample7Gauge, nextVram, GAUGE_VRAM_DYNAMIC))
        return;

    Gauge_setTrailMode(&g_sample7Gauge,
                       GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK,
                       40,
                       0,
                       0);

    /* Independent mini PIP gauge (same row) */
    GaugeDescription pipDescription = {
        .mode = GAUGE_VALUE_MODE_PIP,
        .orientation = GAUGE_ORIENT_HORIZONTAL,
        .fillDirection = GAUGE_FILL_FORWARD,
        .originX = SAMPLE7_PIP_X,
        .originY = SAMPLE7_PIP_Y,
        .maxValue = 4,
        .capStartFixed = 0,
        .capEndFixed = 0,
        .palette = PAL0,
        .priority = 1,
        .verticalFlip = 0,
        .horizontalFlip = 0
    };

    GaugeBuilder_init(&s_builder, &pipDescription);
    GaugeSegmentDefinition pipSegment = GAUGE_SEGMENT_ATTR(
        SAMPLE7_PIP_LENGTH,
        GAUGE_SEGMENT_TILESETS(&gauge_h_straight_yellow_strip, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL),
        GAUGE_SEGMENT_TILESETS(NULL, NULL, NULL, NULL)
    );
    pipSegment.pipStrip = gauge_h_pip_mini_bar_strip.tiles;
    pipSegment.pipWidth = 2;
    GaugeBuilder_addSegment(&s_builder, 0, &pipSegment);
    buildGaugeFromBuilder("Sample 7 Mini PIP", &s_builder, &g_sample7PipGauge, nextVram, GAUGE_VRAM_DYNAMIC);

    VDP_drawText("Sample 7 MultiPart", SAMPLE7_X, SAMPLE7_SLAVE_Y + 1);
    VDP_drawText("+ Mini PIP", SAMPLE7_X, SAMPLE7_SLAVE_Y + 2);
}

/* -----------------------------------------------------------------------------
   Input and update
   ----------------------------------------------------------------------------- */
static void handleInput(u16 pressed, u16 held)
{
    if (pressed & BUTTON_C)
    {
        g_selectedGauge = (u8)((g_selectedGauge + 1) % GAUGE_COUNT);
        updateSelectedDisplay();
    }

    if (pressed & BUTTON_A)
    {
        Gauge *selectedGauge = getSelectedGauge();
        u16 amount = (g_selectedGauge == 1) ? 1 : 4;
        Gauge_increase(selectedGauge, amount, 40, 20);
        g_holdA = 0;
    }

    if (pressed & BUTTON_B)
    {
        Gauge *selectedGauge = getSelectedGauge();
        u16 amount = (g_selectedGauge == 1) ? 1 : 4;
        Gauge_decrease(selectedGauge, amount, 40, 80);
        g_holdB = 0;
    }

    if (held & BUTTON_A)
    {
        g_holdA++;
        if (g_holdA >= 12 && (g_frameCount & 3) == 0)
            Gauge_increase(getSelectedGauge(), 1, 0, 0);
    }
    else
    {
        g_holdA = 0;
    }

    if (held & BUTTON_B)
    {
        g_holdB++;
        if (g_holdB >= 12 && (g_frameCount & 3) == 0)
            Gauge_decrease(getSelectedGauge(), 1, 0, 0);
    }
    else
    {
        g_holdB = 0;
    }
}

static void tickMiniPipAutoWrap(void)
{
    if ((g_frameCount & 31) == 0)
    {
        g_sample7PipValue++;
        if (g_sample7PipValue > 4)
            g_sample7PipValue = 0;
        Gauge_setValue(&g_sample7PipGauge, g_sample7PipValue);
    }
}

static void updateAllGauges(void)
{
    Gauge_update(&g_sample1Gauge);
    Gauge_update(&g_sample2Gauge);
    Gauge_update(&g_sample3Gauge);
    Gauge_update(&g_sample4Gauge);
    Gauge_update(&g_sample5Gauge);
    Gauge_update(&g_sample6Gauge);
    Gauge_update(&g_sample7Gauge);
    Gauge_update(&g_sample7PipGauge);
}

/* -----------------------------------------------------------------------------
   Main
   ----------------------------------------------------------------------------- */
int main(bool hardReset)
{

        if (!hardReset)
                SYS_hardReset();
                
    if (DMA_getMaxQueueSize() < 160)
        DMA_setMaxQueueSize(160);

    JOY_init();
    setupWindowFullScreen();
    PAL_setPalette(PAL0, gauge_palette.data, DMA);

    drawHeader();
    updateSelectedDisplay();

    u16 nextVram = VRAM_BASE;

    KLog((char *)"=== GAUGE BUILDER V2 DEMO ===");

    initSample1(&nextVram);
    initSample2(&nextVram);
    initSample3(&nextVram);
    initSample4(&nextVram);
    initSample5(&nextVram);
    initSample6(&nextVram);
    initSample7(&nextVram);

    KLog_U1("Total tiles used in VRAM: ", (u16)(nextVram - VRAM_BASE));

    while (TRUE)
    {
        g_frameCount++;

        const u16 padState = JOY_readJoypad(JOY_1);
        const u16 pressed = (u16)((padState ^ g_previousPad) & padState);
        g_previousPad = padState;

        handleInput(pressed, padState);
        tickMiniPipAutoWrap();
        updateAllGauges();

        SYS_doVBlankProcess();
    }

    return 0;
}

