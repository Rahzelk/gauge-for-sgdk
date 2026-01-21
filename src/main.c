#include <genesis.h>
#include <dma.h>
#include "gauge.h"
#include "gauge_test.h"

/* ============================================================================
   Inputs
   ----------------------------------------------------------------------------
   A     : increase yellow gauges (heal)
   B     : decrease yellow gauges (damage: blink + trail on press)
   START : toggle orientation (horizontal <-> vertical)
   C     : toggle 2-player mirror mode
   ----------------------------------------------------------------------------
   Blue gauge (example 3) wraps automatically 0..100% in loop.
   ============================================================================ */

typedef struct
{
    GaugeRenderer ex1Yellow;
    GaugeRenderer ex2Yellow;
    GaugeRenderer ex3Yellow;
    GaugeRenderer ex3Blue;

    /* Mirror renderers (P2) */
    GaugeRenderer ex1YellowMirror;
    GaugeRenderer ex2YellowMirror;
    GaugeRenderer ex3YellowMirror;
    GaugeRenderer ex3BlueMirror;

    /* Mirror layouts are built in main (need fill direction opposite of P1) */
    GaugeLayout ex1MirrorLayout;
    GaugeLayout ex2MirrorLayout;
    GaugeLayout ex3YellowMirrorLayout;
    GaugeLayout ex3BlueMirrorLayout;

} TestRenderers;

static GaugeLogic g_yellowLogic;
static GaugeLogic g_blueLogic;

static TestRenderers g_renderers;

static GaugeOrientation g_orientation = GAUGE_ORIENT_HORIZONTAL;
static u8 g_mirrorEnabled = 0;

static u16 g_frame = 0;
static u16 g_prevPad = 0;

/* Hold-repeat counters for A/B */
static u8 g_holdA = 0;
static u8 g_holdB = 0;

/* Blue percent 0..100 */
static u16 g_bluePercent = 0;

/* ------------------------------
   VRAM allocation (fixed)
   ------------------------------
   We allocate distinct VRAM regions for each renderer to avoid conflicts.
   Total stays small (a few dozen tiles per gauge).
*/
#define VRAM_BASE   (TILE_USER_INDEX)

/* P1 */
#define VRAM_EX1_P1_L0   (VRAM_BASE)                /* 12 */
#define VRAM_EX1_P1_L1   (VRAM_EX1_P1_L0 + 16)      /* unused, but keep spacing */

#define VRAM_EX2_P1_L0   (VRAM_BASE +  16)          /* 12 */
#define VRAM_EX2_P1_L1   (VRAM_EX2_P1_L0 + 16)      /* 12 */

#define VRAM_EX3Y_P1_L0  (VRAM_BASE +  48)          /* 12 */
#define VRAM_EX3Y_P1_L1  (VRAM_EX3Y_P1_L0 + 16)     /* 4 (we still reserve 16 for simplicity) */

#define VRAM_EX3B_P1_L0  (VRAM_BASE +  80)          /* 8 */
#define VRAM_EX3B_P1_L1  (VRAM_EX3B_P1_L0 + 16)     /* unused */

/* P2 mirrors (separate regions) */
#define VRAM_EX1_P2_L0   (VRAM_BASE + 112)
#define VRAM_EX1_P2_L1   (VRAM_EX1_P2_L0 + 16)

#define VRAM_EX2_P2_L0   (VRAM_BASE + 128)
#define VRAM_EX2_P2_L1   (VRAM_EX2_P2_L0 + 16)

#define VRAM_EX3Y_P2_L0  (VRAM_BASE + 160)
#define VRAM_EX3Y_P2_L1  (VRAM_EX3Y_P2_L0 + 16)

#define VRAM_EX3B_P2_L0  (VRAM_BASE + 192)
#define VRAM_EX3B_P2_L1  (VRAM_EX3B_P2_L0 + 16)

/* Background tile is GAUGE_TEST_VRAM_BG_TILE (TILE_USER_INDEX+256) */

static void setupWindowFullScreen(void)
{
    VDP_setPlaneSize(64, 32, TRUE);

    /* Full-screen WINDOW */
    VDP_setWindowHPos(TRUE, 0);
    VDP_setWindowVPos(TRUE, 0);

    VDP_setBackgroundColor(0);
    VDP_setTextPlane(WINDOW);
}

static void drawTextHeader(void)
{
    VDP_drawText("Gauge Test - A:+  B:-  START:H/V  C:Mirror", 2, 1);

    if (g_orientation == GAUGE_ORIENT_HORIZONTAL)
        VDP_drawText("MODE: HORIZONTAL", 2, 2);
    else
        VDP_drawText("MODE: VERTICAL", 2, 2);

    if (g_mirrorEnabled)
        VDP_drawText("MIRROR: ON ", 22, 2);
    else
        VDP_drawText("MIRROR: OFF", 22, 2);

    VDP_drawText("Example1: yellow 1 lane", 2, 4);
    VDP_drawText("Example2: yellow 2 lanes", 2, 5);
    VDP_drawText("Example3: yellow 2->1 lanes + blue wrap (thin)", 2, 6);
}

static void drawGaugeBackgroundBox(GaugeOrientation orient, u16 x, u16 y, u8 lengthTiles, u8 thicknessLanes)
{
    const u16 bgAttr = TILE_ATTR_FULL(0, 1, 0, 0, GAUGE_TEST_VRAM_BG_TILE);

    if (orient == GAUGE_ORIENT_HORIZONTAL)
    {
        /* width=lengthTiles, height=thicknessLanes (1 or 2) */
        for (u8 cell = 0; cell < lengthTiles; cell++)
        {
            for (u8 lane = 0; lane < thicknessLanes; lane++)
                VDP_setTileMapXY(WINDOW, bgAttr, (u16)(x + cell), (u16)(y + lane));
        }
    }
    else
    {
        /* width=thicknessLanes, height=lengthTiles */
        for (u8 cell = 0; cell < lengthTiles; cell++)
        {
            for (u8 lane = 0; lane < thicknessLanes; lane++)
                VDP_setTileMapXY(WINDOW, bgAttr, (u16)(x + lane), (u16)(y + cell));
        }
    }
}

/* Mirror layout builder that flips BOTH:
   - cell order (segmentIdByCell reversed)
   - fill direction (forward <-> reverse), detected from src fillIndexByCell[0]
   This is required because GaugeLayout_makeMirror() always sets reverse.
*/
static void buildMirrorLayoutOppositeFill(GaugeLayout *dst, const GaugeLayout *src, const GaugeRom *rom)
{
    dst->lengthTiles = src->lengthTiles;

    for (u8 c = 0; c < src->lengthTiles; c++)
    {
        const u8 srcCell = (u8)(src->lengthTiles - 1 - c);
        dst->segmentIdByCell[c] = src->segmentIdByCell[srcCell];
    }

    /* Detect src direction */
    if (src->fillIndexByCell[0] == 0)
        GaugeLayout_setFillReverse(dst);
    else
        GaugeLayout_setFillForward(dst);

    GaugeLayout_buildLaneListsFromRom(dst, rom);
}

static void initOrRebuildAllRenderers(void)
{
    VDP_clearPlane(WINDOW, TRUE);
    VDP_setTextPlane(WINDOW);

    drawTextHeader();

    const GaugeRom *yellowRom = GaugeTest_getYellowRom(g_orientation);
    const GaugeRom *blueRom   = GaugeTest_getBlueRom(g_orientation);

    const GaugeLayout *ex1Layout = GaugeTest_getLayoutExample1(g_orientation);
    const GaugeLayout *ex2Layout = GaugeTest_getLayoutExample2(g_orientation);
    const GaugeLayout *ex3YLayout = GaugeTest_getLayoutExample3Yellow(g_orientation);
    const GaugeLayout *ex3BLayout = GaugeTest_getLayoutExample3Blue(g_orientation);

    /* --------------------------------------------------------
       Place examples
       -------------------------------------------------------- */
    u16 p1_ex1_x, p1_ex1_y;
    u16 p1_ex2_x, p1_ex2_y;
    u16 p1_ex3_x, p1_ex3_y;

    u16 p2_ex1_x, p2_ex1_y;
    u16 p2_ex2_x, p2_ex2_y;
    u16 p2_ex3_x, p2_ex3_y;

    if (g_orientation == GAUGE_ORIENT_HORIZONTAL)
    {
        /* stacked vertically */
        p1_ex1_x = 2;  p1_ex1_y = 8;
        p1_ex2_x = 2;  p1_ex2_y = 11;
        p1_ex3_x = 2;  p1_ex3_y = 15;

        /* mirror to the right */
        const u16 sep = 4;
        const u16 mirrorX = (u16)(p1_ex1_x + GAUGE_TEST_YELLOW_LENGTH_TILES + sep);

        p2_ex1_x = mirrorX; p2_ex1_y = p1_ex1_y;
        p2_ex2_x = mirrorX; p2_ex2_y = p1_ex2_y;
        p2_ex3_x = mirrorX; p2_ex3_y = p1_ex3_y;
    }
    else
    {
        /* side-by-side near bottom */
        const u16 bottomY = (u16)(28 - GAUGE_TEST_YELLOW_LENGTH_TILES - 1);

        p1_ex1_x = 4;   p1_ex1_y = bottomY;
        p1_ex2_x = 10;  p1_ex2_y = bottomY;
        p1_ex3_x = 18;  p1_ex3_y = bottomY;

        /* mirror above */
        const u16 sepY = 2;
        const u16 mirrorY = (u16)(bottomY - GAUGE_TEST_YELLOW_LENGTH_TILES - sepY);

        p2_ex1_x = p1_ex1_x; p2_ex1_y = mirrorY;
        p2_ex2_x = p1_ex2_x; p2_ex2_y = mirrorY;
        p2_ex3_x = p1_ex3_x; p2_ex3_y = mirrorY;
    }

    /* --------------------------------------------------------
       Draw background boxes (to see lane holes)
       -------------------------------------------------------- */
    /* Example 1 thickness=1 */
    drawGaugeBackgroundBox(g_orientation, p1_ex1_x, p1_ex1_y, GAUGE_TEST_YELLOW_LENGTH_TILES, 1);
    /* Example 2 thickness=2 */
    drawGaugeBackgroundBox(g_orientation, p1_ex2_x, p1_ex2_y, GAUGE_TEST_YELLOW_LENGTH_TILES, 2);
    /* Example 3 thickness=2 (because yellow has 2 lanes on first segment, and blue lives in the 2nd lane area) */
    drawGaugeBackgroundBox(g_orientation, p1_ex3_x, p1_ex3_y, GAUGE_TEST_YELLOW_LENGTH_TILES, 2);

    if (g_mirrorEnabled)
    {
        drawGaugeBackgroundBox(g_orientation, p2_ex1_x, p2_ex1_y, GAUGE_TEST_YELLOW_LENGTH_TILES, 1);
        drawGaugeBackgroundBox(g_orientation, p2_ex2_x, p2_ex2_y, GAUGE_TEST_YELLOW_LENGTH_TILES, 2);
        drawGaugeBackgroundBox(g_orientation, p2_ex3_x, p2_ex3_y, GAUGE_TEST_YELLOW_LENGTH_TILES, 2);
    }

    /* --------------------------------------------------------
       Init P1 renderers
       -------------------------------------------------------- */
    /* Example 1: yellow 1 lane */
    GaugeRenderer_init(&g_renderers.ex1Yellow,
                       g_orientation,
                       yellowRom, ex1Layout,
                       p1_ex1_x, p1_ex1_y,
                       VRAM_EX1_P1_L0, VRAM_EX1_P1_L1,
                       0, 1, 0, 0);

    /* Example 2: yellow 2 lanes */
    GaugeRenderer_init(&g_renderers.ex2Yellow,
                       g_orientation,
                       yellowRom, ex2Layout,
                       p1_ex2_x, p1_ex2_y,
                       VRAM_EX2_P1_L0, VRAM_EX2_P1_L1,
                       0, 1, 0, 0);

    /* Example 3: yellow mixed 2->1 lanes */
    GaugeRenderer_init(&g_renderers.ex3Yellow,
                       g_orientation,
                       yellowRom, ex3YLayout,
                       p1_ex3_x, p1_ex3_y,
                       VRAM_EX3Y_P1_L0, VRAM_EX3Y_P1_L1,
                       0, 1, 0, 0);

    /* Example 3: blue thin gauge, aligned with the tail part (cells 4..11 => 8 tiles)
       Horizontal: place it "under" the 1-lane part (same axis, lane below).
       Vertical  : place it "to the right" of the 1-lane part (same axis, lane right).
    */
    u16 blue_p1_x = p1_ex3_x;
    u16 blue_p1_y = p1_ex3_y;

    if (g_orientation == GAUGE_ORIENT_HORIZONTAL)
    {
        blue_p1_x = (u16)(p1_ex3_x + GAUGE_TEST_EX3_HEAD_2LANE_TILES);
        blue_p1_y = (u16)(p1_ex3_y + 1); /* lane below */
    }
    else
    {
        blue_p1_x = (u16)(p1_ex3_x + 1); /* lane right */
        blue_p1_y = (u16)(p1_ex3_y + GAUGE_TEST_EX3_HEAD_2LANE_TILES);
    }

    GaugeRenderer_init(&g_renderers.ex3Blue,
                       g_orientation,
                       blueRom, ex3BLayout,
                       blue_p1_x, blue_p1_y,
                       VRAM_EX3B_P1_L0, VRAM_EX3B_P1_L1,
                       0, 1, 0, 0);

    /* --------------------------------------------------------
       Init P2 mirror renderers (if enabled)
       -------------------------------------------------------- */
    if (g_mirrorEnabled)
    {
        /* Build mirror layouts with opposite fill direction */
        buildMirrorLayoutOppositeFill(&g_renderers.ex1MirrorLayout, ex1Layout, yellowRom);
        buildMirrorLayoutOppositeFill(&g_renderers.ex2MirrorLayout, ex2Layout, yellowRom);
        buildMirrorLayoutOppositeFill(&g_renderers.ex3YellowMirrorLayout, ex3YLayout, yellowRom);
        buildMirrorLayoutOppositeFill(&g_renderers.ex3BlueMirrorLayout, ex3BLayout, blueRom);

        u8 mirrorVFlip = 0;
        u8 mirrorHFlip = 0;

        if (g_orientation == GAUGE_ORIENT_HORIZONTAL)
        {
            mirrorHFlip = 1; /* mirror fill inside tile on X */
        }
        else
        {
            mirrorVFlip = 1; /* mirror fill inside tile on Y */
        }

        GaugeRenderer_init(&g_renderers.ex1YellowMirror,
                           g_orientation,
                           yellowRom, &g_renderers.ex1MirrorLayout,
                           p2_ex1_x, p2_ex1_y,
                           VRAM_EX1_P2_L0, VRAM_EX1_P2_L1,
                           0, 1, mirrorVFlip, mirrorHFlip);

        GaugeRenderer_init(&g_renderers.ex2YellowMirror,
                           g_orientation,
                           yellowRom, &g_renderers.ex2MirrorLayout,
                           p2_ex2_x, p2_ex2_y,
                           VRAM_EX2_P2_L0, VRAM_EX2_P2_L1,
                           0, 1,  mirrorVFlip, mirrorHFlip);

        GaugeRenderer_init(&g_renderers.ex3YellowMirror,
                           g_orientation,
                           yellowRom, &g_renderers.ex3YellowMirrorLayout,
                           p2_ex3_x, p2_ex3_y,
                           VRAM_EX3Y_P2_L0, VRAM_EX3Y_P2_L1,
                           0, 1, mirrorVFlip, mirrorHFlip);

        /* Mirror blue position matches the same geometric rule as P1 */
        u16 blue_p2_x = p2_ex3_x;
        u16 blue_p2_y = p2_ex3_y;

        if (g_orientation == GAUGE_ORIENT_HORIZONTAL)
        {
            blue_p2_x = p2_ex3_x;
            blue_p2_y = (u16)(p2_ex3_y + 1);
        }
        else
        {
            blue_p2_x = (u16)(p2_ex3_x + 1);
            blue_p2_y = p2_ex3_y;
        }

        GaugeRenderer_init(&g_renderers.ex3BlueMirror,
                           g_orientation,
                           blueRom, &g_renderers.ex3BlueMirrorLayout,
                           blue_p2_x, blue_p2_y,
                           VRAM_EX3B_P2_L0, VRAM_EX3B_P2_L1,
                           0, 1,  mirrorVFlip, mirrorHFlip);
    }
}

static void initLogic(void)
{
    /* Yellow logic uses pixels directly:
       maxFillPixels = 12*8 = 96
    */
    const u16 yellowMaxPixels = (u16)(GAUGE_TEST_YELLOW_LENGTH_TILES * 8);
    GaugeLogic_init(&g_yellowLogic, yellowMaxPixels, yellowMaxPixels, NULL);
    GaugeLogic_setAnimation(&g_yellowLogic,
                            0, /* valueAnimEnabled: instant */
                            4, /* valueAnimShift (unused if instant) */
                            4, /* trailAnimShift */
                            3  /* blinkShift */
    );
    GaugeLogic_setValue(&g_yellowLogic, yellowMaxPixels);

    /* Blue logic wraps 0..100% with LUT -> pixels (length 8 => 64px) */
    GaugeLogic_init(&g_blueLogic, 100, (u16)(GAUGE_TEST_BLUE_LENGTH_TILES * 8), GaugeTest_getBluePercentToPixelsLUT());
    GaugeLogic_setAnimation(&g_blueLogic, 0, 4, 4, 3);
    GaugeLogic_setValue(&g_blueLogic, 0);
}

static void tickBlueWrap(void)
{
    /* Wrap speed: 1% every 2 frames (~3.3s per loop at 60fps) */
    if ((g_frame & 1) == 0)
    {
        g_bluePercent++;
        if (g_bluePercent > 100) g_bluePercent = 0;
        GaugeLogic_setValue(&g_blueLogic, (u16)g_bluePercent);
    }
}

static void applyYellowInputPress(u16 pressed)
{
    if (pressed & BUTTON_A)
    {
        GaugeLogic_applyIncrease(&g_yellowLogic, 4);
        g_holdA = 0;
    }

    if (pressed & BUTTON_B)
    {
        /* Damage with visible trail+blink */
        GaugeLogic_applyDecrease(&g_yellowLogic, 4, 20, 90);
        g_holdB = 0;
    }
}

static void applyYellowHoldRepeat(u16 padState)
{
    if (padState & BUTTON_A) g_holdA++; else g_holdA = 0;
    if (padState & BUTTON_B) g_holdB++; else g_holdB = 0;

    /* After delay, repeat every 3 frames */
    if (g_holdA >= 12)
    {
        if ((g_frame & 3) == 0)
            GaugeLogic_applyIncrease(&g_yellowLogic, 1);
    }

    if (g_holdB >= 12)
    {
        if ((g_frame & 3) == 0)
        {
            /* Drain without re-triggering blink/hold */
            GaugeLogic_applyDecrease(&g_yellowLogic, 1, 0, 0);
        }
    }
}

static void queueAllDma(void)
{
    /* Tick logics */
    GaugeLogic_tick(&g_yellowLogic);
    GaugeLogic_tick(&g_blueLogic);

    /* P1 renderers */
    GaugeRenderer_queueDma(&g_renderers.ex1Yellow, &g_yellowLogic, g_frame);
    GaugeRenderer_queueDma(&g_renderers.ex2Yellow, &g_yellowLogic, g_frame);
    GaugeRenderer_queueDma(&g_renderers.ex3Yellow, &g_yellowLogic, g_frame);
    GaugeRenderer_queueDma(&g_renderers.ex3Blue, &g_blueLogic, g_frame);

    /* P2 mirrors (if enabled) */
    if (g_mirrorEnabled)
    {
        GaugeRenderer_queueDma(&g_renderers.ex1YellowMirror, &g_yellowLogic, g_frame);
        GaugeRenderer_queueDma(&g_renderers.ex2YellowMirror, &g_yellowLogic, g_frame);
        GaugeRenderer_queueDma(&g_renderers.ex3YellowMirror, &g_yellowLogic, g_frame);
        GaugeRenderer_queueDma(&g_renderers.ex3BlueMirror, &g_blueLogic, g_frame);
    }
}

int main(bool hardReset)
{
    (void)hardReset;

    /* Mirrors require streaming >80 tiles at once -> enlarge DMA queue (default=80). */
    if (DMA_getMaxQueueSize() < 160)
        DMA_setMaxQueueSize(160);

    JOY_init();
    setupWindowFullScreen();

    GaugeTest_init();
    initLogic();

    initOrRebuildAllRenderers();

    while (TRUE)
    {
        g_frame++;

        const u16 padState = JOY_readJoypad(JOY_1);
        const u16 pressed  = (u16)((padState ^ g_prevPad) & padState);
        g_prevPad = padState;

        /* START: toggle orientation */
        if (pressed & BUTTON_START)
        {
            g_orientation = (g_orientation == GAUGE_ORIENT_HORIZONTAL) ? GAUGE_ORIENT_VERTICAL : GAUGE_ORIENT_HORIZONTAL;
            initOrRebuildAllRenderers();
        }

        /* C: toggle mirror mode */
        if (pressed & BUTTON_C)
        {
            g_mirrorEnabled = (u8)!g_mirrorEnabled;
            initOrRebuildAllRenderers();
        }

        /* Inputs apply to yellow gauges (all examples, both players if enabled) */
        applyYellowInputPress(pressed);
        applyYellowHoldRepeat(padState);

        /* Blue wrap runs automatically */
        tickBlueWrap();

        /* Stream tiles */
        queueAllDma();

        SYS_doVBlankProcess();
    }

    return 0;
}
