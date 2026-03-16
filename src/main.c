#include <genesis.h>
#include "./gauge/gauge.h"
#include "gauge_assets.h"

/* =============================================================================
   Gauge minimal demo entry point
   ============================================================================= */

#define GAUGE_DEMO_USE_SHOWCASE   1
#if !GAUGE_DEMO_USE_SHOWCASE

/* -----------------------------------------------------------------------------
   1. Define skins (= assets) and the GaugeDefinition
   ----------------------------------------------------------------------------- */
static const GaugeSkin g_demoSkin = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_yellow_strip
        }
    }
};

static const GaugeDefinition g_demoDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = BG_A,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 4,
    .originY = 12,
    .maxValue = 96,
    .palette = PAL0,
    .lanes = {
        [0] = {
            .segments = {
                [0] = {
                    .cells = 12,
                    .skin = &g_demoSkin
                }
            }
        }
    },
    .behavior = {
        .valueAnimEnabled = 1,
        .valueAnimShift = 4,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .damageHoldFrames = 20,
        .damageBlinkFrames = 32,
        .damageAnimShift = 4,
        .damageBlinkShift = 3,
        .gainMode = GAUGE_GAIN_MODE_DISABLED,
        .gainHoldFrames = 20,
        .gainBlinkFrames = 32,
        .gainAnimShift = 4,
        .gainBlinkShift = 3
    }
};

static void setupDemoScreen(void);
static void handleInput(Gauge * g_demoGauge);
#endif

void runGaugeShowcase(bool hardReset);

int main(bool hardReset)
{
#if GAUGE_DEMO_USE_SHOWCASE
    runGaugeShowcase(hardReset);
    return 0;
#else

    if (!hardReset)
        SYS_hardReset();

    JOY_init();

    setupDemoScreen();

    /* -----------------------------------------------------------------------------
     2. Gauge Init based on the GaugeDefinition
     ----------------------------------------------------------------------------- */
    Gauge g_demoGauge;
    if (!Gauge_init(&g_demoGauge, &g_demoDefinition, TILE_USER_INDEX))
    {
        KLog("Gauge_init failed");
        return 1;
    }

    while (TRUE)
    {
        /* -----------------------------------------------------------------------------
         3. Events happens : increase or decrease of the gauge
        ----------------------------------------------------------------------------- */
        handleInput(&g_demoGauge);

        /* -----------------------------------------------------------------------------
         4. Update it each frame  - it handles the no-op so you don't have to
         ----------------------------------------------------------------------------- */
        Gauge_update(&g_demoGauge);

        SYS_doVBlankProcess();
    }

    /* --------------------------------------------------------------------------------
    5. Dont forget to release when not needed anymore (i.e. between levels and menus...)
    --------------------------------------------------------------------------------- */
    Gauge_release(&g_demoGauge);

    return 0;
#endif
}

#if !GAUGE_DEMO_USE_SHOWCASE

static void setupDemoScreen(void)
{
    VDP_setPlaneSize(32, 32, TRUE);
    VDP_setBackgroundColor(0);
    VDP_setTextPalette(PAL0);
    VDP_setTextPlane(BG_A);
    PAL_setPalette(PAL0, gauge_palette.data, DMA);

    VDP_drawText("Basic Gauge Demo", 1, 0);
    VDP_drawText("A: increase", 1, 2);
    VDP_drawText("B: decrease", 1, 3);
}

static u16 g_previousPad = 0;
static void handleInput(Gauge * g_demoGauge)
{
    const u16 padState = JOY_readJoypad(JOY_1);
    const u16 pressed = (u16)((padState ^ g_previousPad) & padState);
    g_previousPad = padState;

    if (pressed & BUTTON_A)
        Gauge_increase(g_demoGauge, 8);

    if (pressed & BUTTON_B)
        Gauge_decrease(g_demoGauge, 8);
}
#endif
