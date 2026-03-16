#include <genesis.h>
#include "showcase_internal.h"
#include "gauge_assets.h"

/* =============================================================================
   Gauge showcase
   ============================================================================= */

static const char s_blankLine[] = "                                        ";
static const char s_separator[] = "----------------------------------------";

static void clearHudLine(u16 y)
{
    VDP_drawText(s_blankLine, 0, y);
}

void drawHudStatic(void)
{
    clearHudLine(0);
    clearHudLine(1);
    clearHudLine(2);
    clearHudLine(3);
    clearHudLine(4);
    clearHudLine(5);
    clearHudLine(6);

    VDP_drawText("A: increase   B: decrease", 1, 0);
    VDP_drawText("U/L prev  D/R next  C: change screen", 1, 1);
    VDP_drawText("Screen:", 1, 3);
    VDP_drawText("Selected:", 1, 4);
    VDP_drawText("VRAM mode:", 1, 5);
    VDP_drawText(s_separator, 0, 6);
}

/* -----------------------------------------------------------------------------
   Runtime helpers
   ----------------------------------------------------------------------------- */
void updateHudDynamic(void)
{
    const DemoScreenSource *screen = &g_screens[g_selectedScreen];
    const char *selectedLabel = "No case available";

    if (g_activeCaseCount > 0 && g_selectedCase < g_activeCaseCount)
        selectedLabel = g_activeCases[g_selectedCase].label;

    clearHudLine(2);
    clearHudLine(3);
    clearHudLine(4);
    clearHudLine(5);
    VDP_drawText("Screen:", 1, 3);
    VDP_drawText("Selected:", 1, 4);
    VDP_drawText(screen->title, 9, 3);
    VDP_drawText(selectedLabel, 11, 4);
}

void updateCursorSprite(void)
{
    if (!g_cursorSprite)
        return;

    if (g_activeCaseCount == 0 || g_selectedCase >= g_activeCaseCount)
    {
        SPR_setVisibility(g_cursorSprite, HIDDEN);
        return;
    }

    {
        const DemoCaseRuntime *selectedCase = &g_activeCases[g_selectedCase];
        const s16 cursorX = (s16)(selectedCase->cursorTileX * 8);
        const s16 cursorY = (s16)(selectedCase->cursorTileY * 8);

        SPR_setPosition(g_cursorSprite, cursorX, cursorY);
        SPR_setVisibility(g_cursorSprite, VISIBLE);
    }
}

static void setupDemoScreen(void)
{
    VDP_setPlaneSize(64, 32, TRUE);
    VDP_setWindowHPos(TRUE, 0);
    VDP_setWindowVPos(TRUE, 0);
    VDP_setBackgroundColor(0);
    VDP_setTextPalette(PAL0);
    VDP_setTextPlane(WINDOW);
}

void clearScreenPlanes(void)
{
    VDP_clearPlane(BG_A, TRUE);
    VDP_clearPlane(BG_B, TRUE);
    VDP_clearPlane(WINDOW, TRUE);
}

static void handleInput(u16 pressed, u16 held)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();
    u16 stepAmount = DEMO_FILL_STEP;

    if (pressed & BUTTON_START)
    {
        if (selectedCase && selectedCase->gaugeCount > 0)
        {
            const u8 enabled = selectedCase->gauges[0] ?
                (selectedCase->gauges[0]->debugMode ? 0 : 1) : 1;

            for (u8 gaugeIndex = 0; gaugeIndex < selectedCase->gaugeCount; gaugeIndex++)
            {
                Gauge *gauge = selectedCase->gauges[gaugeIndex];
                if (gauge)
                    gauge->debugMode = enabled;
            }
        }
    }

    if (pressed & BUTTON_C)
    {
        changeScreen();
        return;
    }

    if (pressed & (BUTTON_UP | BUTTON_LEFT))
        changeSelection(-1);
    else if (pressed & (BUTTON_DOWN | BUTTON_RIGHT))
        changeSelection(1);

    selectedCase = getSelectedCase();
    if (selectedCase)
        stepAmount = selectedCase->stepAmount;

    if (pressed & BUTTON_A)
    {
        applyActionToSelectedCase(1, stepAmount);
        g_holdA = 0;
    }

    if (pressed & BUTTON_B)
    {
        applyActionToSelectedCase(0, stepAmount);
        g_holdB = 0;
    }

    if (held & BUTTON_A)
    {
        g_holdA++;
        if (g_holdA >= DEMO_REPEAT_DELAY && (g_frameCount & DEMO_REPEAT_MASK) == 0)
            applyImmediateActionToSelectedCase(1, 1);
    }
    else
    {
        g_holdA = 0;
    }

    if (held & BUTTON_B)
    {
        g_holdB++;
        if (g_holdB >= DEMO_REPEAT_DELAY && (g_frameCount & DEMO_REPEAT_MASK) == 0)
            applyImmediateActionToSelectedCase(0, 1);
    }
    else
    {
        g_holdB = 0;
    }
}

static void loadAllPalettes(void)
{
    PAL_setPalette(PAL0, gauge_palette.data, DMA);
    PAL_setPalette(PAL1, gauge_blue_palette.data, DMA);
    PAL_setPalette(PAL2, gauge_lightblue_palette.data, DMA);
    PAL_setPalette(PAL3, gauge_palette.data, DMA);
}

/* -----------------------------------------------------------------------------
   Showcase entry point
   ----------------------------------------------------------------------------- */
void runGaugeShowcase(bool hardReset)
{
    if (!hardReset)
        SYS_hardReset();

    DMA_setMaxQueueSize(192);
    JOY_init();
    SPR_init();
    setupDemoScreen();
    loadAllPalettes();

    g_cursorSprite = SPR_addSprite(&menu_cursor_sprite,
                                   -32,
                                   -32,
                                   TILE_ATTR_FULL(PAL0, 1, FALSE, FALSE, 0));
    if (g_cursorSprite)
        SPR_setVisibility(g_cursorSprite, HIDDEN);

    updateCursorSprite();

    loadScreen(g_selectedScreen);
    SPR_update();
    SYS_doVBlankProcess();

    while (TRUE)
    {
        const u16 padState = JOY_readJoypad(JOY_1);
        const u16 pressed = (u16)((padState ^ g_previousPad) & padState);

        g_frameCount++;
        g_previousPad = padState;

        handleInput(pressed, padState);
        updateActiveGauges();
        SPR_update();
        SYS_doVBlankProcess();
    }

    return;
}
