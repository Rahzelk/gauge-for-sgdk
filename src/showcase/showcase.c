#include <genesis.h>
#include "showcase_internal.h"
#include "gauge_assets.h"

/* =============================================================================
   Gauge showcase
   ============================================================================= */

static const char s_blankLine[] = "                                        ";
static const char s_separator[] = "----------------------------------------";
static const char s_noCaseDescriptionLine1[] = "No buildable showcase case on screen.";
static const char s_noCaseDescriptionLine2[] = "Check this screen definition and data.";
static const char s_noCaseDescriptionLine3[] = "No gauge stats are available here.";

static void clearHudLine(u16 y);

static u16 appendString(char *buffer, u16 offset, const char *text)
{
    while (text && *text != '\0' && offset < 40)
        buffer[offset++] = *text++;

    return offset;
}

static u16 appendU16Decimal(char *buffer, u16 offset, u16 value, u8 minDigits)
{
    char temp[5];
    u8 digitCount = 0;
    u8 padCount = 0;

    do
    {
        temp[digitCount++] = (char)('0' + (value % 10));
        value /= 10;
    }
    while (value != 0 && digitCount < sizeof(temp));

    if (digitCount < minDigits)
        padCount = (u8)(minDigits - digitCount);

    while (padCount > 0 && offset < 40)
    {
        buffer[offset++] = '0';
        padCount--;
    }

    while (digitCount > 0 && offset < 40)
        buffer[offset++] = temp[--digitCount];

    return offset;
}

static void drawSelectedGaugeVramLine(void)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();

    clearHudLine(23);
    if (!selectedCase || selectedCase->gaugeCount == 0 || !selectedCase->gauges[0])
        return;

    {
        char lineBuffer[41];
        const Gauge *gauge = selectedCase->gauges[0];
        const u16 vramBase = selectedCase->vramBaseByGauge[0];
        const u16 nextVramIndex = Gauge_getNextVramIndex(gauge);
        const u16 tilesUsed = (nextVramIndex >= vramBase)
            ? (u16)(nextVramIndex - vramBase)
            : 0;
        u16 offset = 0;

        memset(lineBuffer, 0, sizeof(lineBuffer));
        offset = appendString(lineBuffer, offset, "VRAM Base = ");
        offset = appendU16Decimal(lineBuffer, offset, vramBase, 4);
        offset = appendString(lineBuffer, offset, " --- Tiles used = ");
        offset = appendU16Decimal(lineBuffer, offset, tilesUsed, 1);
        lineBuffer[offset] = '\0';
        VDP_drawText(lineBuffer, 1, 26);
    }
}

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
    clearHudLine(7);
    clearHudLine(8);
    clearHudLine(9);
    clearHudLine(10);
    clearHudLine(23);

    VDP_drawText("A: increase   B: decrease", 1, 0);
    VDP_drawText("U/L prev  D/R next  C: change screen", 1, 1);
    VDP_drawText("Screen:", 1, 3);
    VDP_drawText(s_separator, 0, 9);
}

/* -----------------------------------------------------------------------------
   Runtime helpers
   ----------------------------------------------------------------------------- */
void updateHudDynamic(void)
{
    const DemoScreenSource *screen = &g_screens[g_selectedScreen];
    const char *selectedDescriptionLine1 = s_noCaseDescriptionLine1;
    const char *selectedDescriptionLine2 = s_noCaseDescriptionLine2;
    const char *selectedDescriptionLine3 = s_noCaseDescriptionLine3;

    if (g_activeCaseCount > 0 && g_selectedCase < g_activeCaseCount)
    {
        selectedDescriptionLine1 = g_activeCases[g_selectedCase].descriptionLine1;
        selectedDescriptionLine2 = g_activeCases[g_selectedCase].descriptionLine2;
        selectedDescriptionLine3 = g_activeCases[g_selectedCase].descriptionLine3;
    }

    clearHudLine(2);
    clearHudLine(3);
    clearHudLine(4);
    clearHudLine(5);
    clearHudLine(6);
    clearHudLine(7);
    clearHudLine(8);
    clearHudLine(9);
    clearHudLine(10);
    VDP_drawText("Screen:", 1, 3);
    VDP_drawText(screen->title, 9, 3);
    VDP_drawText(selectedDescriptionLine1, 1, 5);
    VDP_drawText(selectedDescriptionLine2, 1, 6);
    VDP_drawText(selectedDescriptionLine3, 1, 7);
    VDP_drawText(s_separator, 0, 9);
    drawSelectedGaugeVramLine();
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
        /* Demo-only: toggles module debug visualization.
         * Not required for normal integration.
         */
        if (selectedCase && selectedCase->gaugeCount > 0)
        {
            const u8 enabled = selectedCase->gauges[0] ?
                (Gauge_getDebugMode(selectedCase->gauges[0]) ? 0 : 1) : 1;

            for (u8 gaugeIndex = 0; gaugeIndex < selectedCase->gaugeCount; gaugeIndex++)
            {
                Gauge *gauge = selectedCase->gauges[gaugeIndex];
                if (gauge)
                    Gauge_setDebugMode(gauge, enabled);
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
