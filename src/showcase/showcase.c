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
static void clearHudLines(u16 firstY, u16 lastY);
static void drawHudScreenHeader(const DemoScreenSource *screen);
static void drawHudCaseDescription(const DemoCaseRuntime *selectedCase);
static void drawHudSelectedGaugeStats(const DemoCaseRuntime *selectedCase);
static u16 getSelectedCaseStepAmount(void);
static void toggleSelectedCaseDebugMode(void);
static void handleSelectionInput(u16 pressed);
static void handleActionPresses(u16 pressed, u16 stepAmount);
static void handleActionRepeat(u16 held);

/* The showcase formats HUD strings manually instead of using sprintf().
 * That keeps code size, stack usage, and CPU cost small and predictable on
 * SGDK / Mega Drive hardware.
 */
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

static void drawHudSelectedGaugeStats(const DemoCaseRuntime *selectedCase)
{
    clearHudLine(26);
    if (!selectedCase || !selectedCase->gauge)
        return;

    {
        char lineBuffer[41];
        /* Runtime case data gives us both sides of the VRAM story:
         * - vramBase: where Gauge_init() started allocating this gauge
         * - Gauge_getNextVramIndex(): first tile index still free after it
         * Their difference is the number of BG tiles consumed by the selected
         * Gauge instance, which is exactly what this HUD line teaches.
         */
        const Gauge *gauge = selectedCase->gauge;
        const u16 vramBase = selectedCase->vramBase;
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

static void clearHudLines(u16 firstY, u16 lastY)
{
    u16 y;

    for (y = firstY; y <= lastY; y++)
        clearHudLine(y);
}

static void drawHudScreenHeader(const DemoScreenSource *screen)
{
    /* Screen titles stay short in the HUD, but they map back to the much more
     * detailed comments in showcase_data.c.
     */
    VDP_drawText("Screen:", 1, 3);
    VDP_drawText(screen->title, 9, 3);
}

static void drawHudCaseDescription(const DemoCaseRuntime *selectedCase)
{
    const char *descriptionLine1 = s_noCaseDescriptionLine1;
    const char *descriptionLine2 = s_noCaseDescriptionLine2;
    const char *descriptionLine3 = s_noCaseDescriptionLine3;

    if (selectedCase)
    {
        descriptionLine1 = selectedCase->descriptionLine1;
        descriptionLine2 = selectedCase->descriptionLine2;
        descriptionLine3 = selectedCase->descriptionLine3;
    }

    VDP_drawText(descriptionLine1, 1, 5);
    VDP_drawText(descriptionLine2, 1, 6);
    VDP_drawText(descriptionLine3, 1, 7);
}

void drawHudStatic(void)
{
    /* This is the part of the HUD that never changes within a screen:
     * controls and the visual separator. Dynamic case-specific content is
     * redrawn separately in updateHudDynamic().
     */
    clearHudLines(0, 10);
    clearHudLine(26);

    VDP_drawText("A: increase   B: decrease", 1, 0);
    VDP_drawText("U/L prev  D/R next  C: change screen", 1, 1);
    VDP_drawText(s_separator, 0, 9);
}

/* -----------------------------------------------------------------------------
   Runtime helpers
   ----------------------------------------------------------------------------- */
void updateHudDynamic(void)
{
    const DemoScreenSource *screen = &g_screens[g_selectedScreen];
    const DemoCaseRuntime *selectedCase = getSelectedCase();

    clearHudLines(2, 10);
    drawHudScreenHeader(screen);
    drawHudCaseDescription(selectedCase);
    VDP_drawText(s_separator, 0, 9);
    drawHudSelectedGaugeStats(selectedCase);
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
        /* Cases store cursor coordinates in tiles, not pixels.
         * Converting with *8 makes the link with the tile-based showcase layout
         * explicit for a junior reading the code.
         */
        const s16 cursorX = (s16)(selectedCase->cursorTileX * 8);
        const s16 cursorY = (s16)(selectedCase->cursorTileY * 8);

        SPR_setPosition(g_cursorSprite, cursorX, cursorY);
        SPR_setVisibility(g_cursorSprite, VISIBLE);
    }
}

static void setupDemoScreen(void)
{
    /* The showcase uses the WINDOW plane for both text HUD and gauge tiles.
     * That keeps the demo simple: one plane to clear, one plane to inspect,
     * and one obvious place where Gauge writes its background tiles.
     */
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
    /* Input flow stays deliberately simple:
     * 1. optional debug toggle
     * 2. screen switch
     * 3. case navigation
     * 4. normal press actions
     * 5. held-button repeat actions
     */
    if (pressed & BUTTON_START)
    {
        /* Demo-only: toggles module debug visualization.
         * Not required for normal integration.
         */
        toggleSelectedCaseDebugMode();
    }

    if (pressed & BUTTON_C)
    {
        changeScreen();
        return;
    }

    handleSelectionInput(pressed);
    handleActionPresses(pressed, getSelectedCaseStepAmount());
    handleActionRepeat(held);
}

static u16 getSelectedCaseStepAmount(void)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();
    /* Each authored case chooses a meaningful step size.
     * Fill cases usually jump by more than one unit, while PIP cases often
     * prefer small steps so quantized states remain easy to inspect.
     */
    return selectedCase ? selectedCase->stepAmount : DEMO_FILL_STEP;
}

static void toggleSelectedCaseDebugMode(void)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();
    Gauge *gauge = selectedCase ? selectedCase->gauge : NULL;

    if (!gauge)
        return;

    /* Gauge debug mode is owned by the module itself.
     * The showcase just toggles it on the selected instance so a learner can
     * inspect one case at a time without changing authored data.
     */
    Gauge_setDebugMode(gauge, Gauge_getDebugMode(gauge) ? 0 : 1);
}

static void handleSelectionInput(u16 pressed)
{
    if (pressed & (BUTTON_UP | BUTTON_LEFT))
        changeSelection(-1);
    else if (pressed & (BUTTON_DOWN | BUTTON_RIGHT))
        changeSelection(1);
}

static void handleActionPresses(u16 pressed, u16 stepAmount)
{
    if (pressed & BUTTON_A)
    {
        /* A normal press uses the case step and the GaugeDefinition's authored
         * behavior timings, so the showcase demonstrates the default API path.
         */
        applyActionToSelectedCase(1, stepAmount);
        g_buttonAHoldFrames = 0;
    }

    if (pressed & BUTTON_B)
    {
        /* B mirrors A but in the loss direction.
         * Keeping the two paths symmetric makes it easier to compare gain and
         * damage behavior in the selected gauge.
         */
        applyActionToSelectedCase(0, stepAmount);
        g_buttonBHoldFrames = 0;
    }
}

static void handleActionRepeat(u16 held)
{
    /* Held buttons switch to immediate 1-point steps after a short delay.
     * This makes it easy to inspect intermediate Gauge states without changing
     * the normal "one press = case stepAmount" behavior.
     */
    if (held & BUTTON_A)
    {
        g_buttonAHoldFrames++;
        if (g_buttonAHoldFrames >= DEMO_REPEAT_DELAY &&
            (g_frameCount & DEMO_REPEAT_MASK) == 0)
        {
            applyImmediateActionToSelectedCase(1, 1);
        }
    }
    else
    {
        g_buttonAHoldFrames = 0;
    }

    if (held & BUTTON_B)
    {
        g_buttonBHoldFrames++;
        if (g_buttonBHoldFrames >= DEMO_REPEAT_DELAY &&
            (g_frameCount & DEMO_REPEAT_MASK) == 0)
        {
            applyImmediateActionToSelectedCase(0, 1);
        }
    }
    else
    {
        g_buttonBHoldFrames = 0;
    }
}

static void loadAllPalettes(void)
{
    /* The showcase loads all palettes up front.
     * Gauge instances then select a palette index in their authored
     * definitions, which lets the demo compare lane and skin setups without
     * doing palette swaps during interaction.
     */
    PAL_setPalette(PAL0, gauge_palette_0.data, DMA);
    PAL_setPalette(PAL1, gauge_palette_1.data, DMA);
    PAL_setPalette(PAL2, gauge_palette_2.data, DMA);
    PAL_setPalette(PAL3, gauge_palette_0.data, DMA);
}

/* -----------------------------------------------------------------------------
   Showcase entry point
   ----------------------------------------------------------------------------- */
void runGaugeShowcase(bool hardReset)
{
    /* The demo starts from a known SGDK state, then loads assets and the first
     * screen before entering the main frame loop.
     */
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

        /* Frame order:
         * 1. sample input
         * 2. update selection / actions
         * 3. tick every active Gauge instance
         * 4. flush sprite state
         * 5. let SGDK perform deferred VBlank work
         */
        g_frameCount++;
        g_previousPad = padState;

        handleInput(pressed, padState);
        updateActiveGauges();
        SPR_update();
        SYS_doVBlankProcess();
    }

    return;
}
