#include <genesis.h>
#include "showcase_internal.h"

DemoCaseRuntime g_activeCases[DEMO_MAX_CASES_PER_SCREEN];
u8 g_activeCaseCount = 0;

u16 g_previousPad = 0;
u8 g_selectedScreen = 0;
u8 g_selectedCase = 0;
u8 g_buttonAHoldFrames = 0;
u8 g_buttonBHoldFrames = 0;
u16 g_frameCount = 0;
Sprite *g_cursorSprite = NULL;

static Gauge *buildGaugeFromDefinition(const GaugeDefinition *definition,
                                       u16 *nextVram)
{
    Gauge *gauge = NULL;
    /* Each case receives the next free VRAM tile index.
     * Gauge_init() uses it as the start of that gauge's fixed BG allocation.
     */
    const u16 vramBase = *nextVram;

    /* Gauge_init() consumes the authored definition from showcase_data.c and
     * builds the full runtime object once. If it fails, the caller uses the
     * module-global build error diagnostic to explain why.
     */
    gauge = Gauge_init(definition, vramBase);
    if (!gauge)
        return NULL;

    /* Gauge_getNextVramIndex() returns the first tile index still free after
     * this instance's allocation. Chaining that value keeps screen loading
     * monotonic and easy to reason about.
     */
    *nextVram = Gauge_getNextVramIndex(gauge);
    return gauge;
}

/* Demo-only build failure logging.
 * Gauge_init() stores the last build failure in a module-global diagnostic slot.
 * tryBuildCase() fails when its Gauge_init() call returns NULL, so the showcase
 * must read Gauge_getLastBuildError() immediately after that failure
 * path, before another build attempt overwrites the diagnostic.
 *
 * Example trigger:
 * - in showcase_data.c, temporarily give a case an invalid lane definition
 *   (for example a lane hole)
 * - or keep a PIP case but assign an invalid PIP skin geometry
 * - reload that screen in the showcase
 * This logger will then print the failing screen title, the case description,
 * plus the public build error code and text exposed by the Gauge module.
 */
static void logShowcaseBuildFailure(const DemoScreenSource *screen,
                                    u8 screenIndex,
                                    u8 caseIndex,
                                    const DemoCaseSource *sourceCase)
{
    GaugeBuildError buildError = Gauge_getLastBuildError();

    KLog("SHOWCASE: case build failed");
    KLog_U2("SHOWCASE screen: ", screenIndex,
            " case: ", caseIndex);

    if (screen && screen->title)
    {
        KLog("SHOWCASE screen title:");
        KLog((char *)screen->title);
    }

    if (sourceCase && sourceCase->descriptionLine1)
    {
        KLog("SHOWCASE case description:");
        KLog((char *)sourceCase->descriptionLine1);
    }

    KLog_U1("SHOWCASE build error code: ", (u16)buildError);
    KLog("SHOWCASE build error text:");
    KLog((char *)Gauge_getLastBuildErrorText());
}

void releaseActiveScreen(void)
{
    u8 caseIndex;

    for (caseIndex = 0; caseIndex < g_activeCaseCount; caseIndex++)
    {
        if (g_activeCases[caseIndex].gauge)
        {
            /* Gauge_release() is the exact runtime counterpart of Gauge_init().
             * The showcase always releases screen-local instances before it
             * rebuilds another screen.
             */
            Gauge_release(g_activeCases[caseIndex].gauge);
            g_activeCases[caseIndex].gauge = NULL;
        }
    }

    memset(g_activeCases, 0, sizeof(g_activeCases));
    g_activeCaseCount = 0;
}

/* Each showcase case owns exactly one Gauge instance.
 * If that build fails, the case is skipped and the screen keeps loading.
 * This keeps the demo usable while the build-failure log points at the broken
 * case and the Gauge module exposes the exact error text.
 */
u8 tryBuildCase(const DemoCaseSource *sourceCase,
                DemoCaseRuntime *runtimeCase,
                u16 *nextVram)
{
    memset(runtimeCase, 0, sizeof(*runtimeCase));
    /* Copy the short HUD metadata from the authored case, then build the
     * single Gauge instance that this runtime case owns.
     */
    runtimeCase->descriptionLine1 = sourceCase->descriptionLine1;
    runtimeCase->descriptionLine2 = sourceCase->descriptionLine2;
    runtimeCase->descriptionLine3 = sourceCase->descriptionLine3;
    runtimeCase->cursorTileX = sourceCase->cursorTileX;
    runtimeCase->cursorTileY = sourceCase->cursorTileY;
    runtimeCase->stepAmount = sourceCase->stepAmount;
    runtimeCase->vramBase = *nextVram;
    runtimeCase->gauge = buildGaugeFromDefinition(sourceCase->definition, nextVram);

    if (!runtimeCase->gauge)
    {
        memset(runtimeCase, 0, sizeof(*runtimeCase));
        return 0;
    }

    return 1;
}

void loadScreen(u8 screenIndex)
{
    const DemoScreenSource *screen = &g_screens[screenIndex];
    u8 caseIndex;
    u8 activeCaseCount = 0;
    /* The showcase rebuilds each screen from TILE_USER_INDEX and then allocates
     * monotonically as each case succeeds. That keeps VRAM usage predictable
     * and makes the HUD's "VRAM Base / Tiles used" line easy to explain.
     */
    u16 nextVram = TILE_USER_INDEX;

    releaseActiveScreen();
    /* The showcase redraws the planes and HUD every time a screen changes, but
     * the authored GaugeDefinition data remains static in showcase_data.c.
     */
    clearScreenPlanes();
    drawHudStatic();

    for (caseIndex = 0; caseIndex < screen->caseCount; caseIndex++)
    {
        DemoCaseRuntime runtimeCase;

        if (!tryBuildCase(&screen->cases[caseIndex], &runtimeCase, &nextVram))
        {
            logShowcaseBuildFailure(screen,
                                    screenIndex,
                                    caseIndex,
                                    &screen->cases[caseIndex]);
            continue;
        }

        g_activeCases[activeCaseCount] = runtimeCase;
        activeCaseCount++;
    }

    g_activeCaseCount = activeCaseCount;

    if (g_activeCaseCount == 0)
        g_selectedCase = 0;
    else if (g_selectedCase >= g_activeCaseCount)
        g_selectedCase = 0;

    updateHudDynamic();
    updateCursorSprite();
}

DemoCaseRuntime *getSelectedCase(void)
{
    if (g_activeCaseCount == 0 || g_selectedCase >= g_activeCaseCount)
        return NULL;

    return &g_activeCases[g_selectedCase];
}

void applyActionToSelectedCase(u8 isIncrease, u16 amount)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();

    if (!selectedCase || !selectedCase->gauge)
        return;

    /* These public helpers use the timings authored in the GaugeDefinition's
     * behavior block. That is why a normal press follows the case's configured
     * hold / blink rules instead of changing the value immediately.
     */
    if (isIncrease)
        Gauge_increase(selectedCase->gauge, amount);
    else
        Gauge_decrease(selectedCase->gauge, amount);
}

void applyImmediateActionToSelectedCase(u8 isIncrease, u16 amount)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();

    if (!selectedCase || !selectedCase->gauge)
        return;

    /* The Ex variants let the showcase override hold / blink timings.
     * Passing 0,0 makes held-button repeat advance immediately so the user can
     * inspect fine-grained intermediate states one point at a time.
     */
    if (isIncrease)
        Gauge_increaseEx(selectedCase->gauge, amount, 0, 0);
    else
        Gauge_decreaseEx(selectedCase->gauge, amount, 0, 0);
}

void changeSelection(s16 direction)
{
    if (g_activeCaseCount == 0)
        return;

    if (direction < 0)
        g_selectedCase = (u8)((g_selectedCase + g_activeCaseCount - 1) % g_activeCaseCount);
    else
        g_selectedCase = (u8)((g_selectedCase + 1) % g_activeCaseCount);

    g_buttonAHoldFrames = 0;
    g_buttonBHoldFrames = 0;
    updateHudDynamic();
    updateCursorSprite();
}

void changeScreen(void)
{
    g_selectedScreen = (u8)((g_selectedScreen + 1) % DEMO_SCREEN_COUNT);
    g_selectedCase = 0;
    g_buttonAHoldFrames = 0;
    g_buttonBHoldFrames = 0;
    loadScreen(g_selectedScreen);
}

void updateActiveGauges(void)
{
    u8 caseIndex;

    for (caseIndex = 0; caseIndex < g_activeCaseCount; caseIndex++)
    {
        Gauge *gauge = g_activeCases[caseIndex].gauge;
        if (gauge)
        {
            /* Gauge_update() is the module's per-frame tick.
             * It advances animations, resolves trail / gain / blink state, and
             * emits any VRAM or tilemap work needed for this frame.
             */
            Gauge_update(gauge);
        }
    }
}
