#include <genesis.h>
#include "showcase_internal.h"

Gauge *g_runtimeGaugePool[DEMO_MAX_ACTIVE_GAUGES];
DemoCaseRuntime g_activeCases[DEMO_MAX_CASES_PER_SCREEN];
u8 g_activeCaseCount = 0;
u8 g_activeGaugeCount = 0;

u16 g_previousPad = 0;
u8 g_selectedScreen = 0;
u8 g_selectedCase = 0;
u8 g_holdA = 0;
u8 g_holdB = 0;
u16 g_frameCount = 0;
Sprite *g_cursorSprite = NULL;

Gauge *buildGaugeFromDefinition(const GaugeDefinition *definition,
                                u16 *nextVram)
{
    Gauge *gauge = NULL;
    const u16 vramBase = *nextVram;

    gauge = Gauge_init(definition, vramBase);
    if (!gauge)
        return NULL;

    *nextVram = Gauge_getNextVramIndex(gauge);
    return gauge;
}

void releaseActiveScreen(void)
{
    u8 gaugeIndex;

    for (gaugeIndex = 0; gaugeIndex < g_activeGaugeCount; gaugeIndex++)
    {
        Gauge_release(g_runtimeGaugePool[gaugeIndex]);
        g_runtimeGaugePool[gaugeIndex] = NULL;
    }

    memset(g_runtimeGaugePool, 0, sizeof(g_runtimeGaugePool));
    memset(g_activeCases, 0, sizeof(g_activeCases));
    g_activeCaseCount = 0;
    g_activeGaugeCount = 0;
}

u8 tryBuildCase(const DemoCaseSource *sourceCase,
                DemoCaseRuntime *runtimeCase,
                u8 *gaugeCursor,
                u16 *nextVram)
{
    const u8 startGaugeIndex = *gaugeCursor;
    u8 localGaugeCount = 0;
    u8 sourceGaugeIndex;

    memset(runtimeCase, 0, sizeof(*runtimeCase));
    runtimeCase->descriptionLine1 = sourceCase->descriptionLine1;
    runtimeCase->descriptionLine2 = sourceCase->descriptionLine2;
    runtimeCase->descriptionLine3 = sourceCase->descriptionLine3;
    runtimeCase->cursorTileX = sourceCase->cursorTileX;
    runtimeCase->cursorTileY = sourceCase->cursorTileY;
    runtimeCase->stepAmount = sourceCase->stepAmount;

    for (sourceGaugeIndex = 0; sourceGaugeIndex < sourceCase->gaugeCount; sourceGaugeIndex++)
    {
        Gauge *gauge = NULL;
        const DemoGaugeSource *sourceGauge = &sourceCase->gauges[sourceGaugeIndex];

        if (*gaugeCursor >= DEMO_MAX_ACTIVE_GAUGES)
            break;

        runtimeCase->vramBaseByGauge[localGaugeCount] = *nextVram;
        gauge = buildGaugeFromDefinition(sourceGauge->definition, nextVram);
        if (!gauge)
            break;

        g_runtimeGaugePool[*gaugeCursor] = gauge;
        runtimeCase->gauges[localGaugeCount] = gauge;
        localGaugeCount++;
        (*gaugeCursor)++;
    }

    if (localGaugeCount != sourceCase->gaugeCount)
    {
        while (*gaugeCursor > startGaugeIndex)
        {
            (*gaugeCursor)--;
            Gauge_release(g_runtimeGaugePool[*gaugeCursor]);
            g_runtimeGaugePool[*gaugeCursor] = NULL;
        }
        memset(runtimeCase, 0, sizeof(*runtimeCase));
        return 0;
    }

    runtimeCase->gaugeCount = localGaugeCount;
    return 1;
}

void loadScreen(u8 screenIndex)
{
    const DemoScreenSource *screen = &g_screens[screenIndex];
    u8 caseIndex;
    u8 activeCaseCount = 0;
    u8 gaugeCursor = 0;
    u16 nextVram = TILE_USER_INDEX;

    releaseActiveScreen();
    clearScreenPlanes();
    drawHudStatic();

    for (caseIndex = 0; caseIndex < screen->caseCount; caseIndex++)
    {
        DemoCaseRuntime runtimeCase;

        if (!tryBuildCase(&screen->cases[caseIndex], &runtimeCase, &gaugeCursor, &nextVram))
            continue;

        g_activeCases[activeCaseCount] = runtimeCase;
        activeCaseCount++;
    }

    g_activeCaseCount = activeCaseCount;
    g_activeGaugeCount = gaugeCursor;

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
    u8 gaugeIndex;

    if (!selectedCase)
        return;

    for (gaugeIndex = 0; gaugeIndex < selectedCase->gaugeCount; gaugeIndex++)
    {
        if (isIncrease)
            Gauge_increase(selectedCase->gauges[gaugeIndex], amount);
        else
            Gauge_decrease(selectedCase->gauges[gaugeIndex], amount);
    }
}

void applyImmediateActionToSelectedCase(u8 isIncrease, u16 amount)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();
    u8 gaugeIndex;

    if (!selectedCase)
        return;

    for (gaugeIndex = 0; gaugeIndex < selectedCase->gaugeCount; gaugeIndex++)
    {
        if (isIncrease)
            Gauge_increaseEx(selectedCase->gauges[gaugeIndex], amount, 0, 0);
        else
            Gauge_decreaseEx(selectedCase->gauges[gaugeIndex], amount, 0, 0);
    }
}

void changeSelection(s16 direction)
{
    if (g_activeCaseCount == 0)
        return;

    if (direction < 0)
        g_selectedCase = (u8)((g_selectedCase + g_activeCaseCount - 1) % g_activeCaseCount);
    else
        g_selectedCase = (u8)((g_selectedCase + 1) % g_activeCaseCount);

    g_holdA = 0;
    g_holdB = 0;
    updateHudDynamic();
    updateCursorSprite();
}

void changeScreen(void)
{
    g_selectedScreen = (u8)((g_selectedScreen + 1) % DEMO_SCREEN_COUNT);
    g_selectedCase = 0;
    g_holdA = 0;
    g_holdB = 0;
    loadScreen(g_selectedScreen);
}

void updateActiveGauges(void)
{
    u8 gaugeIndex;

    for (gaugeIndex = 0; gaugeIndex < g_activeGaugeCount; gaugeIndex++)
    {
        Gauge *gauge = g_runtimeGaugePool[gaugeIndex];
        if (gauge)
            Gauge_update(gauge);
    }
}
