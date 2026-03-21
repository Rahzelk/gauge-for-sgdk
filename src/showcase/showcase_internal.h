#ifndef SHOWCASE_INTERNAL_H
#define SHOWCASE_INTERNAL_H

#include <genesis.h>
#include "../gauge/gauge.h"

#define DEMO_SCREEN_COUNT          4
#define DEMO_MAX_CASES_PER_SCREEN  8
#define DEMO_MAX_GAUGES_PER_CASE   1
#define DEMO_MAX_ACTIVE_GAUGES     8

#define DEMO_FILL_STEP             4
#define DEMO_PIP_STEP              1
#define DEMO_REPEAT_DELAY          12
#define DEMO_REPEAT_MASK           3

#define DEMO_INCREASE_HOLD         24
#define DEMO_INCREASE_BLINK        16
#define DEMO_DECREASE_HOLD         20
#define DEMO_DECREASE_BLINK        48

#define DEMO_DEFAULT_BEHAVIOR_TIMINGS \
    .damageHoldFrames = DEMO_DECREASE_HOLD, \
    .damageBlinkFrames = DEMO_DECREASE_BLINK, \
    .gainHoldFrames = DEMO_INCREASE_HOLD, \
    .gainBlinkFrames = DEMO_INCREASE_BLINK

#define ARRAY_LEN(array)           ((u16)(sizeof(array) / sizeof((array)[0])))

typedef struct
{
    const GaugeDefinition *definition;
} DemoGaugeSource;

typedef struct
{
    const char *descriptionLine1;
    const char *descriptionLine2;
    const char *descriptionLine3;
    u8 cursorTileX;
    u8 cursorTileY;
    u16 stepAmount;
    u8 gaugeCount;
    DemoGaugeSource gauges[DEMO_MAX_GAUGES_PER_CASE];
} DemoCaseSource;

typedef struct
{
    const char *title;
    const DemoCaseSource *cases;
    u8 caseCount;
} DemoScreenSource;

typedef struct
{
    const char *descriptionLine1;
    const char *descriptionLine2;
    const char *descriptionLine3;
    u8 cursorTileX;
    u8 cursorTileY;
    u16 stepAmount;
    u8 gaugeCount;
    Gauge *gauges[DEMO_MAX_GAUGES_PER_CASE];
    u16 vramBaseByGauge[DEMO_MAX_GAUGES_PER_CASE];
} DemoCaseRuntime;

extern Gauge *g_runtimeGaugePool[DEMO_MAX_ACTIVE_GAUGES];
extern DemoCaseRuntime g_activeCases[DEMO_MAX_CASES_PER_SCREEN];
extern u8 g_activeCaseCount;
extern u8 g_activeGaugeCount;
extern u16 g_previousPad;
extern u8 g_selectedScreen;
extern u8 g_selectedCase;
extern u8 g_holdA;
extern u8 g_holdB;
extern u16 g_frameCount;
extern Sprite *g_cursorSprite;

extern const DemoScreenSource g_screens[DEMO_SCREEN_COUNT];

Gauge *buildGaugeFromDefinition(const GaugeDefinition *definition,
                                u16 *nextVram);
void releaseActiveScreen(void);
u8 tryBuildCase(const DemoCaseSource *sourceCase,
                DemoCaseRuntime *runtimeCase,
                u8 *gaugeCursor,
                u16 *nextVram);
void loadScreen(u8 screenIndex);
DemoCaseRuntime *getSelectedCase(void);
void applyActionToSelectedCase(u8 isIncrease, u16 amount);
void applyImmediateActionToSelectedCase(u8 isIncrease, u16 amount);
void changeSelection(s16 direction);
void changeScreen(void);
void updateActiveGauges(void);

void drawHudStatic(void);
void updateHudDynamic(void);
void updateCursorSprite(void);
void clearScreenPlanes(void);

#endif
