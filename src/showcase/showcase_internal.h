#ifndef SHOWCASE_INTERNAL_H
#define SHOWCASE_INTERNAL_H

#include <genesis.h>
#include "../gauge/gauge.h"

#define DEMO_SCREEN_COUNT          4
#define DEMO_MAX_CASES_PER_SCREEN  8

/* Showcase interaction defaults.
 * Fill cases move faster than PIP cases because their authored max values and
 * visible state density are different.
 */
#define DEMO_FILL_STEP             4
#define DEMO_PIP_STEP              1
/* Held-button repeat starts after a short delay, then uses the frame counter
 * mask to fire at a stable cadence without extra timers.
 */
#define DEMO_REPEAT_DELAY          12
#define DEMO_REPEAT_MASK           3

/* Default timings shared by most authored Gauge behaviors in the showcase.
 * Individual cases still override damage / gain modes or animation shifts when
 * a specific teaching example needs to highlight them.
 */
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

/* HUD descriptions are intentionally short.
 * They are only meant to summarize the selected case on screen.
 * The real pedagogical explanation for each case lives in showcase_data.c,
 * where comments can explain the purpose, the interesting fields, and the
 * expected visual result without consuming HUD space.
 */
typedef struct
{
    /* Three short HUD lines shown for the selected case.
     * They help identify the case at runtime, but they are not meant to be a
     * full explanation of the authored GaugeDefinition.
     */
    const char *descriptionLine1;
    const char *descriptionLine2;
    const char *descriptionLine3;
    /* Cursor position in tile coordinates.
     * The showcase cursor sprite uses these coordinates directly, so a junior
     * can relate the selection grid to the authored case table.
     */
    u8 cursorTileX;
    u8 cursorTileY;
    /* Step used by A / B presses for this case.
     * Fill cases use a larger step so value changes stay readable, while PIP
     * cases usually use smaller steps to expose quantization states.
     */
    u16 stepAmount;
    /* Static authored GaugeDefinition for this case.
     * This points into showcase_data.c and is passed unchanged to Gauge_init().
     */
    const GaugeDefinition *definition;
} DemoCaseSource;

typedef struct
{
    /* Short title shown in the HUD header.
     * Each screen groups a coherent family of Gauge examples.
     */
    const char *title;
    const DemoCaseSource *cases;
    u8 caseCount;
} DemoScreenSource;

/* Runtime mirror of one showcase case.
 * The case still owns short HUD text, but now also carries the single built
 * Gauge instance and the VRAM base used to build it.
 */
typedef struct
{
    const char *descriptionLine1;
    const char *descriptionLine2;
    const char *descriptionLine3;
    u8 cursorTileX;
    u8 cursorTileY;
    u16 stepAmount;
    /* Built Gauge instance owned by the case at runtime.
     * This is the object returned by Gauge_init() and later consumed by
     * Gauge_update(), Gauge_increase(), Gauge_decrease(), and Gauge_release().
     */
    Gauge *gauge;
    /* First VRAM tile index reserved for this Gauge instance.
     * The HUD compares this base with Gauge_getNextVramIndex() to show how many
     * background tiles the selected gauge consumes.
     */
    u16 vramBase;
} DemoCaseRuntime;

/* Active runtime cases for the current screen.
 * The showcase rebuilds them when the screen changes, so this array is the
 * live bridge between static authored data and the running Gauge instances.
 */
extern DemoCaseRuntime g_activeCases[DEMO_MAX_CASES_PER_SCREEN];
extern u8 g_activeCaseCount;
extern u16 g_previousPad;
/* Current screen and case selection.
 * These drive both the HUD and the action routing toward the selected gauge.
 */
extern u8 g_selectedScreen;
extern u8 g_selectedCase;
/* Hold counters for A / B repeat behavior.
 * They are kept outside the input handler so the repeat state survives from one
 * frame to the next.
 */
extern u8 g_buttonAHoldFrames;
extern u8 g_buttonBHoldFrames;
/* Monotonic frame counter used by repeat timing.
 * The repeat mask keeps held-button stepping simple and deterministic.
 */
extern u16 g_frameCount;
extern Sprite *g_cursorSprite;

extern const DemoScreenSource g_screens[DEMO_SCREEN_COUNT];

void releaseActiveScreen(void);
u8 tryBuildCase(const DemoCaseSource *sourceCase,
                DemoCaseRuntime *runtimeCase,
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
