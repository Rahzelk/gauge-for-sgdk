#ifndef GAUGE_H
#define GAUGE_H

#include <genesis.h>

/* =============================================================================
   GH-00 Public Module Overview

   Purpose:
   - Describe one HUD gauge declaratively through GaugeDefinition.
   - Build an opaque runtime handle through Gauge_init().
   - Drive the handle once per frame through Gauge_update() and the value
     mutation helpers.

   Search tags:
   - GH-10 : integration configuration
   - GH-20 : public constants and enums
   - GH-30 : public build-time data structures
   - GH-40 : public runtime API

   Public hierarchy:
   - Gauge   : opaque runtime handle
   - Lane    : one visual row or column of the gauge
   - Segment : contiguous authored portion of a lane with one skin
   - Cell    : one rendered 8x8 tile in fill order

   Rendering model:
   - Fill mode renders continuous bars through per-cell strip updates.
   - PIP mode renders discrete steps through compact authored states.
   - The module owns the VRAM layout internally; callers only provide the first
     VRAM tile index reserved for the gauge.
   - Build and runtime stay intentionally separate: Gauge_init() performs the
     expensive validation/build work once, then Gauge_update() stays allocation-free.
   ============================================================================= */

/* -----------------------------------------------------------------------------
   GH-10 Integration Configuration
   ----------------------------------------------------------------------------- */
#ifndef GAUGE_ENABLE_TRACE
/* Set to 1 to compile frame/cell trace support into the module. */
#define GAUGE_ENABLE_TRACE 0
#endif

#ifndef GAUGE_LUT_CAPACITY
/* Scratch capacity for runtime LUTs and the maximum supported logical value. */
#define GAUGE_LUT_CAPACITY 160
#endif

/* -----------------------------------------------------------------------------
   GH-20 Public Limits And Authoring Constants
   ----------------------------------------------------------------------------- */
#define GAUGE_MAX_SEGMENTS      12
#define GAUGE_MAX_LENGTH        16
#define GAUGE_MAX_LANES          8

#define GAUGE_PIXELS_PER_TILE    8
#define GAUGE_FILL_TILE_COUNT   45
#define GAUGE_TRAIL_TILE_COUNT  64
#define GAUGE_TILE_U32_COUNT     8

#define GAUGE_FILL_PX_MIN        0
#define GAUGE_FILL_PX_MAX        8

/* -----------------------------------------------------------------------------
   GH-21 Public Enumerations
   ----------------------------------------------------------------------------- */
typedef enum
{
    GAUGE_ORIENT_HORIZONTAL = 0,
    GAUGE_ORIENT_VERTICAL   = 1
} GaugeOrientation;

typedef enum
{
    GAUGE_FILL_FORWARD = 0,
    GAUGE_FILL_REVERSE = 1
} GaugeFillDirection;

typedef enum
{
    GAUGE_VALUE_MODE_FILL = 0,
    GAUGE_VALUE_MODE_PIP  = 1
} GaugeValueMode;

typedef GaugeValueMode GaugeMode;

#define GAUGE_MODE_FILL GAUGE_VALUE_MODE_FILL
#define GAUGE_MODE_PIP  GAUGE_VALUE_MODE_PIP

typedef enum
{
    GAUGE_STRIP_COVERAGE_FULL    = 0,
    GAUGE_STRIP_COVERAGE_HALF    = 1,
    GAUGE_STRIP_COVERAGE_QUARTER = 2
} GaugeStripCoverage;

typedef GaugeStripCoverage GaugePipCoverage;

#define GAUGE_PIP_COVERAGE_FULL    GAUGE_STRIP_COVERAGE_FULL
#define GAUGE_PIP_COVERAGE_HALF    GAUGE_STRIP_COVERAGE_HALF
#define GAUGE_PIP_COVERAGE_QUARTER GAUGE_STRIP_COVERAGE_QUARTER

typedef enum
{
    GAUGE_TRAIL_MODE_DISABLED                   = 0,
    GAUGE_TRAIL_MODE_FOLLOW                     = 1,
    GAUGE_TRAIL_MODE_STATIC_TRAIL               = 2,
    GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK = 3,
    GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK       = 4,
    GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK       = 5
} GaugeTrailMode;

typedef enum
{
    GAUGE_CAP_INACTIVE = 0,
    GAUGE_CAP_ACTIVE   = 1
} GaugeCapEndState;

typedef enum
{
    GAUGE_GAIN_MODE_DISABLED   = 0,
    GAUGE_GAIN_MODE_FOLLOW     = 1,
    GAUGE_GAIN_MODE_RESERVED_1 = 2,
    GAUGE_GAIN_MODE_RESERVED_2 = 3
} GaugeGainMode;

/* Result code of the most recent Gauge_init() call. */
typedef enum
{
    GAUGE_BUILD_OK = 0,
    GAUGE_BUILD_ERR_NULL_DEFINITION,
    GAUGE_BUILD_ERR_GAUGE_ALLOC,
    GAUGE_BUILD_ERR_INVALID_MODE,
    GAUGE_BUILD_ERR_INVALID_LANE,
    GAUGE_BUILD_ERR_LANE_HOLE,
    GAUGE_BUILD_ERR_LANE_WINDOW,
    GAUGE_BUILD_ERR_NO_BASE_LANE,
    GAUGE_BUILD_ERR_INVALID_SEGMENT,
    GAUGE_BUILD_ERR_INVALID_ORIGIN,
    GAUGE_BUILD_ERR_LAYOUT_BUILD,
    GAUGE_BUILD_ERR_RUNTIME_ALLOC
} GaugeBuildError;

/* -----------------------------------------------------------------------------
   GH-30 Public Build-Time Data Structures
   ----------------------------------------------------------------------------- */
/* Opaque runtime handle returned by Gauge_init() and destroyed by Gauge_release(). */
typedef struct Gauge Gauge;

/* BODY / END / BRIDGE strips use 45 tiles. TRAIL strips use 64 tiles. */
typedef struct
{
    const TileSet *body;    /* 45-tile BODY strip for regular interior cells */
    const TileSet *trail;   /* 64-tile TRAIL strip for trail-edge rendering */
    const TileSet *end;     /* 45-tile END strip for the value/trail frontier cell */
    const TileSet *bridge;  /* 45-tile BRIDGE strip for segment transitions */
} GaugeFillAssets;

/*
 * Public fill families:
 * - normal   : default rendering
 * - gain     : used while gain trail is active
 * - blinkOff : used on blink-off frames for both damage and gain blinking
 */
typedef struct
{
    GaugeFillAssets normal;    /* Base family used by the normal runtime state */
    GaugeFillAssets gain;      /* Alternate family used while gain trail is active */
    GaugeFillAssets blinkOff;  /* Alternate family used on OFF blink frames */
} GaugeFillSkin;

/*
 * Compact PIP source art for one segment style.
 *
 * State order inside the authored tileset must be:
 *   EMPTY, VALUE, LOSS, GAIN, BLINK_OFF
 * Only EMPTY and VALUE are mandatory; trailing states are optional.
 */
typedef struct
{
    const TileSet *tileset;        /* Compact state tileset for one segment style */
    u8 pipWidth;                   /* Width in tiles of one authored state */
    u8 pipHeight;                  /* Height in tiles of one authored state */
    GaugePipCoverage coverage;     /* FULL / HALF / QUARTER authored coverage */
} GaugePipSkin;

/* One authored skin family exposing both rendering modes.
 * Only the branch matching GaugeDefinition::mode is consumed at build time. */
typedef struct
{
    GaugeFillSkin fill;  /* Continuous bar rendering assets */
    GaugePipSkin pip;    /* Discrete pip rendering assets */
} GaugeSkin;

/* One authored segment inside a lane.
 * The build pipeline expands its cells into rendered fill-order cells. */
typedef struct
{
    u8 cells;               /* Number of cells contributed by this segment */
    const GaugeSkin *skin;  /* Visual skin used by those cells */
} GaugeSegment;

/*
 * One visual lane.
 *
 * offsetX / offsetY move the lane relative to GaugeDefinition origin.
 * firstValueCell lets a shorter lane start later along the base gauge span.
 */
typedef struct
{
    s8 offsetX;                            /* Relative X offset from GaugeDefinition origin */
    s8 offsetY;                            /* Relative Y offset from GaugeDefinition origin */
    u8 firstValueCell;                     /* First base-lane cell sampled by this lane */
    u8 overridePalette;                    /* 0 = use GaugeDefinition palette */
    u8 palette;                            /* Lane palette when overridePalette != 0 */
    GaugeSegment segments[GAUGE_MAX_SEGMENTS];
} GaugeLane;

/*
 * Runtime behavior shared by the whole gauge.
 *
 * damageMode selects the normal damage-state visuals and timing.
 * gainMode selects the gain-state visuals and timing.
 * blinkOff always refers to the OFF phase of the active blinking family.
 */
typedef struct
{
    u8 valueAnimEnabled;          /* 0 = instant value updates, 1 = animated value */
    u8 valueAnimShift;            /* Value animation speed divisor */
    GaugeTrailMode damageMode;    /* Visual behavior used for normal damage state */
    u16 criticalValue;            /* Threshold for critical damage modes */
    u8 damageHoldFrames;          /* Default hold duration for Gauge_decrease() */
    u8 damageBlinkFrames;         /* Default blink duration for Gauge_decrease() */
    u8 damageAnimShift;           /* Damage-trail animation speed divisor */
    u8 damageBlinkShift;          /* Damage blink cadence divisor */
    GaugeGainMode gainMode;       /* Visual behavior used for gain state */
    u8 gainHoldFrames;            /* Default hold duration for Gauge_increase() */
    u8 gainBlinkFrames;           /* Default blink duration for Gauge_increase() */
    u8 gainAnimShift;             /* Gain animation speed divisor */
    u8 gainBlinkShift;            /* Gain blink cadence divisor */
} GaugeBehavior;

/*
 * Full declarative build description.
 *
 * Typical flow:
 * 1. author segment skins
 * 2. describe lanes and their segments
 * 3. call Gauge_init() to allocate and build a gauge
 * 4. drive the runtime with Gauge_update()/Gauge_setValue()/Gauge_decrease()/Gauge_increase()
 */
typedef struct
{
    GaugeMode mode;                        /* FILL or PIP */
    VDPPlane plane;                        /* Target tilemap plane (BG_A / BG_B / WINDOW) */
    GaugeOrientation orientation;          /* Horizontal or vertical gauge */
    GaugeFillDirection fillDirection;      /* Forward or reverse fill order */
    u16 originX;                           /* Tilemap origin X */
    u16 originY;                           /* Tilemap origin Y (bottom tile for vertical) */
    u16 maxValue;                          /* 0 = auto-derive from built base lane */
    u8 fixedStartCap;                      /* 1 = force a start cap on the first fill cell */
    u8 fixedEndCap;                        /* 1 = force an end cap on the last fill cell */
    u8 palette;                            /* Default palette for every lane */
    u8 priority;                           /* Tile priority bit */
    u8 verticalFlip;                       /* Vertical flip for all lanes */
    u8 horizontalFlip;                     /* Horizontal flip for all lanes */
    GaugeLane lanes[GAUGE_MAX_LANES];
    GaugeBehavior behavior;                /* Shared runtime behavior for the gauge */
} GaugeDefinition;

/* -----------------------------------------------------------------------------
   GH-40 Public Runtime API
   ----------------------------------------------------------------------------- */
/**
 * Allocate, validate, and build one runtime gauge.
 *
 * @param definition Public authored build description.
 * @param vramBase   First VRAM tile reserved for this gauge.
 * @return Heap-allocated opaque Gauge handle, or NULL on validation/allocation failure.
 *         When NULL is returned, call Gauge_getLastBuildError() or
 *         Gauge_getLastBuildErrorText() to inspect the failure reason.
 */
Gauge *Gauge_init(const GaugeDefinition *definition,
                  u16 vramBase);

/**
 * Read the result code of the most recent Gauge_init() attempt.
 *
 * @return Last build result recorded by the module.
 */
GaugeBuildError Gauge_getLastBuildError(void);

/**
 * Read a short human-readable description of the most recent build result.
 *
 * @return Stable text for the last Gauge_init() result. When a build stage
 *         identified a precise source, the text may include a lane/segment
 *         suffix such as "lane 2, segment 1".
 */
const char *Gauge_getLastBuildErrorText(void);

/**
 * Tick the gauge logic and render every lane once.
 * Call this once per frame for every live gauge.
 *
 * @param gauge Gauge to update. NULL is ignored.
 */
void Gauge_update(Gauge *gauge);

/**
 * Change the logical maximum while keeping the built topology intact.
 *
 * @param gauge       Gauge to update. NULL is ignored.
 * @param newMaxValue New logical maximum, clamped to the module LUT capacity.
 */
void Gauge_setMaxValue(Gauge *gauge, u16 newMaxValue);

/**
 * Set the current logical value immediately.
 * This bypasses damage/gain transitions and resets trail state.
 *
 * @param gauge    Gauge to update. NULL is ignored.
 * @param newValue New logical value, clamped to the current max value.
 */
void Gauge_setValue(Gauge *gauge, u16 newValue);

/**
 * Decrease the current value using the default damage timing from GaugeBehavior.
 *
 * @param gauge  Gauge to update. NULL is ignored.
 * @param amount Logical amount to subtract.
 */
void Gauge_decrease(Gauge *gauge, u16 amount);

/**
 * Decrease the current value with explicit one-shot damage timing.
 *
 * @param gauge       Gauge to update. NULL is ignored.
 * @param amount      Logical amount to subtract.
 * @param holdFrames  Number of frames the damage trail should hold before blinking.
 * @param blinkFrames Number of blink-gated frames before the trail shrinks.
 */
void Gauge_decreaseEx(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames);

/**
 * Increase the current value using the default gain timing from GaugeBehavior.
 *
 * @param gauge  Gauge to update. NULL is ignored.
 * @param amount Logical amount to add.
 */
void Gauge_increase(Gauge *gauge, u16 amount);

/**
 * Increase the current value with explicit one-shot gain timing.
 *
 * @param gauge       Gauge to update. NULL is ignored.
 * @param amount      Logical amount to add.
 * @param holdFrames  Number of frames the gain trail should hold before blinking.
 * @param blinkFrames Number of blink-gated frames before the value catches up.
 */
void Gauge_increaseEx(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames);

/**
 * Enable or disable the module debug visualization for one gauge.
 * This is mainly useful for the local demo and debugging tools.
 *
 * @param gauge   Gauge to update. NULL is ignored.
 * @param enabled Non-zero enables debug visualization, zero disables it.
 */
void Gauge_setDebugMode(Gauge *gauge, u8 enabled);

/**
 * Query whether debug visualization is enabled for one gauge.
 *
 * @param gauge Gauge to inspect.
 * @return 1 when debug visualization is enabled, else 0.
 */
u8 Gauge_getDebugMode(const Gauge *gauge);

/**
 * Read the next free VRAM tile index after the current gauge allocation.
 *
 * @param gauge Gauge to inspect.
 * @return First VRAM tile index not consumed by this gauge, or 0 if unavailable.
 */
u16 Gauge_getNextVramIndex(const Gauge *gauge);

/**
 * Destroy a gauge and free all runtime allocations it owns.
 *
 * @param gauge Gauge to destroy. NULL is accepted.
 */
void Gauge_release(Gauge *gauge);

/**
 * Read the current logical value.
 *
 * @param gauge Gauge to inspect.
 * @return Current logical value, or 0 if gauge is NULL.
 */
u16 Gauge_getValue(const Gauge *gauge);

#endif /* GAUGE_H */
