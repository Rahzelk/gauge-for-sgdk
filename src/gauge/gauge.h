#ifndef GAUGE_H
#define GAUGE_H

#include <genesis.h>

/* =============================================================================
   Public Concepts
   =============================================================================

   A Gauge is one runtime HUD meter built from a declarative GaugeDefinition.

   Public hierarchy:
   - Gauge   : runtime object updated once per frame
   - Lane    : one visual row/column of that gauge
   - Segment : contiguous stretch of a lane sharing one skin
   - Cell    : one 8x8 tile inside a segment

   Fixed-only note:
   - Older revisions also had a dynamic VRAM path built around a smaller shared
     tile pool and more frequent tilemap updates.
   - In practice its tradeoff was too dependent on the exact lane/segment
     layout to justify keeping two rendering pipelines.
   - The figures below are static estimates taken from the representative demo
     gauges in this repository. They are useful for comparison, but they are
     not emulator-instrumented timings.
   - Representative comparisons:
       Screen 1 / simple fill   : FIXED 12 tiles, ~700-1000 cycles/frame
                                  DYNAMIC 5 tiles, ~850-1250 cycles/frame
       Screen 2 / bridges       : FIXED 12 tiles, ~1000-1400 cycles/frame
                                  DYNAMIC 15 tiles, ~1300-1900 cycles/frame
       Screen 3 / vertical gain : FIXED 12 tiles, ~750-1050 cycles/frame
                                  DYNAMIC 5 tiles, ~950-1350 cycles/frame
       Screen 4 / PIP basic     : FIXED 12 tiles, ~700-1000 cycles/frame
                                  DYNAMIC 10 tiles, ~750-1050 cycles/frame
   - Representative tilemap pressure:
       Screen 1 / Blink off     : FIXED runtime tilemap writes = 0
                                  DYNAMIC normal frame         = 0-2 cells
                                  DYNAMIC blink switch         = 0
                                  DYNAMIC trail switch         = 1-12 cells
   - In short, dynamic could reduce VRAM on some simple gauges, but it was not
     a general CPU win, it could lose VRAM on bridge-heavy layouts, and it
     added more VDP/tilemap complexity and more opportunities for glitches.
   - The module therefore keeps a single implicit VRAM strategy matching the
     old fixed tile-per-cell behavior.

   Fill authoring contract:
   - BODY, END and BRIDGE strips use 45 tiles each.
   - TRAIL strips use 64 tiles.
   - GaugeFillSkin exposes three public visual families:
     normal   : default rendering
     gain     : used while the gain trail is active
     blinkOff : alternate graphics used on blink-off frames

   PIP authoring contract:
   - GaugePipSkin::tileset stores the compact states for one segment style.
   - Minimum state order is: EMPTY, VALUE.
   - Optional extra states extend that order with: LOSS, GAIN, BLINK_OFF.
   - coverage describes whether the authored source art covers a full, half, or
     quarter pip surface before runtime expansion.

   Runtime visual states:
   - normal    : base rendering family
   - gain      : visuals used while the value is increasing
   - blink-off : alternate visuals used during OFF blink frames

   The public section below is the complete preferred integration surface.
   ============================================================================= */

/* -----------------------------------------------------------------------------
   Advanced Integration Configuration
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
   Public Limits And Authoring Constants
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
   Public Enumerations
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

/* -----------------------------------------------------------------------------
   Public Types
   ----------------------------------------------------------------------------- */
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

typedef struct
{
    GaugeFillSkin fill;  /* Continuous bar rendering assets */
    GaugePipSkin pip;    /* Discrete pip rendering assets */
} GaugeSkin;

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
   Public Functions
   ----------------------------------------------------------------------------- */
/* Allocate and build a gauge from a declarative definition and a base VRAM tile index.
 * Returns NULL on allocation or build failure. */
Gauge *Gauge_init(const GaugeDefinition *definition,
                  u16 vramBase);

/* Tick animations/state and render the gauge. Call once per frame. */
void Gauge_update(Gauge *gauge);
/* Change the logical maximum while keeping the built topology intact. */
void Gauge_setMaxValue(Gauge *gauge, u16 newMaxValue);
/* Set the logical value immediately. */
void Gauge_setValue(Gauge *gauge, u16 newValue);
/* Decrease the value using the default hold/blink timing from GaugeBehavior. */
void Gauge_decrease(Gauge *gauge, u16 amount);
/* Decrease the value with explicit one-shot hold/blink timing. */
void Gauge_decreaseEx(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames);
/* Increase the value using the default hold/blink timing from GaugeBehavior. */
void Gauge_increase(Gauge *gauge, u16 amount);
/* Increase the value with explicit one-shot hold/blink timing. */
void Gauge_increaseEx(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames);
/* Enable or disable module debug visualization for this gauge. */
void Gauge_setDebugMode(Gauge *gauge, u8 enabled);
/* Read whether module debug visualization is enabled. */
u8 Gauge_getDebugMode(const Gauge *gauge);
/* Read the next free VRAM tile index after this gauge allocation. */
u16 Gauge_getNextVramIndex(const Gauge *gauge);
/* Destroy a gauge and free all runtime allocations. Accepts NULL. */
void Gauge_release(Gauge *gauge);
/* Read the current logical value. */
u16 Gauge_getValue(const Gauge *gauge);

#endif /* GAUGE_H */
