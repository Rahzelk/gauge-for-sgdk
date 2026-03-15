#include "gauge.h"

/* =============================================================================
   gauge.c - HUD gauge implementation for SGDK (fill + pip, fixed-only)
   =============================================================================
 
   TILE STRIP SYSTEM:
   ------------------
   Each segment has pre-rendered "strips" of tiles stored in ROM.
   A 45-tile strip covers all fill combinations for one tile:
     - 9 levels of value (0..8 px) x 9 levels of trail (0..8 px)
     - Only valid combos where trail >= value (triangular matrix = 45 tiles)
   A 64-tile strip is used for trail-specific edge rendering.

   The LUT s_tileIndexByValueTrail[valuePx][trailPx] maps a (value, trail)
   pixel pair to the correct tile index within a strip (O(1) lookup).

   BREAK ZONE:
   -----------
   Most cells are trivially FULL (value=8, trail=8) or EMPTY (0,0).
   The "break zone" is the small region where value/trail partially fill a tile.
   Only break zone cells need per-pixel tile computation.

   Cell types in the break zone (fill direction: left to right):

     |FULL|FULL|VALUE_BREAK|TRAIL_FULL|TRAIL_BREAK|END|EMPTY|EMPTY|
      <--- value --->|<--------- trail -------->|
                     ^                          ^
                value edge                 trail edge

   VALUE_BREAK  : Cell where the value edge falls (0 < valuePx < 8)
   TRAIL_FULL   : Cells fully covered by trail but no value (valuePx=0, trailPx=8)
   TRAIL_BREAK  : Cell where the trail edge falls (0 < trailPx < 8)
   TRAIL_BREAK2 : Second trail break (before END, when bridge forces split)
   END          : Cell at the value/trail frontier (uses end tileset)

   BLINK-OFF:
   ----------
   During trail blink OFF frames, the break zone is recomputed using the
   "actual" trail (breakInfoActual) instead of the rendered trail (breakInfo).
   Trail-zone tiles get replaced with blink-off tilesets for the visual effect.

   PERFORMANCE:
   ------------
   - logicTickHandler: ~50-100 cycles (mostly conditionals)
   - process_fill_mode: ~200 cycles/cell (DMA dominant), lazy strip eval
   - Change detection avoids redundant DMA writes

   ============================================================================= */

/* -----------------------------------------------------------------------------
   Constants
   ----------------------------------------------------------------------------- */
#define TILE_TO_PIXEL_SHIFT  3

/* Invalid/uninitialized marker for cache values */
#define CACHE_INVALID_U16    0xFFFF
#define CACHE_INVALID_U8     0xFF

/* Default animation speeds (bit shift dividers: higher = slower)
 * Animation step formula: step = (distance >> shift) + 1
 * Blink full-cycle frequency: 60fps / (2 * (1 << shift)) Hz
 */
#define GAUGE_DEFAULT_VALUE_ANIM_SHIFT  4  /* Value moves at distance/16 + 1 px per frame */
#define GAUGE_DEFAULT_TRAIL_ANIM_SHIFT  4  /* Trail shrinks at distance/16 + 1 px per frame */
#define GAUGE_DEFAULT_BLINK_SHIFT       3  /* Blink toggles every 8 frames (7.5 toggles/sec @ 60fps) */

/* Helper macros */
#define GAUGE_ARRAY_LEN(array)      ((u16)(sizeof(array) / sizeof((array)[0])))
#define CALC_ANIM_STEP(diff, shift)   ((u16)(((diff) >> (shift)) + 1))  /* Animation step: distance/2^shift + 1 (always >= 1) */
#define FILL_IDX_TO_OFFSET(idx)       (((u16)(idx)) << 3)              /* fillIndex * 8 = pixel offset (each cell = 8 px) */

/* Simple zero-init allocator wrappers (SGDK MEM_alloc / MEM_free). */
static void *gauge_alloc_bytes(u16 bytes)
{
    if (bytes == 0)
        return NULL;
    void *ptr = MEM_alloc(bytes);
    if (ptr)
        memset(ptr, 0, bytes);
    return ptr;
}

static void gauge_free_ptr(void **ptr)
{
    if (ptr && *ptr)
    {
        MEM_free(*ptr);
        *ptr = NULL;
    }
}

typedef struct
{
    u8 *base;
    u16 size;
    u16 offset;
} GaugeRuntimeArena;

static u16 gauge_runtime_arena_align(u16 offset, u16 alignment)
{
    if (alignment <= 1)
        return offset;

    return (u16)((offset + alignment - 1) & (u16)~(alignment - 1));
}

static void gauge_runtime_arena_init(GaugeRuntimeArena *arena, void *memory, u16 size)
{
    if (!arena)
        return;

    arena->base = (u8 *)memory;
    arena->size = size;
    arena->offset = 0;
}

static void *gauge_runtime_arena_alloc(GaugeRuntimeArena *arena, u16 bytes, u16 alignment)
{
    if (!arena || !arena->base || bytes == 0)
        return NULL;

    const u16 alignedOffset = gauge_runtime_arena_align(arena->offset, alignment);
    if ((u32)alignedOffset + bytes > arena->size)
        return NULL;

    void *ptr = arena->base + alignedOffset;
    arena->offset = (u16)(alignedOffset + bytes);
    return ptr;
}

/* PIP compact strip state ordering */
#define PIP_STATE_EMPTY      0
#define PIP_STATE_VALUE      1
#define PIP_STATE_LOSS       2
#define PIP_STATE_GAIN       3
#define PIP_STATE_BLINK_OFF  4
#define PIP_HALF_AXIS_HORIZONTAL 0
#define PIP_HALF_AXIS_VERTICAL   1
#define GAUGE_PIP_RUNTIME_STATE_COUNT (PIP_STATE_BLINK_OFF + 1)
#define GAUGE_PIP_MAX_RENDER_TILES (GAUGE_MAX_LENGTH * 4)
#define GAUGE_PIP_RENDER_STATE_LUT_SIZE (GAUGE_PIP_RUNTIME_STATE_COUNT * GAUGE_PIP_MAX_RENDER_TILES)

/* Pre-computed strip indices for trivial cell states (avoid LUT indexing in hot path) */
#define STRIP_INDEX_EMPTY       0   /* s_tileIndexByValueTrail[0][0] : value=0, trail=0 */
#define STRIP_INDEX_FULL_TRAIL  8   /* s_tileIndexByValueTrail[0][8] : value=0, trail=8 */
#define STRIP_INDEX_FULL       44   /* s_tileIndexByValueTrail[8][8] : value=8, trail=8 */

/* Shared sentinel arrays for optional per-segment fields (no per-layout heap cost). */
static const u32 *s_nullSegmentTilesets[GAUGE_MAX_SEGMENTS] = {0};
static const u8 s_zeroSegmentFlags[GAUGE_MAX_SEGMENTS] = {0};
static const u8 s_oneSegmentFlags[GAUGE_MAX_SEGMENTS] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
};
static const u8 s_zeroCellFlags[GAUGE_MAX_LENGTH] = {0};
static const u8 s_oneCellFlags[GAUGE_MAX_LENGTH] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
};
static const u8 s_invalidCellIndexes[GAUGE_MAX_LENGTH] = {
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8,
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8,
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8,
    CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8, CACHE_INVALID_U8
};
static const u8 s_zeroPipRenderStateLut[GAUGE_PIP_RENDER_STATE_LUT_SIZE] = {0};

/**
 * Trail mode (internal state machine state).
 *
 * NONE:   No trail effect active. Trail equals value.
 *         This is the idle state (no damage/heal pending).
 *
 * DAMAGE: Classic damage trail. Value has decreased; the trail stays behind
 *         at the old position, holds, blinks, then shrinks toward the value.
 *         [####====........]   # = value, = = damage trail
 *
 * GAIN:   Gain trail. Value is increasing; the trail leads ahead of the
 *         current value, holds, blinks, then value catches up.
 *         [####====########]   = = gain trail (ahead of value)
 */
typedef enum
{
    GAUGE_TRAIL_STATE_NONE   = 0,
    GAUGE_TRAIL_STATE_DAMAGE = 1,
    GAUGE_TRAIL_STATE_GAIN   = 2
} GaugeActiveTrailState;

typedef void GaugeTickAndRenderHandler(Gauge *gauge);
typedef void GaugeLogicTickHandler(GaugeLogic *logic);
typedef void GaugeLaneRenderHandler(GaugeLaneInstance *laneInstance,
                                    u16 valuePixels,
                                    u16 trailPixelsRendered,
                                    u16 trailPixelsActual,
                                    u8 blinkOffActive,
                                    u8 blinkOnChanged,
                                    u8 trailMode,
                                    u8 trailModeChanged);

/* =============================================================================
   Internal Runtime Types
   =============================================================================

   These structures are intentionally private to gauge.c.
   gauge.h only keeps the public build contract plus the minimum runtime layout
   required for source compatibility (`Gauge`, `GaugeLogic`, a few direct fields).
   ============================================================================= */

struct GaugeLaneLayout
{
    u16 fillOffset;
    u8 length;
    u8 segmentCount;

    u8 segmentIdByCellStorage[GAUGE_MAX_LENGTH];
    u8 fillIndexByCellStorage[GAUGE_MAX_LENGTH];
    u8 cellIndexByFillIndexStorage[GAUGE_MAX_LENGTH];
    u8 *segmentIdByCell;
    u8 *fillIndexByCell;
    u8 *cellIndexByFillIndex;

    Vect2D_u16 tilemapPosByCellStorage[GAUGE_MAX_LENGTH];
    Vect2D_u16 *tilemapPosByCell;

    const u32 *tilesetBySegmentStorage[GAUGE_MAX_SEGMENTS];
    const u32 **tilesetBySegment;
    const u32 **tilesetEndBySegment;
    const u32 **tilesetTrailBySegment;
    const u32 **tilesetBridgeBySegment;
    const u32 **tilesetCapStartBySegment;
    const u32 **tilesetCapEndBySegment;
    const u32 **tilesetCapStartBreakBySegment;
    const u32 **tilesetCapStartTrailBySegment;
    u8 *capEndBySegment;

    const u32 **gainTilesetBySegment;
    const u32 **gainTilesetEndBySegment;
    const u32 **gainTilesetTrailBySegment;
    const u32 **gainTilesetBridgeBySegment;
    const u32 **gainTilesetCapStartBySegment;
    const u32 **gainTilesetCapEndBySegment;
    const u32 **gainTilesetCapStartBreakBySegment;
    const u32 **gainTilesetCapStartTrailBySegment;

    const u32 **blinkOffTilesetBySegment;
    const u32 **blinkOffTilesetEndBySegment;
    const u32 **blinkOffTilesetTrailBySegment;
    const u32 **blinkOffTilesetBridgeBySegment;
    const u32 **blinkOffTilesetCapStartBySegment;
    const u32 **blinkOffTilesetCapEndBySegment;
    const u32 **blinkOffTilesetCapStartBreakBySegment;
    const u32 **blinkOffTilesetCapStartTrailBySegment;

    const u32 **gainBlinkOffTilesetBySegment;
    const u32 **gainBlinkOffTilesetEndBySegment;
    const u32 **gainBlinkOffTilesetTrailBySegment;
    const u32 **gainBlinkOffTilesetBridgeBySegment;
    const u32 **gainBlinkOffTilesetCapStartBySegment;
    const u32 **gainBlinkOffTilesetCapEndBySegment;
    const u32 **gainBlinkOffTilesetCapStartBreakBySegment;
    const u32 **gainBlinkOffTilesetCapStartTrailBySegment;

    const u32 **pipTilesetBySegment;
    u8 *pipWidthBySegment;
    u8 *pipHeightBySegment;
    u8 *pipOffsetBySegment;
    u8 *pipStateCountBySegment;
    u8 *pipStripCoverageBySegment;
    u8 *pipHalfAxisBySegment;
    u8 *pipSourceWidthBySegment;
    u8 *pipSourceHeightBySegment;

    u8 pipCount;
    u8 *pipIndexByFillIndex;
    u8 *pipLocalTileByFillIndex;
    u8 *pipWidthByPipIndex;
    u8 pipRenderCount;
    u8 *pipRenderFillIndexByRenderIndex;
    u8 *pipRenderRowByRenderIndex;
    u8 *pipRenderExtraHFlipByRenderIndex;
    u8 *pipRenderExtraVFlipByRenderIndex;
    u8 *pipRenderStripIndexByState;
    u8 *bridgeEndByFillIndex;
    u8 *bridgeBreakByFillIndex;
    u8 *bridgeBreakBoundaryByFillIndex;

    u8 orientation;
    u8 palette;
    u8 priority;
    u8 verticalFlip;
    u8 horizontalFlip;
    u8 endOverrideEnabled;

    u8 hasBlinkOff;
    u8 hasGainBlinkOff;
    u8 capStartEnabled;
    u8 capEndEnabled;
    u16 refCount;
};

typedef struct
{
    const u32 *cachedStrip;
    u16 vramTileIndex;
    u8 cachedFillIndex;
    u8 cellIndex;
    u8 pipFillIndex;
    u8 pipRow;
    u8 pipRenderIndex;
} GaugeStreamCell;

struct GaugeLaneInstance
{
    u16 originX;
    u16 originY;

    u16 vramBase;
    GaugeLaneRenderHandler *renderHandler;
    const Gauge *gauge;

    const GaugeLaneLayout *layout;
    u8 baseLaneFillIndexByCell[GAUGE_MAX_LENGTH];

    GaugeStreamCell *cells;
    u8 cellCount;
};

typedef struct
{
    const GaugeLane *lane;
    u8 laneIndex;
    u16 originX;
    u16 originY;
    u16 fillOffset;
    u8 palette;
    u8 length;
    u8 segmentCount;
    u8 segmentIdByCell[GAUGE_MAX_LENGTH];
} GaugeBuildLanePlan;

typedef struct
{
    GaugeDefinition sanitizedDefinition;
    GaugeBuildLanePlan lanePlans[GAUGE_MAX_LANES];
    GaugeLaneLayout *builtLayouts[GAUGE_MAX_LANES];
} GaugeBuildScratch;

static GaugeBuildScratch s_buildScratch;

/* Public API uses internal helpers implemented later in the file. */
static u8 gauge_is_zeroed(const Gauge *gauge);
static inline u8 sanitize_value_mode(u8 mode);
static inline u8 sanitize_orientation(u8 orientation);
static inline u8 sanitize_fill_direction(u8 fillDirection);
static inline u8 sanitize_palette_index(u8 palette);
static u8 definition_lane_is_empty(const GaugeLane *lane);
static u8 find_baseLane_index(const GaugeDefinition *definition,
                              u8 *outLaneIndex);
static u8 build_lane_plan(const GaugeDefinition *definition,
                          u8 laneIndex,
                          const GaugeLane *lane,
                          const GaugeLane *baseLane,
                          GaugeBuildLanePlan *outPlan);
static u8 build_layout_from_lane_plan(const GaugeDefinition *definition,
                                      const GaugeBuildLanePlan *lanePlan,
                                      GaugeLaneLayout **outLayout);
static u8 init_layout_from_lane_plan(const GaugeDefinition *definition,
                                     const GaugeBuildLanePlan *lanePlan,
                                     GaugeLaneLayout *layout,
                                     const u32 * const *bodyTilesets);
static void release_built_layouts(GaugeLaneLayout **builtLayouts);
static inline u8 gauge_layout_has_bridge(const GaugeLaneLayout *layout);
static u16 compute_pip_total_pixels(const GaugeLaneLayout *layout);
static u16 clamp_max_value_to_capacity(u16 maxValue);
static u16 compute_vram_size_for_layout(const GaugeLaneLayout *layout,
                                        u8 trailEnabled,
                                        GaugeValueMode valueMode);
static GaugeTickAndRenderHandler *resolve_tick_and_render_handler(GaugeValueMode valueMode);
static u8 gauge_compute_stream_cell_capacity(GaugeValueMode valueMode,
                                             const GaugeLaneLayout *layout);
static u16 gauge_compute_runtime_arena_size(GaugeLaneLayout * const *builtLayouts,
                                            u8 laneCount,
                                            GaugeValueMode valueMode,
                                            u16 maxFillPixels);
static void GaugeLaneLayout_applyFillDirection(GaugeLaneLayout *layout,
                                               GaugeFillDirection fillDirection);
static void GaugeLaneLayout_setFillOffset(GaugeLaneLayout *layout, u16 fillOffsetPixels);
static void GaugeLaneLayout_retain(GaugeLaneLayout *layout);
static void GaugeLaneLayout_release(GaugeLaneLayout *layout);
static u8 GaugeLaneInstance_initInternal(GaugeLaneInstance *lane,
                                         const Gauge *gauge,
                                         GaugeLaneLayout *layout,
                                         u16 originX,
                                         u16 originY,
                                         u16 vramBase,
                                         GaugeRuntimeArena *runtimeArena);
static void GaugeLaneInstance_releaseInternal(GaugeLaneInstance *lane);
static void GaugeLogic_init(GaugeLogic *logic,
                            u16 maxValue,
                            u16 maxFillPixels,
                            const u16 *valueToPixelsLUT,
                            u16 initialValue);
static void GaugeLogic_tick_follow_mode(GaugeLogic *logic);
static inline void apply_non_follow_trail_mode_state(GaugeLogic *logic,
                                                     u8 advanceBlinkTimer);
static inline void apply_configured_trail_mode_state(GaugeLogic *logic,
                                                     u8 advanceBlinkTimer);
static inline void invalidate_render_cache(GaugeLogic *logic);
static u16 reproject_trail_pixels_by_ratio(u16 oldTrailPixels,
                                           u16 oldMaxFillPixels,
                                           u16 newMaxFillPixels);
static inline void gauge_set_current_value_and_target(GaugeLogic *logic,
                                                      u16 newValue,
                                                      u8 syncDisplayedValue);
static void build_base_lane_projection_maps(Gauge *gauge);
static void fill_value_to_pixels_lut(u16 *outValueToPixelsLUT,
                                     u16 maxValue,
                                     u16 maxFillPixels);
static void fill_pip_value_lut(u16 *outValueToPixelsLUT,
                               u16 maxValue,
                               const GaugeLaneLayout *layout);
static void gauge_rebuild_value_to_pixels_lut(Gauge *gauge);
static void gauge_rebuild_pip_quantize_lut(Gauge *gauge);
static void gauge_apply_behavior_from_definition(Gauge *gauge,
                                                 const GaugeBehavior *behavior);


/* =============================================================================
   Public API
   ============================================================================= */

u8 Gauge_build(Gauge *gauge,
               const GaugeDefinition *definition,
               u16 vramBase)
{
    if (!gauge || !definition)
        return 0;
    if (!gauge_is_zeroed(gauge))
        return 0;

    memset(&s_buildScratch, 0, sizeof(s_buildScratch));
    GaugeDefinition *sanitized = &s_buildScratch.sanitizedDefinition;
    *sanitized = *definition;
    sanitized->mode = (GaugeMode)sanitize_value_mode((u8)sanitized->mode);
    sanitized->orientation = (GaugeOrientation)sanitize_orientation((u8)sanitized->orientation);
    sanitized->fillDirection = (GaugeFillDirection)sanitize_fill_direction((u8)sanitized->fillDirection);
    sanitized->palette = sanitize_palette_index(sanitized->palette);
    sanitized->priority = sanitized->priority ? 1 : 0;
    sanitized->verticalFlip = sanitized->verticalFlip ? 1 : 0;
    sanitized->horizontalFlip = sanitized->horizontalFlip ? 1 : 0;
    sanitized->fixedStartCap = sanitized->fixedStartCap ? 1 : 0;
    sanitized->fixedEndCap = sanitized->fixedEndCap ? 1 : 0;
    if (sanitized->maxValue > GAUGE_LUT_CAPACITY)
        sanitized->maxValue = GAUGE_LUT_CAPACITY;

    u8 baseLaneIndex = 0;
    if (!find_baseLane_index(sanitized, &baseLaneIndex))
        return 0;

    const GaugeLane *baseLane = &sanitized->lanes[baseLaneIndex];
    GaugeBuildLanePlan *lanePlans = s_buildScratch.lanePlans;
    GaugeLaneLayout **builtLayouts = s_buildScratch.builtLayouts;
    u8 laneCount = 0;
    u8 longestLaneLength = 0;

    for (u8 laneIndex = 0; laneIndex < GAUGE_MAX_LANES; laneIndex++)
    {
        const GaugeLane *lane = &sanitized->lanes[laneIndex];
        if (definition_lane_is_empty(lane))
            continue;

        if (laneCount >= GAUGE_MAX_LANES)
        {
            release_built_layouts(builtLayouts);
            return 0;
        }

        if (!build_lane_plan(sanitized,
                             laneIndex,
                             lane,
                             baseLane,
                             &lanePlans[laneCount]))
        {
            release_built_layouts(builtLayouts);
            return 0;
        }

        if (lanePlans[laneCount].length > longestLaneLength)
            longestLaneLength = lanePlans[laneCount].length;

        laneCount++;
    }

    if (laneCount == 0)
        return 0;

    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
    {
        if (!build_layout_from_lane_plan(sanitized,
                                         &lanePlans[laneIndex],
                                         &builtLayouts[laneIndex]))
        {
            release_built_layouts(builtLayouts);
            return 0;
        }
    }

    u8 baseLaneRuntimeIndex = CACHE_INVALID_U8;
    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
    {
        if (lanePlans[laneIndex].laneIndex == baseLaneIndex)
        {
            baseLaneRuntimeIndex = laneIndex;
            break;
        }
    }

    if (baseLaneRuntimeIndex == CACHE_INVALID_U8)
    {
        release_built_layouts(builtLayouts);
        return 0;
    }

    GaugeLaneLayout *baseLayout = builtLayouts[baseLaneRuntimeIndex];
    const u16 maxFillPixels = (sanitized->mode == GAUGE_MODE_PIP)
        ? compute_pip_total_pixels(baseLayout)
        : (u16)(baseLayout->length << TILE_TO_PIXEL_SHIFT);

    u16 resolvedMaxValue = sanitized->maxValue;
    if (resolvedMaxValue == 0)
    {
        resolvedMaxValue = (sanitized->mode == GAUGE_MODE_PIP)
            ? baseLayout->pipCount
            : (u16)(longestLaneLength << TILE_TO_PIXEL_SHIFT);
    }
    resolvedMaxValue = clamp_max_value_to_capacity(resolvedMaxValue);

    if (maxFillPixels == 0 || resolvedMaxValue == 0)
    {
        release_built_layouts(builtLayouts);
        return 0;
    }

    const GaugeValueMode valueMode = (GaugeValueMode)sanitized->mode;
    const u16 runtimeArenaBytes = gauge_compute_runtime_arena_size(builtLayouts,
                                                                   laneCount,
                                                                   valueMode,
                                                                   maxFillPixels);
    void *runtimeArenaMemory = gauge_alloc_bytes(runtimeArenaBytes);
    if (!runtimeArenaMemory)
    {
        release_built_layouts(builtLayouts);
        return 0;
    }

    GaugeRuntimeArena runtimeArena;
    gauge_runtime_arena_init(&runtimeArena, runtimeArenaMemory, runtimeArenaBytes);

    GaugeLaneInstance **lanes = (GaugeLaneInstance **)gauge_runtime_arena_alloc(
        &runtimeArena,
        (u16)(laneCount * (u8)sizeof(GaugeLaneInstance *)),
        4);
    GaugeLaneInstance *laneStorage = (GaugeLaneInstance *)gauge_runtime_arena_alloc(
        &runtimeArena,
        (u16)(laneCount * (u16)sizeof(GaugeLaneInstance)),
        4);
    u16 *valueToPixelsData = (u16 *)gauge_runtime_arena_alloc(
        &runtimeArena,
        (u16)((GAUGE_LUT_CAPACITY + 1) * (u8)sizeof(u16)),
        2);
    u16 *pixelsToQuantizedPixelsLUT = NULL;
    if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        /* PIP quantization LUT is preallocated once in the runtime arena.
         * Subsequent rebuilds must stay allocation-free. */
        pixelsToQuantizedPixelsLUT = (u16 *)gauge_runtime_arena_alloc(
            &runtimeArena,
            (u16)((maxFillPixels + 1) * (u8)sizeof(u16)),
            2);
    }

    if (!lanes || !laneStorage || !valueToPixelsData ||
        (valueMode == GAUGE_VALUE_MODE_PIP && !pixelsToQuantizedPixelsLUT))
    {
        gauge_free_ptr(&runtimeArenaMemory);
        release_built_layouts(builtLayouts);
        return 0;
    }

    const u16 *valueToPixelsLUT = NULL;
    if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        fill_pip_value_lut(valueToPixelsData, resolvedMaxValue, baseLayout);
        valueToPixelsLUT = valueToPixelsData;
    }
    else
    {
        fill_value_to_pixels_lut(valueToPixelsData, resolvedMaxValue, maxFillPixels);
        if (resolvedMaxValue != maxFillPixels)
            valueToPixelsLUT = valueToPixelsData;
    }

    gauge->lanes = lanes;
    gauge->laneCount = laneCount;
    gauge->baseLaneIndex = baseLaneRuntimeIndex;
    gauge->baseLaneHasBridge = gauge_layout_has_bridge(baseLayout);
    gauge->baseLaneSpanPixels = maxFillPixels;
    gauge->ownedLayoutCount = laneCount;
    gauge->tickAndRenderHandler = resolve_tick_and_render_handler(valueMode);
    gauge->valueMode = valueMode;
    gauge->vramNextOffset = 0;
    gauge->runtimeArena = runtimeArenaMemory;
    gauge->logic.valueToPixelsData = valueToPixelsData;
    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
        gauge->ownedLayouts[laneIndex] = builtLayouts[laneIndex];

    GaugeLogic_init(&gauge->logic,
                    resolvedMaxValue,
                    maxFillPixels,
                    valueToPixelsLUT,
                    resolvedMaxValue);
    gauge->logic.valueToPixelsData = valueToPixelsData;
    gauge->logic.pixelsToQuantizedPixelsLUT = pixelsToQuantizedPixelsLUT;
    gauge_rebuild_pip_quantize_lut(gauge);
    gauge_apply_behavior_from_definition(gauge, &sanitized->behavior);

    u16 nextVram = vramBase;
    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
    {
        const u16 vramSize = compute_vram_size_for_layout(builtLayouts[laneIndex],
                                                          gauge->logic.trailEnabled,
                                                          valueMode);
        GaugeLaneInstance *lane = &laneStorage[laneIndex];

        if (!GaugeLaneInstance_initInternal(lane,
                                            gauge,
                                            builtLayouts[laneIndex],
                                            lanePlans[laneIndex].originX,
                                            lanePlans[laneIndex].originY,
                                            nextVram,
                                            &runtimeArena))
        {
            Gauge_release(gauge);
            return 0;
        }

        gauge->lanes[laneIndex] = lane;
        gauge->vramNextOffset = (u16)(gauge->vramNextOffset + vramSize);
        nextVram = (u16)(nextVram + vramSize);
    }

    build_base_lane_projection_maps(gauge);

    DMA_flushQueue();

    return 1;
}

/** Update gauge: tick logic + render all lanes. Call once per frame. */
void Gauge_update(Gauge *gauge)
{
    if (!gauge || !gauge->tickAndRenderHandler)
        return;

    gauge->tickAndRenderHandler(gauge);
}

void Gauge_setMaxValue(Gauge *gauge, u16 newMaxValue)
{
    if (!gauge || !gauge->tickAndRenderHandler)
        return;

    newMaxValue = clamp_max_value_to_capacity(newMaxValue);
    if (newMaxValue == 0 || newMaxValue == gauge->logic.maxValue)
        return;

    GaugeLogic *logic = &gauge->logic;
    const u16 oldMaxFillPixels = logic->maxFillPixels;
    const u16 oldTrailPixels = logic->trailPixels;

    logic->maxValue = newMaxValue;
    if (logic->currentValue > newMaxValue)
        logic->currentValue = newMaxValue;
    if (logic->criticalValue > newMaxValue)
        logic->criticalValue = newMaxValue;

    gauge_rebuild_value_to_pixels_lut(gauge);
    gauge_rebuild_pip_quantize_lut(gauge);

    gauge_set_current_value_and_target(logic, logic->currentValue, 1);

    logic->trailPixels = reproject_trail_pixels_by_ratio(oldTrailPixels,
                                                         oldMaxFillPixels,
                                                         logic->maxFillPixels);
    if (logic->trailPixels < logic->valuePixels)
        logic->trailPixels = logic->valuePixels;
    if (logic->trailPixels > logic->maxFillPixels)
        logic->trailPixels = logic->maxFillPixels;

    invalidate_render_cache(logic);
}

/** Set gauge value instantly (no trail, no animation). Resets all trail state. */
void Gauge_setValue(Gauge *gauge, u16 newValue)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->needUpdate = 1;
    gauge_set_current_value_and_target(logic, newValue, 1);
    apply_configured_trail_mode_state(logic, 0);
}

void Gauge_decrease(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->needUpdate = 1;

    gauge_set_current_value_and_target(logic,
                                       (logic->currentValue > amount) ? (u16)(logic->currentValue - amount) : 0,
                                       !logic->valueAnimEnabled);

    if (gauge->logicTickHandler != GaugeLogic_tick_follow_mode)
    {
        apply_non_follow_trail_mode_state(logic, 0);
        return;
    }

    const u16 previousDisplayedValuePixels = logic->valuePixels;
    if (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN)
    {
        logic->trailPixels = previousDisplayedValuePixels;
    }
    else if (logic->trailPixels < previousDisplayedValuePixels)
    {
        logic->trailPixels = previousDisplayedValuePixels;
    }

    logic->holdFramesRemaining = holdFrames;
    logic->blinkFramesRemaining = blinkFrames;
    logic->blinkTimer = 0;
    logic->activeTrailState = GAUGE_TRAIL_STATE_DAMAGE;
}

void Gauge_increase(Gauge *gauge, u16 amount, u8 holdFrames, u8 blinkFrames)
{
    if (!gauge)
        return;

    GaugeLogic *logic = &gauge->logic;
    logic->needUpdate = 1;

    gauge_set_current_value_and_target(logic,
                                       (logic->currentValue + amount > logic->maxValue)
                                           ? logic->maxValue
                                           : (u16)(logic->currentValue + amount),
                                       !logic->valueAnimEnabled);

    if ((GaugeGainMode)logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW &&
        logic->valueAnimEnabled)
    {
        logic->trailPixels = logic->valueTargetPixels;
        logic->holdFramesRemaining = holdFrames;
        logic->blinkFramesRemaining = blinkFrames;
        logic->blinkTimer = 0;
        logic->activeTrailState = GAUGE_TRAIL_STATE_GAIN;
        return;
    }

    if ((GaugeTrailMode)logic->configuredTrailMode == GAUGE_TRAIL_MODE_FOLLOW)
    {
        logic->trailPixels = logic->valuePixels;
        logic->holdFramesRemaining = 0;
        logic->blinkFramesRemaining = 0;
        logic->blinkTimer = 0;
        logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    }
    else
    {
        apply_non_follow_trail_mode_state(logic, 0);
    }
}

void Gauge_release(Gauge *gauge)
{
    if (!gauge)
        return;

    for (u8 i = 0; i < gauge->laneCount; i++)
    {
        GaugeLaneInstance *lane = (gauge->lanes != NULL) ? gauge->lanes[i] : NULL;
        if (!lane)
            continue;

        GaugeLaneInstance_releaseInternal(lane);
        gauge->lanes[i] = NULL;
    }

    for (u8 layoutIndex = 0; layoutIndex < gauge->ownedLayoutCount; layoutIndex++)
    {
        GaugeLaneLayout *ownedLayout = gauge->ownedLayouts[layoutIndex];
        if (!ownedLayout)
            continue;

        if (ownedLayout->length != 0 || ownedLayout->refCount != 0)
            GaugeLaneLayout_release(ownedLayout);
        gauge_free_ptr((void **)&ownedLayout);
        gauge->ownedLayouts[layoutIndex] = NULL;
    }
    gauge->ownedLayoutCount = 0;

    gauge_free_ptr(&gauge->runtimeArena);

    memset(gauge, 0, sizeof(*gauge));
}

u16 Gauge_getValue(const Gauge *gauge)
{
    return gauge ? gauge->logic.currentValue : 0;
}


/* =============================================================================
   Tile index lookup table
   =============================================================================
   Maps (valuePxInTile, trailPxInTile) to tile index in 45-tile strip.

   Row = valuePxInTile (0..8)
   Column = trailPxInTile (0..8)

   Constraint: trail >= value (invalid combinations marked 0xFF = CACHE_INVALID_U8)

   The 45-tile strip is organized as triangular matrix (trail >= value):
   - Tiles 0-8:   value=0, trail=0..8  (9 tiles: empty to full trail)
   - Tiles 9-16:  value=1, trail=1..8  (8 tiles)
   - Tiles 17-23: value=2, trail=2..8  (7 tiles)
   - ...
   - Tile 44:     value=8, trail=8     (1 tile: fully filled)
   Total: 9+8+7+6+5+4+3+2+1 = 45 tiles

   Visual representation of a single tile's fill zones:
     +--------+
     |TTTTTTTT|  T = trail pixels (trailPx - valuePx)
     |VVVVTTTT|  V = value pixels
     |VVVV    |  (blank) = empty pixels
     +--------+

   ============================================================================= */
static const u8 s_tileIndexByValueTrail[9][9] =
{
    /*          trail=0  1    2    3    4    5    6    7    8  */
    /* val=0 */ {   0,   1,   2,   3,   4,   5,   6,   7,   8 },
    /* val=1 */ { 0xFF,  9,  10,  11,  12,  13,  14,  15,  16 },
    /* val=2 */ { 0xFF,0xFF, 17,  18,  19,  20,  21,  22,  23 },
    /* val=3 */ { 0xFF,0xFF,0xFF, 24,  25,  26,  27,  28,  29 },
    /* val=4 */ { 0xFF,0xFF,0xFF,0xFF, 30,  31,  32,  33,  34 },
    /* val=5 */ { 0xFF,0xFF,0xFF,0xFF,0xFF, 35,  36,  37,  38 },
    /* val=6 */ { 0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, 39,  40,  41 },
    /* val=7 */ { 0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, 42,  43 },
    /* val=8 */ { 0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF, 44 }
};

/* =============================================================================
   Trail tile index lookup table (64-tile strip)
   =============================================================================
   Maps (valuePxInTile, trailPxInTile) to tile index in 64-tile trail strip.
   Used for TRAIL BREAK cells (transition zone between value and trail edges).

   Row = valuePxInTile (0..8)
   Column = trailPxInTile (0..8)

   Unlike the 45-tile body strip, this strip uses a different tile layout
   optimized for trail-specific rendering (e.g., different edge shapes).
   Column 8 maps to the same tile as column 7 (trail=7 and trail=8 share
   the same visual when the cell is fully covered by trail).

   Note: 0xFF entries (trail < value) are invalid and never accessed.
   ============================================================================= */
static const u8 s_trailTileIndexByValueTrail[9][9] =
{
    /*          trail=0  1    2    3    4    5    6    7    8  */
    /* val=0 */ {   0,   1,   2,   3,   4,   5,   6,   7,   7 },
    /* val=1 */ { 0xFF,  8,   9,  10,  11,  12,  13,  14,  14 },
    /* val=2 */ { 0xFF,  15, 16,  17,  18,  19,  20,  21,  21 },
    /* val=3 */ { 0xFF,  22, 23,  24,  25,  26,  27,  28,  28 },
    /* val=4 */ { 0xFF,  29, 30,  31,  32,  33,  34,  35,  35 },
    /* val=5 */ { 0xFF,  36, 37,  38,  39,  40,  41,  42,  42 },
    /* val=6 */ { 0xFF,  43, 44,  45,  46,  47,  48,  49,  49 },
    /* val=7 */ { 0xFF,  50, 51,  52,  53,  54,  55,  56,  56 },
    /* val=8 */ { 0xFF,  57, 58,  59,  60,  61,  62,  63,  63 }
};


/* =============================================================================
   Helper functions (inlined for performance)
   ============================================================================= */

/**
 * Clamp value to tile pixel range [0, 8].
 *
 * Lower clamp is branchless via arithmetic masking:
 * - If v < 0, bit 15 is set, so ~(v >> 15) = 0x0000 which zeroes v.
 * - If v >= 0, ~(v >> 15) = 0xFFFF which preserves v.
 * Upper clamp uses a single branch (rare: only on break cells).
 *
 * Cost: ~8-12 cycles (asr + not + and + cmp + branch)
 * vs original: ~10-20 cycles (2 branches worst-case)
 */
static inline u8 clamp_to_tile_size(s16 v)
{
    /* Branchless lower clamp: zero if negative.
     * v >> 15 is SIGNED (arithmetic) shift: gives -1 (0xFFFF) if v < 0, 0 otherwise.
     * ~(-1) = 0 zeroes v; ~0 = 0xFFFF preserves v.
     * IMPORTANT: must NOT cast v to u16 before shift - that would make it unsigned
     * and give 0 or 1 instead of 0 or 0xFFFF, breaking the mask. */
    u16 clamped = (u16)(v & ~(v >> 15));
    /* Upper clamp: cap at 8 (rare case, only at break cells) */
    return (clamped > GAUGE_PIXELS_PER_TILE) ? GAUGE_PIXELS_PER_TILE : (u8)clamped;
}

/**
 * Convert gauge value to pixel position.
 * Uses LUT if available, otherwise returns value directly (1:1 mapping).
 *
 * The LUT is generated during Gauge_build() and rebuilt in place by
 * Gauge_setMaxValue() when the logical scale changes.
 */
static inline u16 value_to_pixels(const GaugeLogic *logic, u16 value)
{
    if (logic->valueToPixelsLUT)
        return logic->valueToPixelsLUT[value];
    return value;
}

/**
 * Compute fill amounts for a given fillIndex (0..length-1).
 *
 * Core algorithm for pixel-perfect gauge rendering.
 * Determines how many pixels of value and trail are visible in one cell.
 *
 * @param layout       Layout containing fillOffset
 * @param fillIndex    Logical fill position (0 = first filled cell)
 * @param valuePixels  Current value fill in pixels
 * @param trailPixels  Current trail fill in pixels
 * @param outValuePx   [out] Value fill in this cell (0-8)
 * @param outTrailPx   [out] Trail fill in this cell (0-8, always >= outValuePx)
 *
 * Invariant: outTrailPx >= outValuePx (trail can never be behind value).
 * Cost: ~20 cycles (2x clamp + 1 compare)
 */
static inline void compute_fill_for_fill_index(const GaugeLaneLayout *layout,
                                               u8 fillIndex,
                                               u16 valuePixels,
                                               u16 trailPixels,
                                               u8 *outValuePx,
                                               u8 *outTrailPx)
{
    const u16 cellStartPixel = layout->fillOffset + ((u16)fillIndex << TILE_TO_PIXEL_SHIFT);

    u8 valuePxInCell = clamp_to_tile_size((s16)valuePixels - (s16)cellStartPixel);
    u8 trailPxInCell = clamp_to_tile_size((s16)trailPixels - (s16)cellStartPixel);

    /* Enforce trail >= value invariant */
    if (trailPxInCell < valuePxInCell)
        trailPxInCell = valuePxInCell;

    *outValuePx = valuePxInCell;
    *outTrailPx = trailPxInCell;
}

/**
 * Compute fill amounts for a cell by its cellIndex.
 * Wrapper around compute_fill_for_fill_index using the fillIndexByCell lookup.
 *
 * @param layout       Layout configuration
 * @param cellIndex    Cell index in the layout (0..length-1)
 * @param valuePixels  Current value fill in pixels
 * @param trailPixels  Current trail fill in pixels
 * @param outValuePx   [out] Value fill in this cell (0-8)
 * @param outTrailPx   [out] Trail fill in this cell (0-8, always >= outValuePx)
 */
static inline void compute_fill_for_cell(const GaugeLaneLayout *layout,
                                         u8 cellIndex,
                                         u16 valuePixels,
                                         u16 trailPixels,
                                         u8 *outValuePx,
                                         u8 *outTrailPx)
{
    compute_fill_for_fill_index(layout, layout->fillIndexByCell[cellIndex],
                                valuePixels, trailPixels, outValuePx, outTrailPx);
}

/**
 * Compute the break fillIndex and its pixel offset within that cell.
 *
 * Given a pixel position, determines which cell (by fillIndex) contains that
 * pixel and how many pixels are filled within that cell.
 *
 * Special case: exact tile boundary (pixel % 8 == 0 && pixel > 0) maps to
 * the previous cell with 8px filled, not the next cell with 0px.
 *
 * @param layout      Layout containing fillOffset and length
 * @param pixels      Pixel position to locate
 * @param outPxInTile [out] Pixels filled in the located cell (0-8)
 * @return fillIndex of the cell containing this pixel position
 *
 * Cost: ~25 cycles (subtract + shift + mask test + clamp)
 */
static inline u8 compute_fill_index_and_px(const GaugeLaneLayout *layout,
                                           u16 pixels,
                                           u8 *outPxInTile)
{
    s16 rel = (s16)pixels - (s16)layout->fillOffset;
    if (rel < 0)
    {
        *outPxInTile = 0;
        return 0;
    }

    u16 fillIndex;
    u8 pxInTile;

    /* Exact tile boundary: use previous cell with 8px filled. */
    if ((rel > 0) && ((rel & (GAUGE_PIXELS_PER_TILE - 1)) == 0))
    {
        fillIndex = (u16)((rel >> TILE_TO_PIXEL_SHIFT) - 1);
        pxInTile = GAUGE_PIXELS_PER_TILE;
    }
    else
    {
        fillIndex = (u16)(rel >> TILE_TO_PIXEL_SHIFT);
        const u16 cellStartPixel = layout->fillOffset + ((u16)fillIndex << TILE_TO_PIXEL_SHIFT);
        pxInTile = clamp_to_tile_size((s16)pixels - (s16)cellStartPixel);
    }

    if (fillIndex >= layout->length)
    {
        fillIndex = (u16)(layout->length - 1);
        pxInTile = GAUGE_PIXELS_PER_TILE;
    }

    *outPxInTile = pxInTile;

    return (u8)fillIndex;
}

/**
 * Compute tile position on the WINDOW plane based on orientation.
 *
 * For horizontal gauges: X varies by cell, Y stays at origin.
 * For vertical gauges:   Y varies upward from the bottom origin, X stays at origin.
 *
 * @param orient  GAUGE_ORIENT_HORIZONTAL or GAUGE_ORIENT_VERTICAL
 * @param originX Tilemap X origin of the gauge
 * @param originY Tilemap Y origin of the gauge (bottom tile for vertical)
 * @param cell    Cell index (0..length-1)
 * @param outX    Output: tilemap X coordinate
 * @param outY    Output: tilemap Y coordinate
 */
static inline void compute_tile_xy(u8 orient,
                                   u16 originX, u16 originY,
                                   u8 cell,
                                   u16 *outX, u16 *outY)
{
    if (orient == GAUGE_ORIENT_HORIZONTAL)
    {
        *outX = (u16)(originX + cell);
        *outY = originY;
    }
    else
    {
        *outX = originX;
        *outY = (u16)(originY - cell);
    }
}

static inline void compute_pip_render_xy(const GaugeLaneLayout *layout,
                                         u16 originX,
                                         u16 originY,
                                         u8 fillIndex,
                                         u8 renderRow,
                                         u16 *outX,
                                         u16 *outY)
{
    if (!layout)
        return;

    if (layout->orientation == GAUGE_ORIENT_HORIZONTAL)
    {
        *outX = (u16)(originX + fillIndex);
        *outY = (u16)(originY + renderRow);
    }
    else
    {
        *outX = (u16)(originX + renderRow);
        *outY = (u16)(originY - fillIndex);
    }
}

static inline u16 compute_pip_render_state_lut_index(u8 pipState, u8 renderIndex)
{
    return (u16)((u16)pipState * (u16)GAUGE_PIP_MAX_RENDER_TILES + renderIndex);
}



/* =============================================================================
   GaugeBreakInfo - Pre-computed break/transition zone data

   Visual notation (single cell):
   F = BODY full   (value=8, trail=8)
   V = BODY empty  (value=0, trail=0)
   B = VALUE_BREAK (BODY tileset, transition before value edge)
   T = TRAIL full  (TRAIL tileset, value=0, trail=8)
   E = END         (END tileset, end of gauge)
   D = BRIDGE      (BRIDGE tileset, transition between segments)
   =============================================================================

   Shared by process_fill_mode() and the fill decision helpers.
   Contains all the information needed to classify cells in the break zone
   between the value edge and the trail edge.

   Gauge rendering visualized (horizontal, fill forward):

     Cell:     [0][1][2][3][4][5][6][7]
     Fill:     F  F  B  V  T  T  E  V
     Pixels:   F=8px, V=0px, B/T/E use partial LUTs

     F (FULL)     = value fills entire cell (8px)
     B (BREAK)    = transition cell before value edge (BODY tileset)
     VALUE CELL   = cell containing the value edge (partial value px)
     T (TRAIL)    = cells between value and trail edges (TRAIL tileset)
     E (END)      = cell containing the trail edge / cap (END tileset)
     V (EMPTY)    = no fill (0px)

   Multi-lane gauges: if value or trail edges fall outside this layout's
   pixel range [fillOffset .. fillOffset + length*8], the corresponding
   break features are disabled for this lane.
   ============================================================================= */

typedef struct
{
    /* Break cell fill indices (CACHE_INVALID_U8 if not applicable) */
    u8 valueFillIndex;          /* Fill index of the value break cell (B) */
    u8 trailFillIndex;          /* Fill index of the trail break cell (T) */
    u8 valuePxInBreakCell;      /* Pixels of value in the value break cell (B) */
    u8 trailPxInBreakCell;      /* Pixels of trail in the trail break cell (T) */

    /* Cell indices (via inverse LUT) */
    u8 valueCellIndex;          /* Layout cell index for value break (B) */
    u8 trailCellIndex;          /* Layout cell index for trail break (T) */

    /* END/BREAK zone indices */
    u8 endFillIndex;            /* Fill index for END cap tile (E) (CACHE_INVALID_U8 if none) */
    u8 valueBreakFillIndex;     /* Fill index for VALUE BREAK cell (B) (CACHE_INVALID_U8 if none) */
    u8 trailBreakFillIndex;     /* Fill index for TRAIL BREAK 1 (T) (CACHE_INVALID_U8 if none) */
    u8 trailBreakFillIndex2;    /* Fill index for TRAIL BREAK 2 (T) (CACHE_INVALID_U8 if none) */

    /* Pre-computed END cell fill values */
    u8 endValuePxInTile;        /* Value pixels in END cell (E) */
    u8 endTrailPxInTile;        /* Trail pixels in END cell (E) */

    /* Flags */
    u8 trailBreakActive;        /* Trail break zone is active */
    u8 trailBreakSecondActive;  /* Second trail break cell exists */
    u8 regionRenderActive;      /* Region-based rendering applies */

} GaugeBreakInfo;

/* Forward declarations for helpers used by compute_break_info */
static inline const u32 *select_base_strip(const u32 *normalStrip,
                                           const u32 *gainStrip,
                                           u8 trailMode);
static inline u8 select_pip_state_for_cell(const GaugeLaneLayout *layout,
                                           u8 cellIndex,
                                           u16 valuePixels,
                                           u16 trailPixelsRendered,
                                           u16 trailPixelsActual,
                                           u8 blinkOffActive,
                                           u8 trailMode);

/**
 * Compute all break/transition zone information for a gauge lane.
 *
 * This function determines where the value and trail edges fall within the
 * layout, and pre-computes all indices and flags needed for per-cell
 * classification in the render loop.
 *
 * @param layout              Layout configuration (with inverse LUT)
 * @param valuePixels         Current value fill in pixels
 * @param trailPixelsRendered Current trail fill in pixels (after blink)
 * @param out                 [out] Pre-computed break zone information
 *
 * Cost: ~80 cycles (2x compute_fill_index_and_px + LUT lookups + flags)
 */
static inline void compute_break_info(const GaugeLaneLayout *layout,
                                      u16 valuePixels,
                                      u16 trailPixelsRendered,
                                      u8 trailMode,
                                      GaugeBreakInfo *out)
{
    /* === Step 1: Compute value and trail fill positions ===
     * For each edge (value and trail), find which cell it falls in (fillIndex)
     * and how many pixels into that cell it extends (pxInBreakCell). */
    out->valueFillIndex = compute_fill_index_and_px(layout, valuePixels, &out->valuePxInBreakCell);
    out->trailFillIndex = compute_fill_index_and_px(layout, trailPixelsRendered, &out->trailPxInBreakCell);

    /* O(1) inverse LUT lookup instead of O(n) linear scan (~300 cycles saved) */
    out->valueCellIndex = layout->cellIndexByFillIndex[out->valueFillIndex];
    out->trailCellIndex = layout->cellIndexByFillIndex[out->trailFillIndex];

    const u8 valueSegId = layout->segmentIdByCell[out->valueCellIndex];
    const u8 trailSegId = layout->segmentIdByCell[out->trailCellIndex];
    const u8 valueSegmentHasEnd =
        (select_base_strip(layout->tilesetEndBySegment[valueSegId],
                           layout->gainTilesetEndBySegment[valueSegId],
                           trailMode) != NULL);
    const u8 trailSegmentHasEnd =
        (select_base_strip(layout->tilesetEndBySegment[trailSegId],
                           layout->gainTilesetEndBySegment[trailSegId],
                           trailMode) != NULL);
    const u8 valueSegmentHasTrail =
        (select_base_strip(layout->tilesetTrailBySegment[valueSegId],
                           layout->gainTilesetTrailBySegment[valueSegId],
                           trailMode) != NULL);

    /* Detect if break cells belong to this layout (multi-lane gauges) */
    const u16 layoutStart = layout->fillOffset;
    const u16 layoutEnd = (u16)(layoutStart + ((u16)layout->length << TILE_TO_PIXEL_SHIFT));
    const u8 valueInLayout = (valuePixels >= layoutStart && valuePixels <= layoutEnd);
    const u8 trailInLayout = (trailPixelsRendered >= layoutStart && trailPixelsRendered <= layoutEnd);

    /* === Step 2: Compute END cell position (transition tile) ===
     * END cap (E):
     * - Normal case: place END on trail edge when trail is visible.
     * - Offset/window fallback:
     *   keep one END inside the visible window even when trail edge is outside,
     *   so partial gauges with END/BREAK assets remain visually stable.
     *
     * Priority order is important:
     * 1) trail in layout      -> END on trail edge
     * 2) trail right of view  -> END on last visible cell
     * 3) trail left of view   -> END on first visible cell
     * 4) value in layout      -> END on value edge
     * 5) no END in this lane
     */
    if (trailInLayout && trailSegmentHasEnd)
    {
        out->endFillIndex = out->trailFillIndex;
    }
    else if (trailPixelsRendered > layoutEnd && trailSegmentHasEnd)
    {
        out->endFillIndex = (u8)(layout->length - 1);
    }
    else if (trailPixelsRendered < layoutStart && trailSegmentHasEnd)
    {
        out->endFillIndex = 0;
    }
    else if (valueInLayout && valueSegmentHasEnd)
    {
        out->endFillIndex = out->valueFillIndex;
    }
    else
    {
        out->endFillIndex = CACHE_INVALID_U8;
    }

    /* === Step 3: Compute VALUE_BREAK position ===
     * VALUE BREAK (B): one cell before value break */
    out->valueBreakFillIndex =
        (valueInLayout && valueSegmentHasEnd && out->valueFillIndex > 0)
        ? (u8)(out->valueFillIndex - 1) : CACHE_INVALID_U8;

    /* === Step 4: Compute TRAIL_BREAK positions (first and second) ===
     * TRAIL BREAK (T): active when the trail zone intersects this layout window
     * and value/trail edges are not in the same cell. */
    const u8 trailZoneIntersectsLayout =
        (trailPixelsRendered > layoutStart) && (valuePixels < layoutEnd);

    out->trailBreakActive =
        (trailZoneIntersectsLayout && valueSegmentHasEnd && valueSegmentHasTrail &&
         (out->valueCellIndex != out->trailCellIndex));
    out->trailBreakFillIndex = out->trailBreakActive ? out->valueFillIndex : CACHE_INVALID_U8;
    out->trailBreakSecondActive =
        (out->trailBreakActive && (out->trailFillIndex > (u8)(out->valueFillIndex + 1)));
    out->trailBreakFillIndex2 = out->trailBreakSecondActive
        ? (u8)(out->trailFillIndex - 1) : CACHE_INVALID_U8;

    /* === Step 5: Check if region-based rendering is needed ===
     * Region rendering: active when both edges are visible and have END tilesets */
    out->regionRenderActive = (valueInLayout && trailInLayout &&
                               valueSegmentHasEnd && trailSegmentHasEnd);

    /* Precompute END (E) cell fill values */
    out->endValuePxInTile = 0;
    out->endTrailPxInTile = 0;
    if (out->endFillIndex != CACHE_INVALID_U8)
    {
        compute_fill_for_fill_index(layout, out->endFillIndex,
                                    valuePixels, trailPixelsRendered,
                                    &out->endValuePxInTile, &out->endTrailPxInTile);
    }
}

/**
 * Check if a cell (by fill index) is in the break zone (END/TRAIL/VALUE_BREAK).
 *
 * Returns 1 if the cell is any of: END, VALUE_BREAK, TRAIL_BREAK, TRAIL_BREAK2,
 * or TRAIL_FULL. Used to decide whether blink-off tilesets should override.
 *
 * @param breakInfo     Pre-computed break zone info
 * @param cellFillIndex Fill index of the cell to check
 * @return 1 if cell is in break zone, 0 otherwise
 */
static inline u8 is_cell_in_trail_zone(const GaugeBreakInfo *breakInfo, u8 cellFillIndex)
{
    if (breakInfo->endFillIndex != CACHE_INVALID_U8 && cellFillIndex == breakInfo->endFillIndex)
        return 1;
    if (breakInfo->valueBreakFillIndex != CACHE_INVALID_U8 && cellFillIndex == breakInfo->valueBreakFillIndex)
        return 1;
    if (breakInfo->trailBreakActive)
    {
        if (cellFillIndex == breakInfo->trailBreakFillIndex)
            return 1;
        if (breakInfo->trailBreakSecondActive && cellFillIndex == breakInfo->trailBreakFillIndex2)
            return 1;
        if (cellFillIndex > breakInfo->valueFillIndex && cellFillIndex < breakInfo->trailFillIndex)
            return 1;
    }
    return 0;
}


/* =============================================================================
   Cap Start classification result
   =============================================================================

   Cap Start is a fixed border tile at the start of a gauge (cell 0 in fill order).
   It uses dedicated tilesets that vary depending on the gauge state at cell 0:

   Visual representation (horizontal, fill forward):
     Cell:    [CAP][1][2][3][4][5][6][7]
     State:    CS   F  F  B  V  T  T  E

   Cap start has 3 tileset variants:
   - capStartStrip:      default cap (used when cell is END or FULL)
   - capStartBreakStrip: cap while value hasn't reached cell 0 yet (empty/break state)
   - capStartTrailStrip: cap while trail zone is visible (trail break/full)

   Priority rules for tileset + fill index selection:
   1. Cell is END (E)             -> capStartStrip + endIndex
   2. Cell is TRAIL (break/full)  -> capStartTrailStrip + trail LUT
   3. Next cell is END (E)        -> capStartBreakStrip + endIndex (aligned to E)
   4. Else (cell is FULL)         -> capStartBreakStrip + index 44 (full)

   ============================================================================= */
typedef struct
{
    const u32 *strip;     /* Selected ROM strip for this frame */
    u8 fillStripIndex;    /* Index into the selected strip */
    u8 usesBreak;         /* 1 if using the break variant strip */
    u8 usesTrail;         /* 1 if using the trail variant strip */
} CapStartResult;

/**
 * Classify cap start cell and select the appropriate tileset strip + tile index.
 *
 * Applies the 4-priority rule (see CapStartResult block comment above):
 * 1. Cell is END          -> capStartStrip + endIndex
 * 2. Cell is TRAIL zone   -> capStartTrailStrip + trail LUT index
 * 3. Next cell is END     -> capStartBreakStrip + endIndex
 * 4. Default (FULL/EMPTY) -> capStartBreakStrip + STRIP_INDEX_FULL
 *
 * @param breakInfo          Pre-computed break zone info
 * @param cellFillIndex      Fill index of the cap start cell
 * @param capStartStrip      Default cap tileset (ROM strip)
 * @param capStartBreakStrip Cap tileset for break/full state (can be NULL)
 * @param capStartTrailStrip Cap tileset for trail zone (can be NULL)
 * @param out                Output: selected strip, tile index, and flags
 */
static inline void classify_cap_start_with_strips(const GaugeBreakInfo *breakInfo,
                                                  u8 cellFillIndex,
                                                  const u32 *capStartStrip,
                                                  const u32 *capStartBreakStrip,
                                                  const u32 *capStartTrailStrip,
                                                  CapStartResult *out)
{
    const u8 endValid = (breakInfo->endFillIndex != CACHE_INVALID_U8);
    const u8 isEndHere = (endValid && cellFillIndex == breakInfo->endFillIndex);
    const u8 isEndNext = (endValid && (u8)(cellFillIndex + 1) == breakInfo->endFillIndex);

    /* Pre-compute the END fill index (used in multiple branches) */
    const u8 endIndex = endValid
        ? s_tileIndexByValueTrail[breakInfo->endValuePxInTile][breakInfo->endTrailPxInTile]
        : STRIP_INDEX_FULL;

    /* Detect trail zone states */
    const u8 isTrailBreak = (breakInfo->trailBreakActive && capStartTrailStrip &&
                             cellFillIndex == breakInfo->trailBreakFillIndex);
    const u8 isTrailBreak2 = (breakInfo->trailBreakSecondActive && capStartTrailStrip &&
                              cellFillIndex == breakInfo->trailBreakFillIndex2);
    const u8 isTrailFull = (breakInfo->trailBreakActive && capStartTrailStrip &&
                            cellFillIndex > breakInfo->valueFillIndex &&
                            cellFillIndex < breakInfo->trailFillIndex);
    const u8 useTrail = (isTrailBreak || isTrailBreak2 || isTrailFull);

    /* --- Select strip --- */
    if (isEndHere)
    {
        /* Priority 1: cell is END -> use main cap start strip */
        out->strip = capStartStrip;
        out->usesBreak = 0;
        out->usesTrail = 0;
    }
    else if (useTrail)
    {
        /* Priority 2: cell is in trail zone -> use trail variant */
        out->strip = capStartTrailStrip;
        out->usesBreak = 0;
        out->usesTrail = 1;
    }
    else
    {
        /* Priority 3-4: break/full -> use break variant (fallback to main cap) */
        out->strip = capStartBreakStrip ? capStartBreakStrip : capStartStrip;
        out->usesBreak = 1;
        out->usesTrail = 0;
    }

    /* --- Select fill index --- */
    if (isEndHere)
    {
        /* END cell: use the end fill index directly */
        out->fillStripIndex = endIndex;
    }
    else if (isTrailBreak)
    {
        /* Trail break #1: transition cell right after value edge.
         * If a second trail break exists, this cell is fully covered by trail (trailPx=8).
         * Otherwise, use the actual trail px in the break cell. */
        if (breakInfo->trailBreakSecondActive)
            out->fillStripIndex = s_trailTileIndexByValueTrail[breakInfo->valuePxInBreakCell][8];
        else
            out->fillStripIndex = s_trailTileIndexByValueTrail[breakInfo->valuePxInBreakCell][breakInfo->trailPxInBreakCell];
    }
    else if (isTrailBreak2)
    {
        /* Trail break #2: second transition cell (before END).
         * If END index is 8 (full trail column), use trail LUT for (0,8).
         * Otherwise reuse the END fill index on the trail strip. */
        if (endIndex == 8)
            out->fillStripIndex = s_trailTileIndexByValueTrail[0][8];
        else
            out->fillStripIndex = endIndex;
    }
    else if (isTrailFull)
    {
        /* Full trail zone: fixed index 7 (value=0, trail=full in trail strip) */
        out->fillStripIndex = 7;
    }
    else if (isEndNext)
    {
        /* Next cell is END: align break cap to match END fill index */
        out->fillStripIndex = endIndex;
    }
    else
    {
        /* Default: fully filled (index 44) */
        out->fillStripIndex = STRIP_INDEX_FULL;
    }
}

/**
 * Classify the cap start cell and determine which strip + index to use.
 *
 * @param layout         Layout configuration
 * @param breakInfo            Pre-computed break zone info
 * @param cellFillIndex  Fill index of the cap start cell
 * @param segmentId          Segment ID of the cap start cell
 * @param out            [out] Classification result (strip + index)
 *
 * Cost: ~30-50 cycles (a few comparisons + LUT lookups)
 */
static inline void classify_cap_start(const GaugeLaneLayout *layout,
                                       const GaugeBreakInfo *breakInfo,
                                       u8 cellFillIndex,
                                       u8 segmentId,
                                       CapStartResult *out)
{
    classify_cap_start_with_strips(breakInfo, cellFillIndex,
                                   layout->tilesetCapStartBySegment[segmentId],
                                   layout->tilesetCapStartBreakBySegment[segmentId],
                                   layout->tilesetCapStartTrailBySegment[segmentId],
                                   out);
}

/* =============================================================================
   Bridge strip index computation
   =============================================================================

   A BRIDGE tile sits at the last cell of a segment (before a segment transition).
   Its fill index must match what the NEXT cell would show, so the visual
   transition between segments is seamless.

   Visual example with 2 segments (A and B):
     Cell:    [A][A][A][D][B][B][B][E]
                         ^
                    Bridge cell (D) mirrors what cell B+1 would show

   The bridge strip index is computed by looking ahead at the next fillIndex
   and determining what visual state it would have (full, partial, empty, trail).

   ============================================================================= */

/**
 * Compute the fill strip index for a bridge tile.
 *
 * The bridge tile mirrors the fill state of the next cell in fill order.
 * This ensures a seamless visual transition between segments.
 *
 * @param layout              Layout configuration
 * @param breakInfo                 Pre-computed break zone info (for rendered trail)
 * @param cellFillIndex       Fill index of the bridge cell
 * @param valuePixels         Current value fill in pixels
 * @param trailPixelsRendered Current trail fill in pixels (after blink)
 * @return Strip index to use for the bridge tile
 *
 * Cost: ~20-40 cycles (a few comparisons + optional fill computation)
 */
static inline u8 compute_bridge_strip_index(const GaugeLaneLayout *layout,
                                             const GaugeBreakInfo *breakInfo,
                                             u8 cellFillIndex,
                                             u16 valuePixels,
                                             u16 trailPixelsRendered)
{
    const u8 nextFillIndex = (u8)(cellFillIndex + 1);

    /* Case 1: next cell is END -> use END fill index */
    if (breakInfo->endFillIndex == nextFillIndex)
    {
        return s_tileIndexByValueTrail[breakInfo->endValuePxInTile][breakInfo->endTrailPxInTile];
    }

    /* Case 2: next cell is trail break #1 -> compute its fill */
    if (breakInfo->trailBreakActive && nextFillIndex == breakInfo->trailBreakFillIndex)
    {
        u8 nextValuePx = 0;
        u8 nextTrailPx = 0;
        compute_fill_for_fill_index(layout, nextFillIndex,
                                    valuePixels, trailPixelsRendered,
                                    &nextValuePx, &nextTrailPx);
        return s_tileIndexByValueTrail[nextValuePx][nextTrailPx];
    }

    /* Case 3: next cell is full trail (between value and trail edges) */
    if (breakInfo->trailBreakActive &&
        nextFillIndex > breakInfo->valueFillIndex &&
        nextFillIndex < breakInfo->trailFillIndex)
    {
        return STRIP_INDEX_FULL_TRAIL;
    }

    /* Default: fully filled (index 44) */
    return STRIP_INDEX_FULL;
}


/**
 * Determine if cap start/end are enabled for a given layout.
 *
 * Cap start applies to the first cell in fill order (fillIndex 0).
 * Cap end applies to the last cell in fill order (fillIndex length-1).
 * Rule: if layout is 1 cell and both caps are enabled, cap end wins.
 *
 * @param layout           Layout configuration
 * @param outCapStartEnabled [out] 1 if cap start is enabled
 * @param outCapEndEnabled   [out] 1 if cap end is enabled
 */
static inline void detect_caps_enabled(const GaugeLaneLayout *layout,
                                        u8 *outCapStartEnabled,
                                        u8 *outCapEndEnabled)
{
    const u8 cellStart = layout->cellIndexByFillIndex[0];
    const u8 cellEnd = layout->cellIndexByFillIndex[layout->length - 1];
    const u8 startSegId = layout->segmentIdByCell[cellStart];
    const u8 endSegId = layout->segmentIdByCell[cellEnd];

    *outCapEndEnabled = (layout->capEndBySegment[endSegId] != 0) &&
                        (layout->tilesetCapEndBySegment[endSegId] != NULL ||
                         layout->gainTilesetCapEndBySegment[endSegId] != NULL);
    *outCapStartEnabled = (layout->tilesetCapStartBySegment[startSegId] != NULL ||
                           layout->gainTilesetCapStartBySegment[startSegId] != NULL);

    /* Single-cell layout: cap end takes priority over cap start */
    if (layout->length == 1 && *outCapEndEnabled)
        *outCapStartEnabled = 0;
}

/**
 * Select base tileset for current trail mode.
 * - GAIN: use gain tileset if provided, else fallback to normal
 * - DAMAGE/NONE: use normal tileset (fallback to gain only if normal is NULL)
 */
static inline const u32 *select_base_strip(const u32 *normalStrip,
                                           const u32 *gainStrip,
                                           u8 trailMode)
{
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
        return gainStrip ? gainStrip : normalStrip;
    return normalStrip ? normalStrip : gainStrip;
}

/**
 * Select blink-off tileset for current trail mode.
 * No cross-mode fallback (gain blink-off is distinct).
 */
static inline const u32 *select_blink_strip(const u32 *normalBlinkStrip,
                                            const u32 *gainBlinkStrip,
                                            u8 trailMode)
{
    return (trailMode == GAUGE_TRAIL_STATE_GAIN) ? gainBlinkStrip : normalBlinkStrip;
}

/**
 * Apply blink-off strip overrides for a single cell.
 *
 * Checks whether the cell at cellFillIndex falls into a blink-off zone
 * (end, trail, or value break) using the actual trail zone (breakInfoActual).
 * If so, overrides the corresponding strip pointer to the blink-off variant.
 *
 * Shared by fill rendering to avoid
 * duplicating ~40 lines of identical classification logic.
 *
 * @return 1 if blink-off was applied, 0 otherwise.
 */
static inline u8 apply_blink_off_overrides(
    const GaugeBreakInfo *breakInfoActual,
    u8 cellFillIndex,
    const u32 *blinkOffEndStrip,
    const u32 *blinkOffTrailStrip,
    const u32 *blinkOffBodyStrip,
    const u32 **endStrip,
    const u32 **trailStrip,
    const u32 **bodyStrip)
{
    const u8 cellIsEnd = (breakInfoActual->endFillIndex != CACHE_INVALID_U8 &&
                          *endStrip && cellFillIndex == breakInfoActual->endFillIndex);
    const u8 cellIsTrailBreak =
        (breakInfoActual->trailBreakActive &&
         cellFillIndex == breakInfoActual->trailBreakFillIndex);
    const u8 cellIsTrailBreak2 =
        (breakInfoActual->trailBreakSecondActive &&
         cellFillIndex == breakInfoActual->trailBreakFillIndex2);
    const u8 cellIsTrailCovered =
        cellFillIndex > breakInfoActual->valueFillIndex &&
        cellFillIndex < breakInfoActual->trailFillIndex;
    const u8 cellIsTrailEdge =
        (!cellIsEnd &&
         breakInfoActual->trailFillIndex != breakInfoActual->valueFillIndex &&
         cellFillIndex == breakInfoActual->trailFillIndex);
    const u8 cellHasMixedValueAndTrail =
        (cellFillIndex == breakInfoActual->valueFillIndex &&
         (breakInfoActual->trailFillIndex > breakInfoActual->valueFillIndex ||
          (breakInfoActual->trailFillIndex == breakInfoActual->valueFillIndex &&
           breakInfoActual->trailPxInBreakCell > breakInfoActual->valuePxInBreakCell)));
    const u8 cellIsValueBreak = (breakInfoActual->valueBreakFillIndex != CACHE_INVALID_U8 &&
                                 cellFillIndex == breakInfoActual->valueBreakFillIndex);

    if (cellIsEnd && blinkOffEndStrip)
    {
        *endStrip = blinkOffEndStrip;
        return 1;
    }
    if ((cellIsTrailBreak || cellIsTrailBreak2 || cellIsTrailCovered || cellIsTrailEdge) &&
        blinkOffTrailStrip)
    {
        *trailStrip = blinkOffTrailStrip;
        return 1;
    }
    if ((cellIsTrailBreak || cellIsTrailBreak2 || cellIsTrailCovered || cellIsTrailEdge) &&
        blinkOffBodyStrip)
    {
        *bodyStrip = blinkOffBodyStrip;
        return 1;
    }
    if (cellHasMixedValueAndTrail && blinkOffBodyStrip)
    {
        *bodyStrip = blinkOffBodyStrip;
        return 1;
    }
    if (cellIsValueBreak && blinkOffBodyStrip)
    {
        *bodyStrip = blinkOffBodyStrip;
        return 1;
    }
    return 0;
}

/**
 * Check if any blink-off tileset is provided for the active trail mode.
 *
 * Scans all segments for non-NULL blink-off tilesets (damage or gain).
 * Called once per trail mode change to cache the result in GaugeLaneLayout.
 *
 * @param layout    Layout to inspect
 * @param trailMode GAUGE_TRAIL_STATE_DAMAGE or GAUGE_TRAIL_STATE_GAIN
 * @return 1 if any segment has blink-off tilesets, 0 otherwise
 */
static inline u8 layout_has_blink_off_mode(const GaugeLaneLayout *layout, u8 trailMode)
{
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
    {
        for (u8 i = 0; i < layout->segmentCount; i++)
        {
            if (layout->gainBlinkOffTilesetBySegment[i] ||
                layout->gainBlinkOffTilesetEndBySegment[i] ||
                layout->gainBlinkOffTilesetTrailBySegment[i] ||
                layout->gainBlinkOffTilesetBridgeBySegment[i] ||
                layout->gainBlinkOffTilesetCapStartBySegment[i] ||
                layout->gainBlinkOffTilesetCapEndBySegment[i] ||
                layout->gainBlinkOffTilesetCapStartBreakBySegment[i] ||
                layout->gainBlinkOffTilesetCapStartTrailBySegment[i])
            {
                return 1;
            }
        }
        return 0;
    }

    for (u8 i = 0; i < layout->segmentCount; i++)
    {
        if (layout->blinkOffTilesetBySegment[i] ||
            layout->blinkOffTilesetEndBySegment[i] ||
            layout->blinkOffTilesetTrailBySegment[i] ||
            layout->blinkOffTilesetBridgeBySegment[i] ||
            layout->blinkOffTilesetCapStartBySegment[i] ||
            layout->blinkOffTilesetCapEndBySegment[i] ||
            layout->blinkOffTilesetCapStartBreakBySegment[i] ||
            layout->blinkOffTilesetCapStartTrailBySegment[i])
        {
            return 1;
        }
    }
    return 0;
}


/* =============================================================================
   Unified cell decision types
   =============================================================================

   These types and helpers factor the cell rendering DECISION (which strip and
   tile index to use) out of the fill renderer.

   The decision is fixed-only:
   - Each cell selects a ROM strip and a tile index.
   - The fixed renderer then uploads that tile to the lane-owned VRAM slot.

   This eliminates ~320 lines of duplicated classification logic.
   ============================================================================= */

/**
 * Break zone cell type -- classifies a cell within the break zone.
 *
 * Used by compute_cell_decision() to determine which tileset and index to use.
 *
 *   |FULL|VALUE_BREAK|TRAIL_FULL|TRAIL_BREAK|TRAIL_BREAK2|END|EMPTY|
 *    <-- value -->|<---------- trail ---------->|
 */
typedef enum
{
    CELL_TYPE_END,           /* Value/trail frontier (uses end tileset) */
    CELL_TYPE_TRAIL_BREAK,   /* Trail edge cell, after VALUE_BREAK (uses trail tileset) */
    CELL_TYPE_TRAIL_BREAK2,  /* Second trail break, before END (uses trail tileset) */
    CELL_TYPE_TRAIL_FULL,    /* Fully covered by trail, no value (uses trail tileset) */
    CELL_TYPE_VALUE_BREAK,   /* Value edge cell, before trail zone (uses body tileset) */
    CELL_TYPE_REGION_FULL,   /* Region rendering: fully filled body */
    CELL_TYPE_REGION_EMPTY,  /* Region rendering: empty body */
    CELL_TYPE_REGION_TRAIL,  /* Region rendering: full trail */
    CELL_TYPE_DEFAULT        /* Standard fill computation (no special break zone) */
} CellBreakType;

/**
 * Classify a cell's break zone type based on pre-computed break info.
 *
 * @param breakInfoUsed  Break zone info (normal or blink-off adjusted)
 * @param cellFillIndex  Fill index of the cell to classify
 * @param hasEndStrip    1 if segment has an end tileset
 * @param hasTrailStrip  1 if segment has a trail tileset
 * @return CellBreakType indicating which visual mode applies
 */
static inline CellBreakType classify_break_zone_cell(
    const GaugeBreakInfo *breakInfoUsed,
    u8 cellFillIndex,
    u8 hasEndStrip,
    u8 hasTrailStrip)
{
    if (breakInfoUsed->endFillIndex != CACHE_INVALID_U8 &&
        hasEndStrip && cellFillIndex == breakInfoUsed->endFillIndex)
        return CELL_TYPE_END;

    if (breakInfoUsed->trailBreakActive && hasTrailStrip &&
        cellFillIndex == breakInfoUsed->trailBreakFillIndex)
        return CELL_TYPE_TRAIL_BREAK;

    if (breakInfoUsed->trailBreakSecondActive && hasTrailStrip &&
        cellFillIndex == breakInfoUsed->trailBreakFillIndex2)
        return CELL_TYPE_TRAIL_BREAK2;

    if (breakInfoUsed->trailBreakActive && hasTrailStrip &&
        cellFillIndex > breakInfoUsed->valueFillIndex &&
        cellFillIndex < breakInfoUsed->trailFillIndex)
        return CELL_TYPE_TRAIL_FULL;

    if (breakInfoUsed->valueBreakFillIndex != CACHE_INVALID_U8 &&
        cellFillIndex == breakInfoUsed->valueBreakFillIndex)
        return CELL_TYPE_VALUE_BREAK;

    if (breakInfoUsed->regionRenderActive)
    {
        if (cellFillIndex < breakInfoUsed->valueFillIndex)
            return CELL_TYPE_REGION_FULL;
        if (cellFillIndex > breakInfoUsed->trailFillIndex)
            return CELL_TYPE_REGION_EMPTY;
        if (cellFillIndex > breakInfoUsed->valueFillIndex &&
            cellFillIndex < breakInfoUsed->trailFillIndex)
            return CELL_TYPE_REGION_TRAIL;
    }

    return CELL_TYPE_DEFAULT;
}


/**
 * Cell decision type -- semantic tile class selected for a fill cell.
 */
typedef enum
{
    CELL_DECISION_CAP_END,          /* Cap end tile (dedicated VRAM per lane) */
    CELL_DECISION_CAP_START,        /* Cap start tile (dedicated VRAM per lane) */
    CELL_DECISION_BRIDGE,           /* Bridge tile (dedicated VRAM per segment) */
    CELL_DECISION_STANDARD_FULL,    /* Fully filled body tile (pre-loaded per segment) */
    CELL_DECISION_STANDARD_EMPTY,   /* Empty body tile (pre-loaded per segment) */
    CELL_DECISION_STANDARD_TRAIL,   /* Full trail tile (pre-loaded per segment) */
    CELL_DECISION_PARTIAL_END,      /* Partial END tile (streamed on demand) */
    CELL_DECISION_PARTIAL_TRAIL,    /* Partial trail tile (streamed on demand) */
    CELL_DECISION_PARTIAL_TRAIL2,   /* Second trail break tile (streamed on demand) */
    CELL_DECISION_PARTIAL_VALUE     /* Partial value tile (streamed on demand) */
} CellDecisionType;

/**
 * Cell rendering decision result.
 *
 * Produced by compute_cell_decision() for each cell.
 * Contains all information needed by the fixed fill renderer.
 */
typedef struct
{
    const u32 *strip;           /* Selected ROM strip (NULL = skip rendering) */
    u8 fillStripIndex;          /* Tile index within the strip (0..44 or 0..63) */
    CellDecisionType type;      /* Semantic tile class used by the local resolver */
    u8 capStartUsesBreak;       /* 1 if cap start uses break variant */
    u8 capStartUsesTrail;       /* 1 if cap start uses trail variant */
    u8 useBlinkVariant;         /* 1 when decision strip comes from blink-off variant */
} CellDecision;

/* Debug trace helpers (used by gauge_tick_and_render_fill/pip). */
#if GAUGE_ENABLE_TRACE
typedef struct
{
    u8 active;
    u8 laneIndex;
} GaugeTraceContext;

typedef struct
{
    u8 used;
    u8 segmentId;
    CellDecisionType baseLaneType;
    u8 baseLaneIdx;
    u8 terminalOverrideApplied;
    CellDecision decision;
} GaugeTraceCell;

static GaugeTraceContext s_traceContext = {0, 0};

static const char * const s_decisionTypeText[] = {
    [CELL_DECISION_CAP_END] = "CAP_END",
    [CELL_DECISION_CAP_START] = "CAP_START",
    [CELL_DECISION_BRIDGE] = "BRIDGE",
    [CELL_DECISION_STANDARD_FULL] = "STANDARD_FULL",
    [CELL_DECISION_STANDARD_EMPTY] = "STANDARD_EMPTY",
    [CELL_DECISION_STANDARD_TRAIL] = "STANDARD_TRAIL",
    [CELL_DECISION_PARTIAL_END] = "PARTIAL_END",
    [CELL_DECISION_PARTIAL_TRAIL] = "PARTIAL_TRAIL",
    [CELL_DECISION_PARTIAL_TRAIL2] = "PARTIAL_TRAIL2",
    [CELL_DECISION_PARTIAL_VALUE] = "PARTIAL_VALUE"
};

static const char * const s_trailModeText[] = {
    [GAUGE_TRAIL_STATE_NONE] = "NONE",
    [GAUGE_TRAIL_STATE_DAMAGE] = "DAMAGE",
    [GAUGE_TRAIL_STATE_GAIN] = "GAIN"
};

static const char * const s_pipStateText[] = {
    [PIP_STATE_EMPTY] = "EMPTY",
    [PIP_STATE_VALUE] = "VALUE",
    [PIP_STATE_LOSS] = "LOSS",
    [PIP_STATE_GAIN] = "GAIN",
    [PIP_STATE_BLINK_OFF] = "BLINK_OFF"
};

static inline const char *trace_text_lookup(const char * const *table,
                                            u16 entryCount,
                                            u8 entryIndex,
                                            const char *fallback)
{
    if (!table || entryIndex >= entryCount || !table[entryIndex])
        return fallback;
    return table[entryIndex];
}

static const char *decision_class_to_text(CellDecisionType type,
                                          u8 capStartUsesBreak,
                                          u8 capStartUsesTrail)
{
    switch (type)
    {
    case CELL_DECISION_CAP_END:
        return "CAP_END";
    case CELL_DECISION_CAP_START:
        if (capStartUsesTrail)
            return "CAP_START_TRAIL";
        if (capStartUsesBreak)
            return "CAP_START_BREAK";
        return "CAP_START";
    case CELL_DECISION_BRIDGE:
        return "BRIDGE";
    case CELL_DECISION_PARTIAL_END:
        return "END";
    case CELL_DECISION_STANDARD_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL2:
        return "TRAIL";
    case CELL_DECISION_STANDARD_FULL:
    case CELL_DECISION_STANDARD_EMPTY:
    case CELL_DECISION_PARTIAL_VALUE:
    default:
        return "BODY";
    }
}

static const char *render_state_to_text(u8 trailMode,
                                        u8 blinkOffActive,
                                        u16 valuePixels,
                                        u16 trailPixelsRendered)
{
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
        return "GAIN";
    if (blinkOffActive)
        return "BLINK_OFF";
    if (trailPixelsRendered > valuePixels)
        return "TRAIL";
    return "NORMAL";
}

#endif

/**
 * Fill loop context -- loop-invariant state shared across fill cells.
 *
 * Computed once before the cell loop by init_fill_loop_context().
 * Contains break zone info, cap detection, and rendering parameters.
 */
typedef struct
{
    GaugeBreakInfo breakInfo;         /* Break zone with rendered trail (blink applied) */
    GaugeBreakInfo breakInfoActual;   /* Break zone with actual trail (for bridges) */
    u16 valuePixels;
    u16 trailPixelsRendered;
    u16 trailPixelsActual;
    u8 breakLow;                      /* Early-exit lower bound (fill index) */
    u8 breakHigh;                     /* Early-exit upper bound (fill index) */
    u8 capStartCellIndex;
    u8 capEndCellIndex;
    u8 capStartSegId;
    u8 capEndSegId;
    u8 capStartEnabled;
    u8 capEndEnabled;
    u8 blinkOffActive;
    u8 trailMode;
} FillLoopContext;


/**
 * Initialize fill loop context with loop-invariant state.
 *
 * Computes break zones, early-exit boundaries, and cap detection.
 * Called once before the fill cell loop.
 *
 * @param layout              Layout configuration
 * @param valuePixels         Current value fill in pixels
 * @param trailPixelsRendered Trail pixels after blink
 * @param trailPixelsActual   Trail pixels before blink (for bridges)
 * @param blinkOffActive      1 if blink-off rendering is active
 * @param trailMode           GAUGE_TRAIL_STATE_DAMAGE/GAIN/NONE
 * @param ctx                 [out] Initialized loop context
 */
static inline void init_fill_loop_context(const GaugeLaneLayout *layout,
                                          u16 valuePixels,
                                          u16 trailPixelsRendered,
                                          u16 trailPixelsActual,
                                          u8 blinkOffActive,
                                          u8 trailMode,
                                          FillLoopContext *ctx)
{
    ctx->valuePixels = valuePixels;
    ctx->trailPixelsRendered = trailPixelsRendered;
    ctx->trailPixelsActual = trailPixelsActual;
    ctx->blinkOffActive = blinkOffActive;
    ctx->trailMode = trailMode;

    /* Break zone computation:
     * breakInfo:       uses rendered trail (blink applied) for normal classification
     * breakInfoActual: uses actual trail (ignoring blink) for bridge visibility */
    compute_break_info(layout, valuePixels, trailPixelsRendered, trailMode,
                       &ctx->breakInfo);
    ctx->breakInfoActual = ctx->breakInfo;
    if (trailPixelsActual != trailPixelsRendered)
        compute_break_info(layout, valuePixels, trailPixelsActual, trailMode,
                           &ctx->breakInfoActual);

    /* Early-exit boundaries: cells outside [breakLow..breakHigh] are trivially
     * full or empty (~70-80% of cells). Skips full classification. */
    ctx->breakLow = (ctx->breakInfo.valueBreakFillIndex != CACHE_INVALID_U8)
                   ? ctx->breakInfo.valueBreakFillIndex
                   : ctx->breakInfo.valueFillIndex;
    ctx->breakHigh = ctx->breakInfo.trailFillIndex;

    /* Cap detection */
    ctx->capStartCellIndex = layout->cellIndexByFillIndex[0];
    ctx->capEndCellIndex = layout->cellIndexByFillIndex[layout->length - 1];
    ctx->capStartSegId = layout->segmentIdByCell[ctx->capStartCellIndex];
    ctx->capEndSegId = layout->segmentIdByCell[ctx->capEndCellIndex];
    ctx->capStartEnabled = layout->capStartEnabled;
    ctx->capEndEnabled = layout->capEndEnabled;
}


/**
 * Compute the rendering decision for a single cell.
 *
 * This is the unified decision function shared by the fixed fill pipeline. It determines:
 * - Which ROM strip to use (body, end, trail, bridge, cap, blink-off variant)
 * - Which tile index within that strip
 * - Which semantic cell type is active
 *
 * The caller provides bodyStrip so the local lane resolver can reuse the same
 * classification logic with either base-lane or lane-local strips.
 *
 * All other strips (end, trail, bridge, caps, blink variants) are looked up
 * from the layout inside this function.
 *
 * @param layout           Layout configuration
 * @param ctx              Loop-invariant context (from init_fill_loop_context)
 * @param cellIndex        Cell index in layout
 * @param cellFillIndex    Fill index of the cell (from fillIndexByCell)
 * @param segmentId        Segment ID of the cell
 * @param bodyStrip        Pre-computed body strip (mode-specific source)
 * @param out              [out] Rendering decision
 */
static inline void compute_cell_decision(const GaugeLaneLayout *layout,
                                         const FillLoopContext *ctx,
                                         u8 cellIndex,
                                         u8 cellFillIndex,
                                         u8 segmentId,
                                         const u32 *bodyStrip,
                                         CellDecision *out)
{
    out->capStartUsesBreak = 0;
    out->capStartUsesTrail = 0;
    out->useBlinkVariant = 0;

    /* Blink-off body strip lookup is only needed when blink-off is active. */
    const u32 *blinkOffBodyStrip = NULL;
    if (ctx->blinkOffActive)
    {
        blinkOffBodyStrip = select_blink_strip(
            layout->blinkOffTilesetBySegment[segmentId],
            layout->gainBlinkOffTilesetBySegment[segmentId],
            ctx->trailMode);
    }

    /* === Cap end (always uses its dedicated tileset) === */
    if (cellIndex == ctx->capEndCellIndex &&
        ctx->capEndEnabled &&
        segmentId == ctx->capEndSegId)
    {
        const u32 *capEndStrip = select_base_strip(
            layout->tilesetCapEndBySegment[segmentId],
            layout->gainTilesetCapEndBySegment[segmentId],
            ctx->trailMode);
        u16 capTrailPixels = ctx->trailPixelsRendered;

        const u32 *capEndBlinkStrip = select_blink_strip(
            layout->blinkOffTilesetCapEndBySegment[segmentId],
            layout->gainBlinkOffTilesetCapEndBySegment[segmentId],
            ctx->trailMode);

        u8 capUsesBlink = 0;
        if (ctx->blinkOffActive &&
            capEndBlinkStrip &&
            is_cell_in_trail_zone(&ctx->breakInfoActual, cellFillIndex))
        {
            capEndStrip = capEndBlinkStrip;
            capTrailPixels = ctx->trailPixelsActual;
            capUsesBlink = 1;
        }

        u8 capValuePx = 0;
        u8 capTrailPx = 0;
        compute_fill_for_cell(layout, cellIndex, ctx->valuePixels, capTrailPixels,
                              &capValuePx, &capTrailPx);

        out->type = CELL_DECISION_CAP_END;
        out->strip = capEndStrip;
        out->fillStripIndex = s_tileIndexByValueTrail[capValuePx][capTrailPx];
        out->useBlinkVariant = capUsesBlink;
        return;
    }

    /* === Cap start (fixed border at cell 0) === */
    if (cellIndex == ctx->capStartCellIndex &&
        ctx->capStartEnabled &&
        segmentId == ctx->capStartSegId)
    {
        const u32 *capStartStrip = select_base_strip(
            layout->tilesetCapStartBySegment[segmentId],
            layout->gainTilesetCapStartBySegment[segmentId],
            ctx->trailMode);

        if (capStartStrip)
        {
            const u32 *capStartBreakStrip = select_base_strip(
                layout->tilesetCapStartBreakBySegment[segmentId],
                layout->gainTilesetCapStartBreakBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartTrailStrip = select_base_strip(
                layout->tilesetCapStartTrailBySegment[segmentId],
                layout->gainTilesetCapStartTrailBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartBlinkStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartBlinkBreakStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartBreakBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartBreakBySegment[segmentId],
                ctx->trailMode);
            const u32 *capStartBlinkTrailStrip = select_blink_strip(
                layout->blinkOffTilesetCapStartTrailBySegment[segmentId],
                layout->gainBlinkOffTilesetCapStartTrailBySegment[segmentId],
                ctx->trailMode);

            CapStartResult capResult;
            u8 capUsesBlink = 0;
            if (ctx->blinkOffActive)
            {
                CapStartResult capActual;
                classify_cap_start_with_strips(&ctx->breakInfoActual,
                                               cellFillIndex,
                                               capStartStrip,
                                               capStartBreakStrip,
                                               capStartTrailStrip,
                                               &capActual);

                const u8 inTrailZone = is_cell_in_trail_zone(&ctx->breakInfoActual,
                                                              cellFillIndex);
                const u32 *blinkStrip = NULL;

                if (inTrailZone)
                {
                    if (capActual.usesTrail)
                        blinkStrip = capStartBlinkTrailStrip;
                    else if (capActual.usesBreak)
                        blinkStrip = capStartBlinkBreakStrip;
                    else
                        blinkStrip = capStartBlinkStrip;
                }

                if (blinkStrip)
                {
                    capActual.strip = blinkStrip;
                    capResult = capActual;
                    capUsesBlink = 1;
                }
                else
                {
                    classify_cap_start_with_strips(&ctx->breakInfo,
                                                   cellFillIndex,
                                                   capStartStrip,
                                                   capStartBreakStrip,
                                                   capStartTrailStrip,
                                                   &capResult);
                }
            }
            else
            {
                classify_cap_start_with_strips(&ctx->breakInfo,
                                               cellFillIndex,
                                               capStartStrip,
                                               capStartBreakStrip,
                                               capStartTrailStrip,
                                               &capResult);
            }

            out->type = CELL_DECISION_CAP_START;
            out->strip = capResult.strip;
            out->fillStripIndex = capResult.fillStripIndex;
            out->capStartUsesBreak = capResult.usesBreak;
            out->capStartUsesTrail = capResult.usesTrail;
            out->useBlinkVariant = capUsesBlink;
            return;
        }
        /* capStartStrip == NULL: fall through to normal cell handling */
    }

    /* === Bridge/BREAK forced rendering (D/B before gauge END) ===
     * Bridge cells sit at segment boundaries. When the gauge's value/trail
     * edge is past this cell, the bridge shows a transition tile.
     * breakInfoActual uses trailPixelsActual (ignoring blink) so bridges don't
     * flicker during blink phase. */
    if (ctx->breakInfoActual.endFillIndex != CACHE_INVALID_U8)
    {
        /* Bridge end cell: last cell of a segment with a bridge tileset */
        if (layout->bridgeEndByFillIndex[cellFillIndex] &&
            ctx->breakInfoActual.endFillIndex > cellFillIndex &&
            ctx->breakInfoActual.valueFillIndex > cellFillIndex)
        {
            const u8 nextFillIndex = (u8)(cellFillIndex + 1);
            const u8 bridgeInTrailZone = is_cell_in_trail_zone(
                &ctx->breakInfoActual, nextFillIndex);

            const u32 *bridgeStrip = select_base_strip(
                layout->tilesetBridgeBySegment[segmentId],
                layout->gainTilesetBridgeBySegment[segmentId],
                ctx->trailMode);

            if (bridgeStrip)
            {
                const u32 *blinkOffBridgeStrip = select_blink_strip(
                    layout->blinkOffTilesetBridgeBySegment[segmentId],
                    layout->gainBlinkOffTilesetBridgeBySegment[segmentId],
                    ctx->trailMode);

                const u8 useBlinkOffBridge = (ctx->blinkOffActive &&
                                              blinkOffBridgeStrip &&
                                              bridgeInTrailZone);

                const GaugeBreakInfo *bridgeBreakInfo = useBlinkOffBridge
                    ? &ctx->breakInfoActual : &ctx->breakInfo;
                const u16 bridgeTrailPixels = useBlinkOffBridge
                    ? ctx->trailPixelsActual : ctx->trailPixelsRendered;
                const u32 *bridgeStripUse = useBlinkOffBridge
                    ? blinkOffBridgeStrip : bridgeStrip;
                const u8 bridgeIdx = compute_bridge_strip_index(
                    layout, bridgeBreakInfo, cellFillIndex,
                    ctx->valuePixels, bridgeTrailPixels);

                out->type = CELL_DECISION_BRIDGE;
                out->strip = bridgeStripUse;
                out->fillStripIndex = bridgeIdx;
                out->useBlinkVariant = useBlinkOffBridge;
            }
            else
            {
                /* No bridge tileset: fall back to a fully filled body tile. */
                const u32 *fallbackStrip = bodyStrip;
                if (ctx->blinkOffActive && blinkOffBodyStrip && bridgeInTrailZone)
                    fallbackStrip = blinkOffBodyStrip;

                out->type = CELL_DECISION_BRIDGE;
                out->strip = fallbackStrip;
                out->fillStripIndex = STRIP_INDEX_FULL;
                out->useBlinkVariant = (fallbackStrip == blinkOffBodyStrip) ? 1 : 0;
            }
            return;
        }

        /* Forced BREAK cell: cell before a bridge that must show as fully filled */
        if (layout->bridgeBreakByFillIndex[cellFillIndex])
        {
            const u8 boundaryFillIndex = layout->bridgeBreakBoundaryByFillIndex[cellFillIndex];
            if (ctx->breakInfoActual.endFillIndex > boundaryFillIndex &&
                ctx->breakInfoActual.valueFillIndex > boundaryFillIndex)
            {
                const u32 *breakStripUse = bodyStrip;
                if (ctx->blinkOffActive && blinkOffBodyStrip &&
                    is_cell_in_trail_zone(&ctx->breakInfoActual, cellFillIndex))
                {
                    breakStripUse = blinkOffBodyStrip;
                }
                out->type = CELL_DECISION_STANDARD_FULL;
                out->strip = breakStripUse;
                out->fillStripIndex = STRIP_INDEX_FULL;
                out->useBlinkVariant = (breakStripUse == blinkOffBodyStrip) ? 1 : 0;
                return;
            }
        }
    }

    /* === Early-exit: trivial cells (FULL or EMPTY) ===
     * ~70-80% of cells hit this path, skipping the full classification below.
     * Blink-off needs per-cell classification for mixed segments. */
    if (!ctx->blinkOffActive)
    {
        if (cellFillIndex < ctx->breakLow)
        {
            out->type = CELL_DECISION_STANDARD_FULL;
            out->strip = bodyStrip;
            out->fillStripIndex = STRIP_INDEX_FULL;
            return;
        }
        if (cellFillIndex > ctx->breakHigh)
        {
            out->type = CELL_DECISION_STANDARD_EMPTY;
            out->strip = bodyStrip;
            out->fillStripIndex = STRIP_INDEX_EMPTY;
            return;
        }
    }

    /* === Break zone: full classification needed ===
     * Lazy strip computation: only break-zone cells (~20-30%) need these.
     * bodyStrip is provided by the caller; blink-off body strip is selected here. */
    const u32 *endStrip = select_base_strip(
        layout->tilesetEndBySegment[segmentId],
        layout->gainTilesetEndBySegment[segmentId],
        ctx->trailMode);
    const u32 *trailStrip = select_base_strip(
        layout->tilesetTrailBySegment[segmentId],
        layout->gainTilesetTrailBySegment[segmentId],
        ctx->trailMode);
    const u32 *blinkOffEndStrip = select_blink_strip(
        layout->blinkOffTilesetEndBySegment[segmentId],
        layout->gainBlinkOffTilesetEndBySegment[segmentId],
        ctx->trailMode);
    const u32 *blinkOffTrailStrip = select_blink_strip(
        layout->blinkOffTilesetTrailBySegment[segmentId],
        layout->gainBlinkOffTilesetTrailBySegment[segmentId],
        ctx->trailMode);

    /* Mutable copies for blink-off override */
    const u32 *bodyStripUse = bodyStrip;
    const u32 *endStripUse = endStrip;
    const u32 *trailStripUse = trailStrip;

    const GaugeBreakInfo *breakInfoUsed = &ctx->breakInfo;
    u16 trailPixelsUse = ctx->trailPixelsRendered;
    u8 useBlinkOff = 0;

    if (ctx->blinkOffActive)
    {
        useBlinkOff = apply_blink_off_overrides(
            &ctx->breakInfoActual, cellFillIndex,
            blinkOffEndStrip, blinkOffTrailStrip, blinkOffBodyStrip,
            &endStripUse, &trailStripUse, &bodyStripUse);
        if (useBlinkOff)
        {
            breakInfoUsed = &ctx->breakInfoActual;
            trailPixelsUse = ctx->trailPixelsActual;
        }
    }

    /* Trail strip with body fallback (defensive). */
    const u32 *trailStripFinal = trailStripUse ? trailStripUse : bodyStripUse;
    /* NOTE: full-trail index depends on the strip type:
     * - TRAIL strip uses s_trailTileIndexByValueTrail[0][8] (index 7)
     * - BODY strip uses STRIP_INDEX_FULL_TRAIL (index 8)
     * This preserves the previous body fallback when no dedicated trail strip exists. */
    const u8 fullTrailIndex = (trailStripUse != NULL)
        ? s_trailTileIndexByValueTrail[0][8]
        : STRIP_INDEX_FULL_TRAIL;

    const CellBreakType cellType = classify_break_zone_cell(
        breakInfoUsed, cellFillIndex,
        (endStripUse != NULL), (trailStripUse != NULL));

    switch (cellType)
    {
    case CELL_TYPE_END:
        out->type = CELL_DECISION_PARTIAL_END;
        out->strip = endStripUse;
        out->fillStripIndex = s_tileIndexByValueTrail
            [breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_TRAIL_BREAK:
        out->type = CELL_DECISION_PARTIAL_TRAIL;
        out->strip = trailStripFinal;
        out->fillStripIndex = breakInfoUsed->trailBreakSecondActive
            ? s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell][8]
            : s_trailTileIndexByValueTrail[breakInfoUsed->valuePxInBreakCell]
                                          [breakInfoUsed->trailPxInBreakCell];
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_TRAIL_BREAK2:
    {
        out->type = CELL_DECISION_PARTIAL_TRAIL2;
        out->strip = trailStripFinal;
        const u8 endIndex = s_tileIndexByValueTrail
            [breakInfoUsed->endValuePxInTile][breakInfoUsed->endTrailPxInTile];
        out->fillStripIndex = (endIndex == 8)
            ? s_trailTileIndexByValueTrail[0][endIndex]
            : endIndex;
        out->useBlinkVariant = useBlinkOff;
        return;
    }

    case CELL_TYPE_TRAIL_FULL:
        out->type = CELL_DECISION_STANDARD_TRAIL;
        out->strip = trailStripFinal;
        out->fillStripIndex = fullTrailIndex;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_VALUE_BREAK:
        out->type = CELL_DECISION_PARTIAL_VALUE;
        out->strip = bodyStripUse;
        out->fillStripIndex = breakInfoUsed->trailBreakActive
            ? breakInfoUsed->valuePxInBreakCell
            : s_tileIndexByValueTrail[breakInfoUsed->endValuePxInTile]
                                     [breakInfoUsed->endTrailPxInTile];
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_REGION_FULL:
        out->type = CELL_DECISION_STANDARD_FULL;
        out->strip = bodyStripUse;
        out->fillStripIndex = STRIP_INDEX_FULL;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_REGION_EMPTY:
        out->type = CELL_DECISION_STANDARD_EMPTY;
        out->strip = bodyStripUse;
        out->fillStripIndex = STRIP_INDEX_EMPTY;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_REGION_TRAIL:
        out->type = CELL_DECISION_STANDARD_TRAIL;
        out->strip = trailStripFinal;
        out->fillStripIndex = fullTrailIndex;
        out->useBlinkVariant = useBlinkOff;
        return;

    case CELL_TYPE_DEFAULT:
    {
        /* Sub-routing by cell position relative to value/trail edges.
         * Standard cells use shortcuts; partial cells need fill computation. */
        const u32 *defaultStrip = bodyStripUse;

        /* endStrip override: if this cell is the END cell, use endStrip */
        if (breakInfoUsed->endFillIndex != CACHE_INVALID_U8 &&
            cellFillIndex == breakInfoUsed->endFillIndex &&
            endStripUse)
        {
            defaultStrip = endStripUse;
        }

        if (cellFillIndex < breakInfoUsed->valueFillIndex)
        {
            out->type = CELL_DECISION_STANDARD_FULL;
            out->strip = defaultStrip;
            out->fillStripIndex = STRIP_INDEX_FULL;
            out->useBlinkVariant = useBlinkOff;
        }
        else if (cellFillIndex > breakInfoUsed->valueFillIndex &&
                 cellFillIndex < breakInfoUsed->trailFillIndex)
        {
            out->type = CELL_DECISION_STANDARD_TRAIL;
            out->strip = trailStripFinal;
            out->fillStripIndex = fullTrailIndex;
            out->useBlinkVariant = useBlinkOff;
        }
        else if (cellFillIndex > breakInfoUsed->trailFillIndex)
        {
            out->type = CELL_DECISION_STANDARD_EMPTY;
            out->strip = defaultStrip;
            out->fillStripIndex = STRIP_INDEX_EMPTY;
            out->useBlinkVariant = useBlinkOff;
        }
        else
        {
            /* Partial cell: compute exact pixel fill */
            u8 valuePxInTile = 0;
            u8 trailPxInTile = 0;
            compute_fill_for_cell(layout, cellIndex,
                                  ctx->valuePixels, trailPixelsUse,
                                  &valuePxInTile, &trailPxInTile);

            if (cellFillIndex == breakInfoUsed->valueFillIndex &&
                cellFillIndex == breakInfoUsed->trailFillIndex)
            {
                /* Both value and trail edges in same cell */
                out->type = CELL_DECISION_PARTIAL_VALUE;
                out->strip = defaultStrip;
                out->fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
                out->useBlinkVariant = useBlinkOff;
            }
            else if (cellFillIndex == breakInfoUsed->valueFillIndex)
            {
                /* Value edge cell (trail is full = 8) */
                out->type = CELL_DECISION_PARTIAL_VALUE;
                out->strip = defaultStrip;
                out->fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
                out->useBlinkVariant = useBlinkOff;
            }
            else
            {
                /* Trail edge cell (value is 0) */
                out->type = CELL_DECISION_PARTIAL_TRAIL;
                out->strip = defaultStrip;
                out->fillStripIndex = s_tileIndexByValueTrail[valuePxInTile][trailPxInTile];
                out->useBlinkVariant = useBlinkOff;
            }
        }
        return;
    }
    }

    /* Fallback (should never be reached) */
    out->type = CELL_DECISION_STANDARD_EMPTY;
    out->strip = bodyStripUse;
    out->fillStripIndex = STRIP_INDEX_EMPTY;
    out->useBlinkVariant = useBlinkOff;
}


typedef struct
{
    u16 basetile;
    u16 firstX;
    u16 firstY;
    u16 lastX;
    u16 lastY;
    u16 tiles[GAUGE_MAX_LENGTH];
    u8 length;
    u8 active;
} GaugeTilemapSpan;

typedef struct
{
    const u32 *strip;
    u16 startVramTile;
    u16 lastVramTile;
    u8 startStripIndex;
    u8 lastStripIndex;
    u8 length;
    u8 active;
} GaugeTileUploadSpan;

static inline void tilemap_span_reset(GaugeTilemapSpan *span)
{
    span->active = 0;
    span->length = 0;
    span->basetile = 0;
    span->firstX = 0;
    span->firstY = 0;
    span->lastX = 0;
    span->lastY = 0;
}

static inline void tilemap_span_begin(GaugeTilemapSpan *span,
                                      u16 basetile,
                                      u16 x,
                                      u16 y,
                                      u16 tileIndex)
{
    span->active = 1;
    span->length = 1;
    span->basetile = basetile;
    span->firstX = x;
    span->firstY = y;
    span->lastX = x;
    span->lastY = y;
    span->tiles[0] = tileIndex;
}

static inline u8 tilemap_span_can_append(const GaugeTilemapSpan *span,
                                         u8 orientation,
                                         u16 basetile,
                                         u16 x,
                                         u16 y)
{
    if (!span->active)
        return 1;
    if (span->basetile != basetile || span->length >= GAUGE_MAX_LENGTH)
        return 0;

    if (orientation == GAUGE_ORIENT_HORIZONTAL)
        return (y == span->firstY) && (x == (u16)(span->lastX + 1));

    return (x == span->firstX) && ((u16)(y + 1) == span->lastY);
}

static inline void tilemap_span_append(GaugeTilemapSpan *span,
                                       u16 x,
                                       u16 y,
                                       u16 tileIndex)
{
    span->tiles[span->length] = tileIndex;
    span->length++;
    span->lastX = x;
    span->lastY = y;
}

static inline void tilemap_span_push(GaugeTilemapSpan *span,
                                     u8 orientation,
                                     u16 basetile,
                                     u16 x,
                                     u16 y,
                                     u16 tileIndex)
{
    if (!tilemap_span_can_append(span, orientation, basetile, x, y))
    {
        u16 reversedTiles[GAUGE_MAX_LENGTH];
        if (orientation == GAUGE_ORIENT_HORIZONTAL)
        {
            VDP_setTileMapDataRowEx(WINDOW,
                                    span->tiles,
                                    span->basetile,
                                    span->firstY,
                                    span->firstX,
                                    span->length,
                                    DMA_QUEUE);
        }
        else
        {
            for (u8 i = 0; i < span->length; i++)
                reversedTiles[i] = span->tiles[(u8)(span->length - 1 - i)];

            VDP_setTileMapDataColumnEx(WINDOW,
                                       reversedTiles,
                                       span->basetile,
                                       span->firstX,
                                       span->lastY,
                                       span->length,
                                       1,
                                       DMA_QUEUE);
        }

        tilemap_span_reset(span);
    }

    if (!span->active)
        tilemap_span_begin(span, basetile, x, y, tileIndex);
    else
        tilemap_span_append(span, x, y, tileIndex);
}

static inline void tilemap_span_flush(GaugeTilemapSpan *span, u8 orientation)
{
    if (!span->active)
        return;

    if (orientation == GAUGE_ORIENT_HORIZONTAL)
    {
        VDP_setTileMapDataRowEx(WINDOW,
                                span->tiles,
                                span->basetile,
                                span->firstY,
                                span->firstX,
                                span->length,
                                DMA_QUEUE);
    }
    else
    {
        u16 reversedTiles[GAUGE_MAX_LENGTH];

        for (u8 i = 0; i < span->length; i++)
            reversedTiles[i] = span->tiles[(u8)(span->length - 1 - i)];

        VDP_setTileMapDataColumnEx(WINDOW,
                                   reversedTiles,
                                   span->basetile,
                                   span->firstX,
                                   span->lastY,
                                   span->length,
                                   1,
                                   DMA_QUEUE);
    }

    tilemap_span_reset(span);
}

static inline void tile_upload_span_reset(GaugeTileUploadSpan *span)
{
    span->active = 0;
    span->strip = NULL;
    span->startVramTile = 0;
    span->lastVramTile = 0;
    span->startStripIndex = 0;
    span->lastStripIndex = 0;
    span->length = 0;
}

static inline void tile_upload_span_begin(GaugeTileUploadSpan *span,
                                          const u32 *strip,
                                          u16 vramTile,
                                          u8 stripIndex)
{
    span->active = 1;
    span->strip = strip;
    span->startVramTile = vramTile;
    span->lastVramTile = vramTile;
    span->startStripIndex = stripIndex;
    span->lastStripIndex = stripIndex;
    span->length = 1;
}

static inline u8 tile_upload_span_can_append(const GaugeTileUploadSpan *span,
                                             const u32 *strip,
                                             u16 vramTile,
                                             u8 stripIndex)
{
    if (!span->active)
        return 1;

    return span->strip == strip &&
           span->length < GAUGE_MAX_LENGTH &&
           vramTile == (u16)(span->lastVramTile + 1) &&
           stripIndex == (u8)(span->lastStripIndex + 1);
}

static inline void tile_upload_span_append(GaugeTileUploadSpan *span,
                                           u16 vramTile,
                                           u8 stripIndex)
{
    span->lastVramTile = vramTile;
    span->lastStripIndex = stripIndex;
    span->length++;
}

static inline void tile_upload_span_push(GaugeTileUploadSpan *span,
                                         const u32 *strip,
                                         u16 vramTile,
                                         u8 stripIndex)
{
    if (!strip)
        return;

    if (!tile_upload_span_can_append(span, strip, vramTile, stripIndex))
    {
        const u32 *src = span->strip + FILL_IDX_TO_OFFSET(span->startStripIndex);
        VDP_loadTileData(src, span->startVramTile, span->length, DMA_QUEUE);
        tile_upload_span_reset(span);
    }

    if (!span->active)
        tile_upload_span_begin(span, strip, vramTile, stripIndex);
    else
        tile_upload_span_append(span, vramTile, stripIndex);
}

static inline void tile_upload_span_flush(GaugeTileUploadSpan *span)
{
    if (!span->active)
        return;

    const u32 *src = span->strip + FILL_IDX_TO_OFFSET(span->startStripIndex);
    VDP_loadTileData(src, span->startVramTile, span->length, DMA_QUEUE);
    tile_upload_span_reset(span);
}

/**
 * Upload a fill tile from ROM strip to VRAM.
 */
static inline void upload_fill_tile(const u32 *strip, u8 fillIndex, u16 vramTile, u8 dmaMode)
{
    const u32 *src = strip + FILL_IDX_TO_OFFSET(fillIndex);
    VDP_loadTileData(src, vramTile, 1, dmaMode);
}

static inline u8 segment_tileset_array_has_any(const u32 * const *tilesetsBySegment,
                                               u8 segmentCount)
{
    if (!tilesetsBySegment)
        return 0;

    for (u8 segmentId = 0; segmentId < segmentCount; segmentId++)
    {
        if (tilesetsBySegment[segmentId] != NULL)
            return 1;
    }
    return 0;
}

static inline u8 segment_flag_array_has_any(const u8 *flagsBySegment,
                                            u8 segmentCount)
{
    if (!flagsBySegment)
        return 0;

    for (u8 segmentId = 0; segmentId < segmentCount; segmentId++)
    {
        if (flagsBySegment[segmentId] != 0)
            return 1;
    }
    return 0;
}

/* Layout lazy-allocation helpers (defined later in GaugeLaneLayout section). */
static u8 layout_ensure_segment_tileset_storage(const u32 ***segmentTilesetsBySegment,
                                                u8 segmentCount);
static u8 layout_ensure_optional_values(u8 **values,
                                        u16 valueCount,
                                        const u8 *defaultValues,
                                        u8 defaultValue);
static void layout_free_optional_ptr(void **ptr,
                                     const void *defaultViewA,
                                     const void *defaultViewB,
                                     const void *defaultViewC);
static void layout_reset_bridge_views(GaugeLaneLayout *layout);
static void layout_reset_pip_runtime_views(GaugeLaneLayout *layout);
static inline u8 layout_sync_bridge_luts(GaugeLaneLayout *layout);
static inline u8 layout_sync_pip_luts(GaugeLaneLayout *layout, u16 maxRenderCount);
static u8 layout_sync_optional_segment_tilesets_by_usage(u8 hasAny,
                                                          const u32 ***segmentTilesetsBySegment,
                                                          u8 segmentCount);
static u8 layout_sync_optional_values_by_usage(u8 hasAny,
                                               u8 **values,
                                               u16 valueCount,
                                               const u8 *defaultValues,
                                               u8 defaultValue);
static void layout_copy_segment_tilesets(const u32 **destinationTilesets,
                                         const u32 * const *sourceTilesets,
                                         u8 segmentCount);

#define layout_sync_optional_segment_flags(hasAny, flagsBySegment, segmentCount, defaultFlags, defaultFlag) \
    layout_sync_optional_values_by_usage((hasAny), (flagsBySegment), (segmentCount), (defaultFlags), (defaultFlag))

#define layout_sync_optional_cell_flags(hasAny, flagsByCell, cellCount, defaultFlags, defaultFlag) \
    layout_sync_optional_values_by_usage((hasAny), (flagsByCell), (cellCount), (defaultFlags), (defaultFlag))

#define layout_sync_optional_pip_lut(hasAny, pipLut, pipLutCount, defaultLut, defaultValue) \
    layout_sync_optional_values_by_usage((hasAny), (pipLut), (pipLutCount), (defaultLut), (defaultValue))

/* GaugeLaneLayout helpers used only by the module build/runtime plumbing. */
/**
 * Build bridge/break lookup tables (by fillIndex).
 *
 * Scans consecutive cells in fill order. At each segment boundary
 * (where segmentId changes between adjacent cells), applies these rules:
 *
 * - If outgoing segment has a BRIDGE tileset:
 *     -> last cell of the segment is flagged as bridgeEnd (shows bridge tile)
 *     -> cell before that is flagged as bridgeBreak (forced BREAK, index 44)
 * - If outgoing segment has NO BRIDGE tileset:
 *     -> last cell of the segment is flagged as bridgeBreak (forced BREAK)
 *
 * Example with segments A (has bridge) and B (no bridge):
 *   fillIndex:   0   1   2   3   4   5   6   7
 *   segment:     A   A   A   A   B   B   B   B
 *                          ^   ^
 *                   break(2)  bridge(3)
 *
 * Called after fill order is refreshed.
 */
static void build_bridge_luts(GaugeLaneLayout *layout)
{
    const u8 hasBaseBridge =
        segment_tileset_array_has_any(layout->tilesetBridgeBySegment, layout->segmentCount);
    const u8 hasGainBridge =
        segment_tileset_array_has_any(layout->gainTilesetBridgeBySegment, layout->segmentCount);

    if (!hasBaseBridge && !hasGainBridge)
    {
        layout_reset_bridge_views(layout);
        return;
    }

    if (!layout_sync_bridge_luts(layout))
    {
        layout_reset_bridge_views(layout);
        return;
    }

    memset(layout->bridgeEndByFillIndex, 0, layout->length);
    memset(layout->bridgeBreakByFillIndex, 0, layout->length);
    memset(layout->bridgeBreakBoundaryByFillIndex, 0, layout->length);

    /* Scan adjacent cell pairs looking for segment boundaries.
     * At each boundary we mark:
     *   bridgeEnd[i]   = 1  -> cell i is a bridge cell (last cell of its segment,
     *                          rendered with the bridge tileset instead of body)
     *   bridgeBreak[j] = 1  -> cell j is a BREAK cell (forced fully-filled when
     *                          gauge edge is past this point, so the bridge
     *                          transition tile has a clean left neighbor)
     *   bridgeBreakBoundary[j] = fillIndex of the bridge cell that j protects */
    for (u8 fillIndex = 0; (u16)(fillIndex + 1) < layout->length; fillIndex++)
    {
        const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
        const u8 nextCellIndex = layout->cellIndexByFillIndex[fillIndex + 1];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u8 nextSegId = layout->segmentIdByCell[nextCellIndex];

        /* Same segment: no boundary here */
        if (segmentId == nextSegId)
            continue;

        /* Segment boundary detected between fillIndex and fillIndex+1 */
        if (layout->tilesetBridgeBySegment[segmentId] || layout->gainTilesetBridgeBySegment[segmentId])
        {
            /* Bridge tileset exists: mark this cell as bridge end */
            layout->bridgeEndByFillIndex[fillIndex] = 1;
            if (fillIndex > 0)
            {
                /* Mark the cell before the bridge as BREAK (forced full) */
                const u8 prevCellIndex = layout->cellIndexByFillIndex[fillIndex - 1];
                if (layout->segmentIdByCell[prevCellIndex] == segmentId)
                {
                    layout->bridgeBreakByFillIndex[fillIndex - 1] = 1;
                    layout->bridgeBreakBoundaryByFillIndex[fillIndex - 1] = fillIndex;
                }
            }
        }
        else
        {
            /* No bridge tileset: treat boundary cell itself as BREAK */
            layout->bridgeBreakByFillIndex[fillIndex] = 1;
            layout->bridgeBreakBoundaryByFillIndex[fillIndex] = fillIndex;
        }
    }
}

/**
 * Build PIP lookup tables.
 *
 * PIP boundaries are defined by contiguous runs of segment IDs in fill order.
 * For a given segment style, one logical pip spans pipWidthBySegment[segmentId] cells.
 * A run can therefore contain multiple pips with the same style.
 *
 * Also builds a physical render LUT for multi-height PIP:
 * renderIndex -> (fillIndex,row), where row is [0..segmentHeight-1].
 */
static inline void compute_pip_source_mapping(u8 stripCoverage,
                                              u8 halfAxis,
                                              u8 pipWidth,
                                              u8 pipHeight,
                                              u8 sourceWidth,
                                              u8 sourceHeight,
                                              u8 localCol,
                                              u8 localRow,
                                              u8 *outSourceCol,
                                              u8 *outSourceRow,
                                              u8 *outExtraHFlip,
                                              u8 *outExtraVFlip)
{
    u8 sourceCol = localCol;
    u8 sourceRow = localRow;
    u8 extraHFlip = 0;
    u8 extraVFlip = 0;

    if (pipWidth == 0)
        pipWidth = 1;
    if (pipHeight == 0)
        pipHeight = 1;
    if (sourceWidth == 0)
        sourceWidth = 1;
    if (sourceHeight == 0)
        sourceHeight = 1;
    if (localCol >= pipWidth)
        localCol = 0;
    if (localRow >= pipHeight)
        localRow = 0;

    if (stripCoverage == GAUGE_STRIP_COVERAGE_HALF)
    {
        if (halfAxis == PIP_HALF_AXIS_VERTICAL)
        {
            /* Right side mirrored from left side via HFLIP. */
            if (localCol < sourceWidth)
            {
                sourceCol = localCol;
            }
            else
            {
                sourceCol = (u8)(pipWidth - 1 - localCol);
                extraHFlip = 1;
            }
            sourceRow = localRow;
        }
        else
        {
            /* Bottom side mirrored from top side via VFLIP. */
            sourceCol = localCol;
            if (localRow < sourceHeight)
            {
                sourceRow = localRow;
            }
            else
            {
                sourceRow = (u8)(pipHeight - 1 - localRow);
                extraVFlip = 1;
            }
        }
    }
    else if (stripCoverage == GAUGE_STRIP_COVERAGE_QUARTER)
    {
        /* Reconstruct right half with HFLIP and bottom half with VFLIP. */
        if (localCol < sourceWidth)
        {
            sourceCol = localCol;
        }
        else
        {
            sourceCol = (u8)(pipWidth - 1 - localCol);
            extraHFlip = 1;
        }

        if (localRow < sourceHeight)
        {
            sourceRow = localRow;
        }
        else
        {
            sourceRow = (u8)(pipHeight - 1 - localRow);
            extraVFlip = 1;
        }
    }
    else
    {
        /* FULL coverage: direct lookup. */
        sourceCol = localCol;
        sourceRow = localRow;
    }

    if (sourceCol >= sourceWidth)
        sourceCol = 0;
    if (sourceRow >= sourceHeight)
        sourceRow = 0;

    *outSourceCol = sourceCol;
    *outSourceRow = sourceRow;
    *outExtraHFlip = extraHFlip;
    *outExtraVFlip = extraVFlip;
}

static void build_pip_luts(GaugeLaneLayout *layout)
{
    const u8 hasPipStyles =
        segment_tileset_array_has_any(layout->pipTilesetBySegment, layout->segmentCount);
    u16 computedRenderCount = (u16)layout->length * 4u;
    if (computedRenderCount > GAUGE_PIP_MAX_RENDER_TILES)
        computedRenderCount = GAUGE_PIP_MAX_RENDER_TILES;
    const u8 maxRenderCount = (u8)computedRenderCount;

    if (!hasPipStyles)
    {
        layout_reset_pip_runtime_views(layout);
        return;
    }

    if (!layout_sync_pip_luts(layout, maxRenderCount))
    {
        layout_reset_pip_runtime_views(layout);
        return;
    }

    layout->pipCount = 0;
    layout->pipRenderCount = 0;

    memset(layout->pipIndexByFillIndex, CACHE_INVALID_U8, layout->length);
    memset(layout->pipLocalTileByFillIndex, 0, layout->length);
    memset(layout->pipWidthByPipIndex, 1, layout->length);
    memset(layout->pipRenderFillIndexByRenderIndex, CACHE_INVALID_U8, maxRenderCount);
    memset(layout->pipRenderRowByRenderIndex, 0, maxRenderCount);
    memset(layout->pipRenderExtraHFlipByRenderIndex, 0, maxRenderCount);
    memset(layout->pipRenderExtraVFlipByRenderIndex, 0, maxRenderCount);
    memset(layout->pipRenderStripIndexByState, 0, GAUGE_PIP_RENDER_STATE_LUT_SIZE);

    u8 fillIndex = 0;
    while (fillIndex < layout->length && layout->pipCount < layout->length)
    {
        const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        u8 styleWidth = layout->pipWidthBySegment[segmentId];
        if (styleWidth == 0)
            styleWidth = 1;

        /* Measure contiguous run for this style in fill order. */
        u8 runLength = 0;
        while ((u16)(fillIndex + runLength) < layout->length)
        {
            const u8 runCellIndex = layout->cellIndexByFillIndex[fillIndex + runLength];
            if (layout->segmentIdByCell[runCellIndex] != segmentId)
                break;
            runLength++;
        }
        if (runLength == 0)
            runLength = 1;

        /* Split the run into logical pips of styleWidth cells. */
        u8 remaining = runLength;
        while (remaining > 0 && layout->pipCount < layout->length)
        {
            const u8 pipWidth = (remaining >= styleWidth) ? styleWidth : remaining;
            layout->pipWidthByPipIndex[layout->pipCount] = pipWidth;

            for (u8 local = 0; local < pipWidth; local++)
            {
                layout->pipIndexByFillIndex[fillIndex + local] = layout->pipCount;
                layout->pipLocalTileByFillIndex[fillIndex + local] = local;
            }

            fillIndex = (u8)(fillIndex + pipWidth);
            remaining = (u8)(remaining - pipWidth);
            layout->pipCount++;
        }
    }

    /* Build physical render LUT: each fillIndex is duplicated by segment height.
     * At the same time, precompute the state-major strip index LUT used by the
     * fixed PIP hot path.
     */
    for (u8 i = 0; i < layout->length && layout->pipRenderCount < maxRenderCount; i++)
    {
        const u8 fillCellIndex = layout->cellIndexByFillIndex[i];
        const u8 segmentId = layout->segmentIdByCell[fillCellIndex];
        const u8 pipIndex = layout->pipIndexByFillIndex[i];
        u8 pipWidth = 1;
        if (pipIndex != CACHE_INVALID_U8)
            pipWidth = layout->pipWidthByPipIndex[pipIndex];
        if (pipWidth == 0)
            pipWidth = 1;

        u8 pipHeight = layout->pipHeightBySegment[segmentId];
        if (pipHeight == 0)
            pipHeight = 1;
        if (pipHeight > 4)
            pipHeight = 4;

        u8 sourceWidth = layout->pipSourceWidthBySegment[segmentId];
        if (sourceWidth == 0)
            sourceWidth = pipWidth;
        u8 sourceHeight = layout->pipSourceHeightBySegment[segmentId];
        if (sourceHeight == 0)
            sourceHeight = pipHeight;

        const u8 stripCoverage = layout->pipStripCoverageBySegment[segmentId];
        const u8 halfAxis = layout->pipHalfAxisBySegment[segmentId];
        u8 localTile = layout->pipLocalTileByFillIndex[i];
        if (localTile >= pipWidth)
            localTile = 0;

        for (u8 row = 0; row < pipHeight && layout->pipRenderCount < maxRenderCount; row++)
        {
            u8 sourceCol = localTile;
            u8 sourceRow = row;
            u8 extraHFlip = 0;
            u8 extraVFlip = 0;
            compute_pip_source_mapping(stripCoverage,
                                       halfAxis,
                                       pipWidth,
                                       pipHeight,
                                       sourceWidth,
                                       sourceHeight,
                                       localTile,
                                       row,
                                       &sourceCol,
                                       &sourceRow,
                                       &extraHFlip,
                                       &extraVFlip);

            const u8 renderIndex = layout->pipRenderCount;
            const u8 stripStateStride = sourceWidth;
            const u8 stripBaseIndex = (u8)((u16)sourceRow * (u16)(sourceWidth * layout->pipStateCountBySegment[segmentId]) + sourceCol);
            layout->pipRenderFillIndexByRenderIndex[renderIndex] = i;
            layout->pipRenderRowByRenderIndex[renderIndex] = row;
            layout->pipRenderExtraHFlipByRenderIndex[renderIndex] = extraHFlip;
            layout->pipRenderExtraVFlipByRenderIndex[renderIndex] = extraVFlip;

            for (u8 pipState = PIP_STATE_EMPTY; pipState < GAUGE_PIP_RUNTIME_STATE_COUNT; pipState++)
            {
                const u16 renderStateIndex = compute_pip_render_state_lut_index(pipState, renderIndex);
                layout->pipRenderStripIndexByState[renderStateIndex] =
                    (u8)(stripBaseIndex + ((u16)pipState * stripStateStride));
            }
            layout->pipRenderCount++;
        }
    }

}


/* =============================================================================
   GaugeLaneLayout implementation
   ============================================================================= */
static void layout_free_optional_ptr(void **ptr,
                                     const void *defaultViewA,
                                     const void *defaultViewB,
                                     const void *defaultViewC)
{
    /* Free only real heap-backed buffers. Shared sentinel views are never freed. */
    if (!ptr || !*ptr)
        return;

    if (*ptr != defaultViewA &&
        *ptr != defaultViewB &&
        *ptr != defaultViewC)
    {
        MEM_free(*ptr);
    }
    *ptr = NULL;
}

static u8 layout_ensure_segment_tileset_storage(const u32 ***segmentTilesetsBySegment,
                                                u8 segmentCount)
{
    if (!segmentTilesetsBySegment)
        return 0;

    if (*segmentTilesetsBySegment != NULL &&
        *segmentTilesetsBySegment != s_nullSegmentTilesets)
    {
        return 1;
    }

    const u32 **allocatedTilesets = (const u32 **)gauge_alloc_bytes(
        (u16)(segmentCount * (u8)sizeof(const u32 *)));
    if (!allocatedTilesets)
    {
        *segmentTilesetsBySegment = (const u32 **)s_nullSegmentTilesets;
        return 0;
    }

    *segmentTilesetsBySegment = allocatedTilesets;
    return 1;
}

static u8 layout_ensure_optional_values(u8 **values,
                                        u16 valueCount,
                                        const u8 *defaultValues,
                                        u8 defaultValue)
{
    if (!values)
        return 0;

    if (*values != NULL && *values != defaultValues)
        return 1;

    u8 *allocatedValues = (u8 *)gauge_alloc_bytes(valueCount);
    if (!allocatedValues)
    {
        *values = (u8 *)defaultValues;
        return 0;
    }

    *values = allocatedValues;
    memset(*values, defaultValue, valueCount);
    return 1;
}

static u8 layout_sync_optional_segment_tilesets_by_usage(u8 hasAny,
                                                          const u32 ***segmentTilesetsBySegment,
                                                          u8 segmentCount)
{
    if (!segmentTilesetsBySegment)
        return 0;

    if (!hasAny)
    {
        layout_free_optional_ptr((void **)segmentTilesetsBySegment, s_nullSegmentTilesets, NULL, NULL);
        *segmentTilesetsBySegment = (const u32 **)s_nullSegmentTilesets;
        return 1;
    }

    if (!layout_ensure_segment_tileset_storage(segmentTilesetsBySegment, segmentCount))
    {
        *segmentTilesetsBySegment = (const u32 **)s_nullSegmentTilesets;
        return 0;
    }

    return 1;
}

static u8 layout_sync_optional_values_by_usage(u8 hasAny,
                                               u8 **values,
                                               u16 valueCount,
                                               const u8 *defaultValues,
                                               u8 defaultValue)
{
    if (!values)
        return 0;

    if (!hasAny)
    {
        layout_free_optional_ptr((void **)values, defaultValues, NULL, NULL);
        *values = (u8 *)defaultValues;
        return 1;
    }

    if (!layout_ensure_optional_values(values, valueCount, defaultValues, defaultValue))
    {
        *values = (u8 *)defaultValues;
        return 0;
    }

    return 1;
}

static void layout_copy_segment_tilesets(const u32 **destinationTilesets,
                                         const u32 * const *sourceTilesets,
                                         u8 segmentCount)
{
    if (!destinationTilesets || destinationTilesets == s_nullSegmentTilesets)
        return;

    for (u8 segmentId = 0; segmentId < segmentCount; segmentId++)
    {
        destinationTilesets[segmentId] = sourceTilesets ? sourceTilesets[segmentId] : NULL;
    }
}

typedef enum
{
    LAYOUT_TILESET_SLOT_BODY = 0,
    LAYOUT_TILESET_SLOT_END,
    LAYOUT_TILESET_SLOT_TRAIL,
    LAYOUT_TILESET_SLOT_BRIDGE,
    LAYOUT_TILESET_SLOT_CAP_START,
    LAYOUT_TILESET_SLOT_CAP_END,
    LAYOUT_TILESET_SLOT_CAP_START_BREAK,
    LAYOUT_TILESET_SLOT_CAP_START_TRAIL,
    LAYOUT_TILESET_SLOT_COUNT
} LayoutTilesetSlot;

typedef enum
{
    LAYOUT_TILESET_GROUP_BASE = 0,
    LAYOUT_TILESET_GROUP_GAIN,
    LAYOUT_TILESET_GROUP_BLINK,
    LAYOUT_TILESET_GROUP_GAIN_BLINK,
    LAYOUT_TILESET_GROUP_COUNT
} LayoutTilesetGroupId;

typedef u16 LayoutTilesetSlotMask;

#define LAYOUT_TILESET_SLOT_MASK(slot) ((LayoutTilesetSlotMask)(1u << (slot)))
#define LAYOUT_TILESET_SLOT_MASK_ALL ((LayoutTilesetSlotMask)((1u << LAYOUT_TILESET_SLOT_COUNT) - 1u))
#define GAUGE_MEMBER_OFFSET(type, memberName) ((u16)((u32)&(((type *)0)->memberName)))

typedef struct
{
    u16 memberOffset;
    const void *defaultView;
    u8 isOptional;
} LayoutBinding;

#define LAYOUT_BINDING_REQUIRED(memberName) { GAUGE_MEMBER_OFFSET(GaugeLaneLayout, memberName), NULL, 0 }
#define LAYOUT_BINDING_OPTIONAL(memberName, sentinelView) { GAUGE_MEMBER_OFFSET(GaugeLaneLayout, memberName), (sentinelView), 1 }

/* Table-driven mapping:
 *   group + slot -> GaugeLaneLayout member + default sentinel view.
 * This removes the old 4-way switch duplicated across defaults/free/build code.
 */
static const LayoutBinding s_layoutBindings[LAYOUT_TILESET_GROUP_COUNT][LAYOUT_TILESET_SLOT_COUNT] = {
    [LAYOUT_TILESET_GROUP_BASE] = {
        LAYOUT_BINDING_REQUIRED(tilesetBySegment),
        LAYOUT_BINDING_OPTIONAL(tilesetEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(tilesetTrailBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(tilesetBridgeBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(tilesetCapStartBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(tilesetCapEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(tilesetCapStartBreakBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(tilesetCapStartTrailBySegment, s_nullSegmentTilesets)
    },
    [LAYOUT_TILESET_GROUP_GAIN] = {
        LAYOUT_BINDING_OPTIONAL(gainTilesetBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainTilesetEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainTilesetTrailBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainTilesetBridgeBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainTilesetCapStartBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainTilesetCapEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainTilesetCapStartBreakBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainTilesetCapStartTrailBySegment, s_nullSegmentTilesets)
    },
    [LAYOUT_TILESET_GROUP_BLINK] = {
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetTrailBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetBridgeBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetCapStartBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetCapEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetCapStartBreakBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(blinkOffTilesetCapStartTrailBySegment, s_nullSegmentTilesets)
    },
    [LAYOUT_TILESET_GROUP_GAIN_BLINK] = {
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetTrailBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetBridgeBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetCapStartBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetCapEndBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetCapStartBreakBySegment, s_nullSegmentTilesets),
        LAYOUT_BINDING_OPTIONAL(gainBlinkOffTilesetCapStartTrailBySegment, s_nullSegmentTilesets)
    }
};

typedef struct
{
    u16 memberOffset;
    const void *defaultView;
} OptionalPointerBinding;

#define OPTIONAL_POINTER_BINDING(memberName, sentinelView) \
    { GAUGE_MEMBER_OFFSET(GaugeLaneLayout, memberName), (sentinelView) }

static const OptionalPointerBinding s_layoutOptionalPointerBindings[] = {
    OPTIONAL_POINTER_BINDING(capEndBySegment, s_zeroSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipTilesetBySegment, s_nullSegmentTilesets),
    OPTIONAL_POINTER_BINDING(pipWidthBySegment, s_oneSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipHeightBySegment, s_oneSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipOffsetBySegment, s_zeroSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipStateCountBySegment, s_zeroSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipStripCoverageBySegment, s_zeroSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipHalfAxisBySegment, s_zeroSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipSourceWidthBySegment, s_oneSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipSourceHeightBySegment, s_oneSegmentFlags),
    OPTIONAL_POINTER_BINDING(pipIndexByFillIndex, s_invalidCellIndexes),
    OPTIONAL_POINTER_BINDING(pipLocalTileByFillIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipWidthByPipIndex, s_oneCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderFillIndexByRenderIndex, s_invalidCellIndexes),
    OPTIONAL_POINTER_BINDING(pipRenderRowByRenderIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderExtraHFlipByRenderIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderExtraVFlipByRenderIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderStripIndexByState, s_zeroPipRenderStateLut),
    OPTIONAL_POINTER_BINDING(bridgeEndByFillIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(bridgeBreakByFillIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(bridgeBreakBoundaryByFillIndex, s_zeroCellFlags)
};

static const OptionalPointerBinding s_layoutPipRuntimeBindings[] = {
    OPTIONAL_POINTER_BINDING(pipIndexByFillIndex, s_invalidCellIndexes),
    OPTIONAL_POINTER_BINDING(pipLocalTileByFillIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipWidthByPipIndex, s_oneCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderFillIndexByRenderIndex, s_invalidCellIndexes),
    OPTIONAL_POINTER_BINDING(pipRenderRowByRenderIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderExtraHFlipByRenderIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderExtraVFlipByRenderIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(pipRenderStripIndexByState, s_zeroPipRenderStateLut)
};

static const OptionalPointerBinding s_layoutBridgeBindings[] = {
    OPTIONAL_POINTER_BINDING(bridgeEndByFillIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(bridgeBreakByFillIndex, s_zeroCellFlags),
    OPTIONAL_POINTER_BINDING(bridgeBreakBoundaryByFillIndex, s_zeroCellFlags)
};

static inline void **layout_get_optional_pointer_target(GaugeLaneLayout *layout,
                                                        const OptionalPointerBinding *binding)
{
    if (!layout || !binding)
        return NULL;

    return (void **)((u8 *)layout + binding->memberOffset);
}

static void layout_set_optional_pointer_defaults(GaugeLaneLayout *layout,
                                                 const OptionalPointerBinding *bindings,
                                                 u16 bindingCount)
{
    if (!layout || !bindings)
        return;

    for (u16 bindingIndex = 0; bindingIndex < bindingCount; bindingIndex++)
    {
        void **target = layout_get_optional_pointer_target(layout, &bindings[bindingIndex]);
        if (target)
            *target = (void *)bindings[bindingIndex].defaultView;
    }
}

static void layout_free_optional_pointer_bindings(GaugeLaneLayout *layout,
                                                  const OptionalPointerBinding *bindings,
                                                  u16 bindingCount)
{
    if (!layout || !bindings)
        return;

    for (u16 bindingIndex = 0; bindingIndex < bindingCount; bindingIndex++)
    {
        void **target = layout_get_optional_pointer_target(layout, &bindings[bindingIndex]);
        if (!target)
            continue;

        layout_free_optional_ptr(target, bindings[bindingIndex].defaultView, NULL, NULL);
    }
}

static void layout_reset_optional_pointer_group(GaugeLaneLayout *layout,
                                                const OptionalPointerBinding *bindings,
                                                u16 bindingCount)
{
    layout_free_optional_pointer_bindings(layout, bindings, bindingCount);
    layout_set_optional_pointer_defaults(layout, bindings, bindingCount);
}

static void layout_reset_bridge_views(GaugeLaneLayout *layout)
{
    layout_reset_optional_pointer_group(layout,
                                        s_layoutBridgeBindings,
                                        GAUGE_ARRAY_LEN(s_layoutBridgeBindings));
}

static void layout_reset_pip_runtime_views(GaugeLaneLayout *layout)
{
    layout_reset_optional_pointer_group(layout,
                                        s_layoutPipRuntimeBindings,
                                        GAUGE_ARRAY_LEN(s_layoutPipRuntimeBindings));
    layout->pipCount = 0;
    layout->pipRenderCount = 0;
}

static inline u8 layout_sync_bridge_luts(GaugeLaneLayout *layout)
{
    return layout_sync_optional_cell_flags(1,
                                           &layout->bridgeEndByFillIndex,
                                           layout->length,
                                           s_zeroCellFlags,
                                           0) &&
           layout_sync_optional_cell_flags(1,
                                           &layout->bridgeBreakByFillIndex,
                                           layout->length,
                                           s_zeroCellFlags,
                                           0) &&
           layout_sync_optional_cell_flags(1,
                                           &layout->bridgeBreakBoundaryByFillIndex,
                                           layout->length,
                                           s_zeroCellFlags,
                                           0);
}

static inline u8 layout_sync_pip_luts(GaugeLaneLayout *layout, u16 maxRenderCount)
{
    return layout_sync_optional_cell_flags(1,
                                           &layout->pipIndexByFillIndex,
                                           layout->length,
                                           s_invalidCellIndexes,
                                           CACHE_INVALID_U8) &&
           layout_sync_optional_cell_flags(1,
                                           &layout->pipLocalTileByFillIndex,
                                           layout->length,
                                           s_zeroCellFlags,
                                           0) &&
           layout_sync_optional_cell_flags(1,
                                           &layout->pipWidthByPipIndex,
                                           layout->length,
                                           s_oneCellFlags,
                                           1) &&
           layout_sync_optional_pip_lut(1,
                                        &layout->pipRenderFillIndexByRenderIndex,
                                        maxRenderCount,
                                        s_invalidCellIndexes,
                                        CACHE_INVALID_U8) &&
           layout_sync_optional_pip_lut(1,
                                        &layout->pipRenderRowByRenderIndex,
                                        maxRenderCount,
                                        s_zeroCellFlags,
                                        0) &&
           layout_sync_optional_pip_lut(1,
                                        &layout->pipRenderExtraHFlipByRenderIndex,
                                        maxRenderCount,
                                        s_zeroCellFlags,
                                        0) &&
           layout_sync_optional_pip_lut(1,
                                        &layout->pipRenderExtraVFlipByRenderIndex,
                                        maxRenderCount,
                                        s_zeroCellFlags,
                                        0) &&
           layout_sync_optional_pip_lut(1,
                                        &layout->pipRenderStripIndexByState,
                                        GAUGE_PIP_RENDER_STATE_LUT_SIZE,
                                        s_zeroPipRenderStateLut,
                                        0);
}

static inline const LayoutBinding *layout_get_binding(LayoutTilesetGroupId group,
                                                      LayoutTilesetSlot slot)
{
    if (group >= LAYOUT_TILESET_GROUP_COUNT || slot >= LAYOUT_TILESET_SLOT_COUNT)
        return NULL;

    return &s_layoutBindings[group][slot];
}

static inline const u32 ***layout_get_group_slot_target(GaugeLaneLayout *layout,
                                                        LayoutTilesetGroupId group,
                                                        LayoutTilesetSlot slot)
{
    const LayoutBinding *binding = layout_get_binding(group, slot);
    if (!layout || !binding)
        return NULL;

    return (const u32 ***)((u8 *)layout + binding->memberOffset);
}

static void layout_zero_optional_flags(GaugeLaneLayout *layout)
{
    layout->hasBlinkOff = 0;
    layout->hasGainBlinkOff = 0;
    layout->capStartEnabled = 0;
    layout->capEndEnabled = 0;
}

static void layout_reset_core_views(GaugeLaneLayout *layout, u8 length, u8 segmentCount)
{
    layout->length = length;
    layout->segmentCount = segmentCount;
    layout->pipCount = 0;
    layout->pipRenderCount = 0;
    layout->segmentIdByCell = layout->segmentIdByCellStorage;
    layout->fillIndexByCell = layout->fillIndexByCellStorage;
    layout->cellIndexByFillIndex = layout->cellIndexByFillIndexStorage;
    layout->tilemapPosByCell = layout->tilemapPosByCellStorage;
    layout->tilesetBySegment = layout->tilesetBySegmentStorage;
}

static void layout_set_optional_views_to_defaults(GaugeLaneLayout *layout)
{
    /* Optional fields point to shared sentinel views until explicitly enabled.
     * This keeps zero-feature layouts RAM-light while preserving branch-free reads. */
    for (u8 group = 0; group < LAYOUT_TILESET_GROUP_COUNT; group++)
    {
        for (u8 slot = 0; slot < LAYOUT_TILESET_SLOT_COUNT; slot++)
        {
            const LayoutBinding *binding = layout_get_binding((LayoutTilesetGroupId)group,
                                                              (LayoutTilesetSlot)slot);
            if (!binding || !binding->isOptional)
                continue;

            const u32 ***target = layout_get_group_slot_target(layout,
                                                               (LayoutTilesetGroupId)group,
                                                               (LayoutTilesetSlot)slot);
            if (!target)
                continue;

            *target = (const u32 **)binding->defaultView;
        }
    }
    layout_set_optional_pointer_defaults(layout,
                                         s_layoutOptionalPointerBindings,
                                         GAUGE_ARRAY_LEN(s_layoutOptionalPointerBindings));
    layout->pipRenderCount = 0;
}

static void layout_free_buffers(GaugeLaneLayout *layout)
{
    if (!layout)
        return;

    for (u8 group = 0; group < LAYOUT_TILESET_GROUP_COUNT; group++)
    {
        for (u8 slot = 0; slot < LAYOUT_TILESET_SLOT_COUNT; slot++)
        {
            const LayoutBinding *binding = layout_get_binding((LayoutTilesetGroupId)group,
                                                              (LayoutTilesetSlot)slot);
            if (!binding || !binding->isOptional)
                continue;

            void **target = (void **)layout_get_group_slot_target(layout,
                                                                  (LayoutTilesetGroupId)group,
                                                                  (LayoutTilesetSlot)slot);
            if (!target)
                continue;

            layout_free_optional_ptr(target, binding->defaultView, NULL, NULL);
        }
    }

    layout_free_optional_pointer_bindings(layout,
                                          s_layoutOptionalPointerBindings,
                                          GAUGE_ARRAY_LEN(s_layoutOptionalPointerBindings));

    layout_reset_core_views(layout, 0, 0);
    layout_set_optional_views_to_defaults(layout);
    layout_zero_optional_flags(layout);
}

static u8 layout_alloc_buffers(GaugeLaneLayout *layout, u8 length, u8 segmentCount)
{
    if (!layout || length > GAUGE_MAX_LENGTH || segmentCount > GAUGE_MAX_SEGMENTS)
        return 0;

    layout_reset_core_views(layout, length, segmentCount);
    memset(layout->segmentIdByCellStorage, 0, sizeof(layout->segmentIdByCellStorage));
    memset(layout->fillIndexByCellStorage, 0, sizeof(layout->fillIndexByCellStorage));
    memset(layout->cellIndexByFillIndexStorage, 0, sizeof(layout->cellIndexByFillIndexStorage));
    memset(layout->tilemapPosByCellStorage, 0, sizeof(layout->tilemapPosByCellStorage));
    memset(layout->tilesetBySegmentStorage, 0, sizeof(layout->tilesetBySegmentStorage));
    layout_set_optional_views_to_defaults(layout);

    return 1;
}

static void GaugeLaneLayout_initEx(GaugeLaneLayout *layout,
                                   u8 length,
                                   GaugeFillDirection fillDirection,
                                   const u32 * const *bodyTilesets,
                                   const u32 * const *endTilesets,
                                   const u32 * const *trailTilesets,
                                   const u32 * const *bridgeTilesets,
                                   const u8 *segmentIdByCell,
                                   GaugeOrientation orientation,
                                   u8 palette,
                                   u8 priority,
                                   u8 verticalFlip,
                                   u8 horizontalFlip)
{
    if (!layout)
        return;

    /* Validate and clamp length */
    if (length == 0) length = 1;
    if (length > GAUGE_MAX_LENGTH) length = GAUGE_MAX_LENGTH;

    /* Segment count = highest segment id present in cell map + 1 */
    u8 maxSegmentId = 0;
    if (segmentIdByCell)
    {
        for (u8 i = 0; i < length; i++)
        {
            if (segmentIdByCell[i] > maxSegmentId)
                maxSegmentId = segmentIdByCell[i];
        }
    }
    if (maxSegmentId >= GAUGE_MAX_SEGMENTS)
        maxSegmentId = (u8)(GAUGE_MAX_SEGMENTS - 1);
    const u8 segmentCount = (u8)(maxSegmentId + 1);

    /* Rebuild layout state from scratch (layout must not be retained while rebuilt). */
    if (layout->refCount != 0)
        return;
    layout_free_buffers(layout);
    if (!layout_alloc_buffers(layout, length, segmentCount))
        return;

    layout->fillOffset = 0;
    layout->endOverrideEnabled = 1;
    layout->refCount = 0;

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(endTilesets, layout->segmentCount),
        &layout->tilesetEndBySegment,
        layout->segmentCount);

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(trailTilesets, layout->segmentCount),
        &layout->tilesetTrailBySegment,
        layout->segmentCount);

    layout_sync_optional_segment_tilesets_by_usage(
        segment_tileset_array_has_any(bridgeTilesets, layout->segmentCount),
        &layout->tilesetBridgeBySegment,
        layout->segmentCount);

    /* Copy mandatory BODY tilesets. */
    for (u8 i = 0; i < layout->segmentCount; i++)
    {
        layout->tilesetBySegment[i] = bodyTilesets ? bodyTilesets[i] : NULL;
    }
    layout_copy_segment_tilesets(layout->tilesetEndBySegment, endTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->tilesetTrailBySegment, trailTilesets, layout->segmentCount);
    layout_copy_segment_tilesets(layout->tilesetBridgeBySegment, bridgeTilesets, layout->segmentCount);

    /* Copy segment IDs per cell (NULL = all segment 0). */
    for (u8 i = 0; i < layout->length; i++)
    {
        u8 segmentId = segmentIdByCell ? segmentIdByCell[i] : 0;
        if (segmentId >= layout->segmentCount)
            segmentId = 0;
        layout->segmentIdByCell[i] = segmentId;
    }

    /* Set fill direction */
    GaugeLaneLayout_applyFillDirection(layout, fillDirection);

    /* Set visual properties */
    layout->orientation = orientation;
    layout->palette = palette;
    layout->priority = priority ? 1 : 0;
    layout->verticalFlip = verticalFlip ? 1 : 0;
    layout->horizontalFlip = horizontalFlip ? 1 : 0;

    /* Direct layout builders refresh these cached flags after optional tilesets are wired. */
    layout_zero_optional_flags(layout);
}

static void GaugeLaneLayout_applyFillDirection(GaugeLaneLayout *layout,
                                               GaugeFillDirection fillDirection)
{
    if (!layout)
        return;

    for (u8 c = 0; c < layout->length; c++)
    {
        const u8 fillIndex = (fillDirection == GAUGE_FILL_REVERSE)
            ? (u8)(layout->length - 1 - c)
            : c;
        layout->fillIndexByCell[c] = fillIndex;
        layout->cellIndexByFillIndex[fillIndex] = c;
    }

    build_bridge_luts(layout);
    build_pip_luts(layout);
}

/** Set pixel offset for fill computation (used for multi-lane gauge windows). */
static void GaugeLaneLayout_setFillOffset(GaugeLaneLayout *layout, u16 fillOffsetPixels)
{
    if (!layout)
        return;

    layout->fillOffset = fillOffsetPixels;
}

static void GaugeLaneLayout_retain(GaugeLaneLayout *layout)
{
    if (!layout)
        return;
    layout->refCount++;
}

static void GaugeLaneLayout_release(GaugeLaneLayout *layout)
{
    if (!layout)
        return;

    if (layout->refCount > 0)
        layout->refCount--;

    if (layout->refCount == 0)
        layout_free_buffers(layout);
}

/* =============================================================================
   Direct layout build helpers
   ============================================================================= */

/**
 * Usage flags for one tileset group (base/gain/blinkOff/gainBlinkOff).
 * Each flag is 1 if any segment provides that tileset, 0 otherwise.
 */
typedef struct
{
    u8 body;
    u8 end;
    u8 trail;
    u8 bridge;
    u8 capStart;
    u8 capEnd;
    u8 capStartBreak;
    u8 capStartTrail;
} SkinSetUsageFlags;

static inline u8 usage_flag_for_slot(const SkinSetUsageFlags *usageFlags,
                                     LayoutTilesetSlot slot)
{
    return (!usageFlags || slot >= LAYOUT_TILESET_SLOT_COUNT)
        ? 0
        : ((const u8 *)usageFlags)[slot];
}

static void sync_layout_tileset_group_slots_from_usage(GaugeLaneLayout *layout,
                                                       LayoutTilesetGroupId group,
                                                       const SkinSetUsageFlags *usageFlags,
                                                       LayoutTilesetSlotMask slotMask)
{
    if (!layout || !usageFlags)
        return;

    for (u8 slot = 0; slot < LAYOUT_TILESET_SLOT_COUNT; slot++)
    {
        const LayoutTilesetSlot slotId = (LayoutTilesetSlot)slot;
        if ((slotMask & LAYOUT_TILESET_SLOT_MASK(slotId)) == 0)
            continue;

        if (group == LAYOUT_TILESET_GROUP_BASE && slotId == LAYOUT_TILESET_SLOT_BODY)
            continue;

        const u32 ***targetField = layout_get_group_slot_target(layout, group, slotId);
        if (!targetField)
            continue;

        layout_sync_optional_segment_tilesets_by_usage(
            usage_flag_for_slot(usageFlags, slotId),
            targetField,
            layout->segmentCount);
    }
}

/**
 * Scan helper: OR-combine a SkinSet's non-NULL pointers into usage flags.
 */
/**
 * Sync base tileset allocations (end/trail/bridge/caps + capEnd flags).
 *
 * base.body is always required and handled by GaugeLaneLayout_initEx, so
 * only the 7 optional tilesets + capEnd flag array are synced here.
 */
static void sync_base_allocations(GaugeLaneLayout *layout,
                                  const SkinSetUsageFlags *f,
                                  u8 hasCapEndFlags)
{
    const u8 sc = layout->segmentCount;

    sync_layout_tileset_group_slots_from_usage(layout,
                                               LAYOUT_TILESET_GROUP_BASE,
                                               f,
                                               LAYOUT_TILESET_SLOT_MASK_ALL);

    layout_sync_optional_segment_flags(hasCapEndFlags,
        &layout->capEndBySegment, sc, s_zeroSegmentFlags, 0);
}

/**
 * Populate base tilesets from segment styles into layout arrays.
 *
 * body is always assigned. Optional arrays are only written when allocated
 * (checked via != s_nullSegmentTilesets / s_zeroSegmentFlags sentinel).
 */
/**
 * Compute all derived layout state after tilesets are populated.
 *
 * Caches blink-off presence flags, detects cap configuration,
 * builds bridge LUTs and pip LUTs.
 */
static void finalize_layout_derived_state(GaugeLaneLayout *layout)
{
    layout->hasBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_STATE_DAMAGE);
    layout->hasGainBlinkOff = layout_has_blink_off_mode(layout, GAUGE_TRAIL_STATE_GAIN);
    detect_caps_enabled(layout, &layout->capStartEnabled, &layout->capEndEnabled);
    build_bridge_luts(layout);
    build_pip_luts(layout);
}


/* =============================================================================
   GaugeLogic implementation (internal -- not exposed in gauge.h)
   ============================================================================= */

/* Forward declaration: GaugeLogic_init calls GaugeLogic_initWithAnim */
static void GaugeLogic_initWithAnim(GaugeLogic *logic,
                                    u16 maxValue,
                                    u16 maxFillPixels,
                                    const u16 *valueToPixelsLUT,
                                    u16 initialValue,
                                    u8 valueAnimEnabled,
                                    u8 valueAnimShift,
                                    u8 trailAnimShift,
                                    u8 blinkShift);
static void GaugeLogic_tick_follow_mode(GaugeLogic *logic);
static void GaugeLogic_tick_non_follow_mode(GaugeLogic *logic);
static GaugeLogicTickHandler *resolve_logic_tick_handler(GaugeTrailMode mode);

static void GaugeLogic_init(GaugeLogic *logic,
                     u16 maxValue,
                     u16 maxFillPixels,
                     const u16 *valueToPixelsLUT,
                     u16 initialValue)
{
    GaugeLogic_initWithAnim(logic, maxValue, maxFillPixels, valueToPixelsLUT,
                            initialValue,
                            0,  /* valueAnimEnabled = instant */
                            GAUGE_DEFAULT_VALUE_ANIM_SHIFT,
                            GAUGE_DEFAULT_TRAIL_ANIM_SHIFT,
                            GAUGE_DEFAULT_BLINK_SHIFT);
}

static void GaugeLogic_initWithAnim(GaugeLogic *logic,
                                    u16 maxValue,
                                    u16 maxFillPixels,
                                    const u16 *valueToPixelsLUT,
                                    u16 initialValue,
                                    u8 valueAnimEnabled,
                             u8 valueAnimShift,
                             u8 trailAnimShift,
                             u8 blinkShift)
{
    /* Core state */
    logic->maxValue = maxValue;
    logic->currentValue = (initialValue > maxValue) ? maxValue : initialValue;

    logic->maxFillPixels = maxFillPixels;
    logic->valueToPixelsLUT = valueToPixelsLUT;
    logic->pixelsToQuantizedPixelsLUT = NULL;

    /* Compute initial pixel values */
    const u16 initialPixels = value_to_pixels(logic, logic->currentValue);
    logic->valueTargetPixels = (initialPixels > maxFillPixels) ? maxFillPixels : initialPixels;
    logic->valuePixels = logic->valueTargetPixels;
    logic->trailPixels = logic->valuePixels;

    /* Timers */
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->blinkTimer = 0;

    /* Animation config */
    logic->valueAnimEnabled = valueAnimEnabled ? 1 : 0;
    logic->valueAnimShift = (valueAnimShift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : valueAnimShift;
    logic->trailAnimShift = (trailAnimShift == 0) ? GAUGE_DEFAULT_TRAIL_ANIM_SHIFT : trailAnimShift;
    logic->blinkShift = (blinkShift == 0) ? GAUGE_DEFAULT_BLINK_SHIFT : blinkShift;
    logic->gainAnimShift = (valueAnimShift == 0) ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT : valueAnimShift;
    logic->gainBlinkShift = (blinkShift == 0) ? GAUGE_DEFAULT_BLINK_SHIFT : blinkShift;

    logic->trailEnabled = 0;
    logic->configuredTrailMode = GAUGE_TRAIL_MODE_DISABLED;
    logic->configuredGainMode = GAUGE_GAIN_MODE_DISABLED;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    logic->lastActiveTrailState = GAUGE_TRAIL_STATE_NONE;
    logic->criticalValue = 0;
    logic->modeUsesCriticalThreshold = 0;
    logic->modeUsesValueBlink = 0;
    logic->modeKeepsStaticTrail = 0;

    /* Initialize render cache to force first render */
    logic->lastValuePixels = CACHE_INVALID_U16;
    logic->lastTrailPixelsRendered = CACHE_INVALID_U16;
    logic->lastBlinkOn = CACHE_INVALID_U8;
    logic->needUpdate = 1;

    /* Disabled by default; Gauge_build() later applies GaugeDefinition.behavior. */
    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
}

/* Trail mode helpers (configuration/runtime). */
static inline u8 is_critical_mode_active(const GaugeLogic *logic)
{
    return (logic->currentValue <= logic->criticalValue) ? 1 : 0;
}

static inline void set_mode_flags(GaugeLogic *logic, GaugeTrailMode mode)
{
    logic->modeUsesCriticalThreshold =
        (mode == GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK ||
         mode == GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK ||
         mode == GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK) ? 1 : 0;
    logic->modeUsesValueBlink =
        (mode == GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK) ? 1 : 0;
    logic->modeKeepsStaticTrail =
        (mode == GAUGE_TRAIL_MODE_STATIC_TRAIL ||
         mode == GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK) ? 1 : 0;
}

static inline void apply_non_follow_trail_mode_state(GaugeLogic *logic, u8 advanceBlinkTimer)
{
    const u8 criticalModeActive =
        logic->modeUsesCriticalThreshold ? is_critical_mode_active(logic) : 0;

    logic->holdFramesRemaining = 0;

    switch ((GaugeTrailMode)logic->configuredTrailMode)
    {
        case GAUGE_TRAIL_MODE_DISABLED:
            logic->trailPixels = logic->valuePixels;
            logic->blinkFramesRemaining = 0;
            logic->blinkTimer = 0;
            logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
            break;

        case GAUGE_TRAIL_MODE_STATIC_TRAIL:
            logic->trailPixels = logic->maxFillPixels;
            logic->blinkFramesRemaining = 0;
            logic->blinkTimer = 0;
            logic->activeTrailState = (logic->trailPixels > logic->valuePixels)
                ? GAUGE_TRAIL_STATE_DAMAGE
                : GAUGE_TRAIL_STATE_NONE;
            break;

        case GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK:
            logic->trailPixels = logic->maxFillPixels;
            logic->activeTrailState = (logic->trailPixels > logic->valuePixels)
                ? GAUGE_TRAIL_STATE_DAMAGE
                : GAUGE_TRAIL_STATE_NONE;
            if (criticalModeActive)
            {
                logic->blinkFramesRemaining = 1; /* continuous blink gate */
                if (advanceBlinkTimer)
                    logic->blinkTimer++;
                else
                    logic->blinkTimer = 0;
            }
            else
            {
                logic->blinkFramesRemaining = 0;
                logic->blinkTimer = 0;
            }
            break;

        case GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK:
            if (criticalModeActive)
            {
                logic->trailPixels = logic->maxFillPixels;
                logic->blinkFramesRemaining = 1; /* continuous blink gate */
                if (advanceBlinkTimer)
                    logic->blinkTimer++;
                else
                    logic->blinkTimer = 0;
                logic->activeTrailState = (logic->trailPixels > logic->valuePixels)
                    ? GAUGE_TRAIL_STATE_DAMAGE
                    : GAUGE_TRAIL_STATE_NONE;
            }
            else
            {
                logic->trailPixels = logic->valuePixels;
                logic->blinkFramesRemaining = 0;
                logic->blinkTimer = 0;
                logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
            }
            break;

        case GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK:
            logic->trailPixels = logic->valuePixels;
            logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
            if (criticalModeActive)
            {
                logic->blinkFramesRemaining = 1; /* continuous blink gate */
                if (advanceBlinkTimer)
                    logic->blinkTimer++;
                else
                    logic->blinkTimer = 0;
            }
            else
            {
                logic->blinkFramesRemaining = 0;
                logic->blinkTimer = 0;
            }
            break;

        case GAUGE_TRAIL_MODE_FOLLOW:
        default:
            break;
    }
}

static inline void tick_value_animation(GaugeLogic *logic, u8 freezeIncreasing)
{
    const u8 valueAnimShift =
        (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN &&
         logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW)
        ? logic->gainAnimShift
        : logic->valueAnimShift;

    if (!logic->valueAnimEnabled || logic->valuePixels == logic->valueTargetPixels)
        return;

    if (logic->valuePixels < logic->valueTargetPixels)
    {
        if (freezeIncreasing)
            return;

        const u16 diff = (u16)(logic->valueTargetPixels - logic->valuePixels);
        const u16 step = CALC_ANIM_STEP(diff, valueAnimShift);
        logic->valuePixels = (u16)(logic->valuePixels + step);

        if (logic->valuePixels > logic->valueTargetPixels)
            logic->valuePixels = logic->valueTargetPixels;
        if (logic->valuePixels > logic->maxFillPixels)
            logic->valuePixels = logic->maxFillPixels;
    }
    else
    {
        const u16 diff = (u16)(logic->valuePixels - logic->valueTargetPixels);
        const u16 step = CALC_ANIM_STEP(diff, valueAnimShift);
        s16 result = (s16)logic->valuePixels - (s16)step;
        logic->valuePixels = (u16)(result & ~(result >> 15));

        if (logic->valuePixels < logic->valueTargetPixels)
            logic->valuePixels = logic->valueTargetPixels;
    }
}

static inline u8 tick_gain_follow_state(GaugeLogic *logic)
{
    if (logic->configuredGainMode != GAUGE_GAIN_MODE_FOLLOW ||
        logic->activeTrailState != GAUGE_TRAIL_STATE_GAIN)
    {
        return 0;
    }

    logic->trailPixels = logic->valueTargetPixels;

    /* Hold phase: trail stays at target. */
    if (logic->holdFramesRemaining > 0)
    {
        logic->holdFramesRemaining--;
        return 1;
    }

    /* Blink phase: trail stays at target. */
    if (logic->blinkFramesRemaining > 0)
    {
        logic->blinkTimer++;
        logic->blinkFramesRemaining--;
        if (logic->blinkFramesRemaining == 0)
            logic->blinkTimer = 0;
        return 1;
    }

    /* Wait until animated value catches the gain trail. */
    if (logic->valuePixels < logic->valueTargetPixels)
        return 1;

    logic->trailPixels = logic->valuePixels;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    return 0;
}

static void GaugeLogic_tick_non_follow_mode(GaugeLogic *logic)
{
    const u8 gainMode = (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN);
    const u8 freezeIncreasing =
        (gainMode && (logic->holdFramesRemaining > 0 || logic->blinkFramesRemaining > 0)) ? 1 : 0;

    tick_value_animation(logic, freezeIncreasing);

    if (tick_gain_follow_state(logic))
        return;

    apply_non_follow_trail_mode_state(logic, 1);
}

static void GaugeLogic_tick_follow_mode(GaugeLogic *logic)
{
    const u8 gainMode = (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN);
    const u8 freezeIncreasing =
        (gainMode && (logic->holdFramesRemaining > 0 || logic->blinkFramesRemaining > 0)) ? 1 : 0;

    tick_value_animation(logic, freezeIncreasing);

    if (gainMode && tick_gain_follow_state(logic))
        return;

    /* Hold phase: trail stays at previous position */
    if (logic->holdFramesRemaining > 0)
    {
        logic->holdFramesRemaining--;
        return;
    }

    /* Blink phase: trail stays at previous position */
    if (logic->blinkFramesRemaining > 0)
    {
        logic->blinkTimer++;
        logic->blinkFramesRemaining--;
        if (logic->blinkFramesRemaining == 0)
            logic->blinkTimer = 0;
        return;
    }

    /* Trail shrink phase: move toward value.
     * Uses saturating subtract to avoid underflow without branch. */
    if (logic->trailPixels > logic->valuePixels)
    {
        const u16 diff = (u16)(logic->trailPixels - logic->valuePixels);
        const u16 step = CALC_ANIM_STEP(diff, logic->trailAnimShift);
        s16 result = (s16)logic->trailPixels - (s16)step;
        logic->trailPixels = (u16)(result & ~(result >> 15));

        if (logic->trailPixels < logic->valuePixels)
            logic->trailPixels = logic->valuePixels;
    }
    else
    {
        logic->trailPixels = logic->valuePixels;
    }

    /* Stop blink as soon as trail has caught up to value */
    if (logic->trailPixels == logic->valuePixels)
    {
        logic->blinkFramesRemaining = 0;
        logic->blinkTimer = 0;
        if (logic->activeTrailState == GAUGE_TRAIL_STATE_DAMAGE)
            logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
    }
}

static GaugeLogicTickHandler *resolve_logic_tick_handler(GaugeTrailMode mode)
{
    if (mode == GAUGE_TRAIL_MODE_FOLLOW)
        return GaugeLogic_tick_follow_mode;
    return GaugeLogic_tick_non_follow_mode;
}

static inline u8 trail_mode_enables_trail(GaugeTrailMode mode)
{
    return (mode != GAUGE_TRAIL_MODE_DISABLED) ? 1 : 0;
}

static inline u8 gain_mode_uses_trail(GaugeGainMode mode)
{
    return (mode == GAUGE_GAIN_MODE_FOLLOW) ? 1 : 0;
}

static inline u8 compute_trail_enabled(GaugeTrailMode trailMode, GaugeGainMode gainMode)
{
    return (trail_mode_enables_trail(trailMode) || gain_mode_uses_trail(gainMode)) ? 1 : 0;
}

static inline void reset_follow_mode_state(GaugeLogic *logic)
{
    logic->trailPixels = logic->valuePixels;
    logic->holdFramesRemaining = 0;
    logic->blinkFramesRemaining = 0;
    logic->blinkTimer = 0;
    logic->activeTrailState = GAUGE_TRAIL_STATE_NONE;
}

static inline void apply_configured_trail_mode_state(GaugeLogic *logic, u8 advanceBlinkTimer)
{
    if ((GaugeTrailMode)logic->configuredTrailMode == GAUGE_TRAIL_MODE_FOLLOW)
    {
        reset_follow_mode_state(logic);
        return;
    }

    apply_non_follow_trail_mode_state(logic, advanceBlinkTimer);
}

static inline void invalidate_render_cache(GaugeLogic *logic)
{
    logic->lastValuePixels = CACHE_INVALID_U16;
    logic->lastTrailPixelsRendered = CACHE_INVALID_U16;
    logic->lastBlinkOn = CACHE_INVALID_U8;
    logic->lastActiveTrailState = CACHE_INVALID_U8;
    logic->needUpdate = 1;
}


/* =============================================================================
   GaugeLaneInstance internals
   ============================================================================= */

static void write_tilemap_pip_init(GaugeLaneInstance *lane)
{
    const GaugeLaneLayout *layout = lane->layout;
    GaugeTilemapSpan tilemapSpan;
    GaugeTileUploadSpan uploadSpan;

    lane->cellCount = 0;
    tilemap_span_reset(&tilemapSpan);
    tile_upload_span_reset(&uploadSpan);

    for (u8 targetRow = 0; targetRow < 4; targetRow++)
    {
        for (u8 renderIndex = 0; renderIndex < layout->pipRenderCount; renderIndex++)
        {
            const u8 fillIndex = layout->pipRenderFillIndexByRenderIndex[renderIndex];
            if (fillIndex == CACHE_INVALID_U8)
                continue;

            const u8 renderRow = layout->pipRenderRowByRenderIndex[renderIndex];
            if (renderRow != targetRow)
                continue;

            const u8 extraHFlip = layout->pipRenderExtraHFlipByRenderIndex[renderIndex];
            const u8 extraVFlip = layout->pipRenderExtraVFlipByRenderIndex[renderIndex];
            const u8 cellIndex = layout->cellIndexByFillIndex[fillIndex];
            const u8 segmentId = layout->segmentIdByCell[cellIndex];
            const u32 *pipStrip = layout->pipTilesetBySegment[segmentId];
            if (!pipStrip)
                continue;

            const u8 pipStateCount = layout->pipStateCountBySegment[segmentId];
            if (pipStateCount < 2)
                continue;

            if (lane->cellCount >= layout->pipRenderCount)
                break;

            const u16 vramTile = (u16)(lane->vramBase + lane->cellCount);
            const u16 attrBase = TILE_ATTR_FULL(layout->palette, layout->priority,
                                                (u8)(layout->verticalFlip ^ extraVFlip),
                                                (u8)(layout->horizontalFlip ^ extraHFlip),
                                                0);

            u16 x = 0;
            u16 y = 0;
            compute_pip_render_xy(layout, lane->originX, lane->originY, fillIndex, renderRow, &x, &y);
            tilemap_span_push(&tilemapSpan, layout->orientation, attrBase, x, y, vramTile);

            const u16 renderStateIndex = compute_pip_render_state_lut_index(PIP_STATE_EMPTY, renderIndex);
            const u8 stripIndex = layout->pipRenderStripIndexByState[renderStateIndex];

            tile_upload_span_push(&uploadSpan, pipStrip, vramTile, stripIndex);

            lane->cells[lane->cellCount].vramTileIndex = vramTile;
            lane->cells[lane->cellCount].cachedStrip = pipStrip;
            lane->cells[lane->cellCount].cachedFillIndex = stripIndex;
            lane->cells[lane->cellCount].cellIndex = cellIndex;
            lane->cells[lane->cellCount].pipFillIndex = fillIndex;
            lane->cells[lane->cellCount].pipRow = renderRow;
            lane->cells[lane->cellCount].pipRenderIndex = renderIndex;
            lane->cellCount++;
        }

        tilemap_span_flush(&tilemapSpan, layout->orientation);
        tile_upload_span_flush(&uploadSpan);
    }
}

/**
 * Initialize tilemap for fixed VRAM mode (one VRAM tile per cell, fill mode).
 *
 * Allocates one VRAM tile per valid cell, pre-caches tileset strip pointers
 * (body/end/trail/bridge) per cell to avoid per-frame segment lookups, and writes
 * the tilemap on the WINDOW plane. Tile data itself is still uploaded lazily on
 * the first runtime update.
 *
 * @param lane  GaugeLaneInstance to initialize (must have layout and vramBase set)
 */
static void write_tilemap_fill_init(GaugeLaneInstance *lane)
{
    const GaugeLaneLayout *layout = lane->layout;
    GaugeTilemapSpan tilemapSpan;

    tilemap_span_reset(&tilemapSpan);
    lane->cellCount = 0;
    const u16 attrBase = TILE_ATTR_FULL(layout->palette, layout->priority,
                                        layout->verticalFlip, layout->horizontalFlip, 0);

    for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
    {
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *bodyStrip = layout->tilesetBySegment[segmentId];
        const u32 *gainBodyStrip = layout->gainTilesetBySegment[segmentId];

        if (!bodyStrip && !gainBodyStrip)
            continue;

        if (lane->cellCount >= layout->length)
            break;

        const u16 vramTile = (u16)(lane->vramBase + lane->cellCount);
        u16 x, y;
        compute_tile_xy(layout->orientation, lane->originX, lane->originY, cellIndex, &x, &y);

        tilemap_span_push(&tilemapSpan,
                          layout->orientation,
                          attrBase,
                          x,
                          y,
                          vramTile);

        lane->cells[lane->cellCount].vramTileIndex = vramTile;
        lane->cells[lane->cellCount].cachedStrip = NULL;
        lane->cells[lane->cellCount].cachedFillIndex = CACHE_INVALID_U8;
        lane->cells[lane->cellCount].cellIndex = cellIndex;
        lane->cells[lane->cellCount].pipFillIndex = CACHE_INVALID_U8;
        lane->cells[lane->cellCount].pipRow = 0;
        lane->cells[lane->cellCount].pipRenderIndex = CACHE_INVALID_U8;
        lane->cellCount++;

    }

    tilemap_span_flush(&tilemapSpan, layout->orientation);
}

static inline GaugeLaneInstance *gauge_get_baseLane_instance(const Gauge *gauge)
{
    if (!gauge || gauge->laneCount == 0 || gauge->baseLaneIndex >= gauge->laneCount)
        return NULL;
    return gauge->lanes[gauge->baseLaneIndex];
}

static void gauge_clear_baseLane_fill_decisions(Gauge *gauge)
{
    memset(gauge->baseLaneDecisionTypeByFillIndex,
           (u8)CELL_DECISION_STANDARD_EMPTY,
           sizeof(gauge->baseLaneDecisionTypeByFillIndex));
    memset(gauge->baseLaneDecisionIdxByFillIndex,
           STRIP_INDEX_EMPTY,
           sizeof(gauge->baseLaneDecisionIdxByFillIndex));
    memset(gauge->baseLaneDecisionCapStartBreakByFillIndex,
           0,
           sizeof(gauge->baseLaneDecisionCapStartBreakByFillIndex));
    memset(gauge->baseLaneDecisionCapStartTrailByFillIndex,
           0,
           sizeof(gauge->baseLaneDecisionCapStartTrailByFillIndex));
    memset(gauge->baseLaneDecisionUseBlinkVariantByFillIndex,
           0,
           sizeof(gauge->baseLaneDecisionUseBlinkVariantByFillIndex));
}

static void gauge_build_baseLane_fill_decisions(Gauge *gauge,
                                              u16 valuePixels,
                                              u16 trailPixelsRendered,
                                              u16 trailPixelsActual,
                                              u8 blinkOffActive,
                                              u8 trailMode)
{
    GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
    if (!baseLane)
        return;

    const GaugeLaneLayout *baseLaneLayout = baseLane->layout;
    FillLoopContext ctx;
    init_fill_loop_context(baseLaneLayout, valuePixels, trailPixelsRendered,
                           trailPixelsActual, blinkOffActive, trailMode, &ctx);
    gauge_clear_baseLane_fill_decisions(gauge);

    for (u8 cellIndex = 0; cellIndex < baseLaneLayout->length; cellIndex++)
    {
        const u8 fillIndex = baseLaneLayout->fillIndexByCell[cellIndex];
        const u8 segmentId = baseLaneLayout->segmentIdByCell[cellIndex];
        const u32 *bodyStrip = select_base_strip(
            baseLaneLayout->tilesetBySegment[segmentId],
            baseLaneLayout->gainTilesetBySegment[segmentId],
            trailMode);

        CellDecision decision;
        compute_cell_decision(baseLaneLayout, &ctx, cellIndex, fillIndex,
                              segmentId, bodyStrip, &decision);

        gauge->baseLaneDecisionTypeByFillIndex[fillIndex] = (u8)decision.type;
        gauge->baseLaneDecisionIdxByFillIndex[fillIndex] = decision.fillStripIndex;
        gauge->baseLaneDecisionCapStartBreakByFillIndex[fillIndex] = decision.capStartUsesBreak;
        gauge->baseLaneDecisionCapStartTrailByFillIndex[fillIndex] = decision.capStartUsesTrail;
        gauge->baseLaneDecisionUseBlinkVariantByFillIndex[fillIndex] = decision.useBlinkVariant;
    }
}

typedef struct
{
    u8 valid;
    const u32 *body;
    const u32 *end;
    const u32 *trail;
    const u32 *bridge;
    const u32 *capStart;
    const u32 *capStartBreak;
    const u32 *capStartTrail;
    const u32 *capEnd;
} LocalStripSet;

typedef struct
{
    LocalStripSet normalBySegment[GAUGE_MAX_SEGMENTS];
    LocalStripSet blinkBySegment[GAUGE_MAX_SEGMENTS];
} LocalStripCache;

static inline const u32 *layout_get_group_slot_strip(const GaugeLaneLayout *layout,
                                                     LayoutTilesetGroupId group,
                                                     LayoutTilesetSlot slot,
                                                     u8 segmentId)
{
    const LayoutBinding *binding = layout_get_binding(group, slot);
    if (!layout || !binding)
        return NULL;

    const u32 * const *view =
        *(const u32 * const * const *)((const u8 *)layout + binding->memberOffset);
    return view ? view[segmentId] : NULL;
}

static inline const u32 *resolve_layout_mode_strip(const GaugeLaneLayout *layout,
                                                   LayoutTilesetGroupId normalGroup,
                                                   LayoutTilesetGroupId gainGroup,
                                                   LayoutTilesetSlot slot,
                                                   u8 segmentId,
                                                   u8 trailMode,
                                                   u8 blinkVariant)
{
    const u32 *normalStrip = layout_get_group_slot_strip(layout, normalGroup, slot, segmentId);
    const u32 *gainStrip = layout_get_group_slot_strip(layout, gainGroup, slot, segmentId);

    return blinkVariant
        ? select_blink_strip(normalStrip, gainStrip, trailMode)
        : select_base_strip(normalStrip, gainStrip, trailMode);
}

static inline void build_local_strip_set(const GaugeLaneLayout *layout,
                                         u8 segmentId,
                                         u8 trailMode,
                                         u8 useBlinkVariant,
                                         LocalStripSet *set)
{
    static const LayoutTilesetSlot s_localStripSlots[] = {
        LAYOUT_TILESET_SLOT_BODY,
        LAYOUT_TILESET_SLOT_END,
        LAYOUT_TILESET_SLOT_TRAIL,
        LAYOUT_TILESET_SLOT_BRIDGE,
        LAYOUT_TILESET_SLOT_CAP_START,
        LAYOUT_TILESET_SLOT_CAP_START_BREAK,
        LAYOUT_TILESET_SLOT_CAP_START_TRAIL,
        LAYOUT_TILESET_SLOT_CAP_END
    };
    const u32 **slotTargets[] = {
        &set->body,
        &set->end,
        &set->trail,
        &set->bridge,
        &set->capStart,
        &set->capStartBreak,
        &set->capStartTrail,
        &set->capEnd
    };

    for (u8 slotIndex = 0; slotIndex < GAUGE_ARRAY_LEN(s_localStripSlots); slotIndex++)
    {
        const LayoutTilesetSlot slot = s_localStripSlots[slotIndex];
        *slotTargets[slotIndex] = resolve_layout_mode_strip(layout,
                                                            LAYOUT_TILESET_GROUP_BASE,
                                                            LAYOUT_TILESET_GROUP_GAIN,
                                                            slot,
                                                            segmentId,
                                                            trailMode,
                                                            0);

        if (!useBlinkVariant)
            continue;

        const u32 *blinkStrip = resolve_layout_mode_strip(layout,
                                                          LAYOUT_TILESET_GROUP_BLINK,
                                                          LAYOUT_TILESET_GROUP_GAIN_BLINK,
                                                          slot,
                                                          segmentId,
                                                          trailMode,
                                                          1);
        if (blinkStrip)
            *slotTargets[slotIndex] = blinkStrip;
    }

    set->valid = 1;
}

static inline const LocalStripSet *get_local_strip_set(const GaugeLaneLayout *layout,
                                                       u8 segmentId,
                                                       u8 trailMode,
                                                       u8 useBlinkVariant,
                                                       LocalStripCache *cache)
{
    LocalStripSet *set = useBlinkVariant
        ? &cache->blinkBySegment[segmentId]
        : &cache->normalBySegment[segmentId];

    if (!set->valid)
        build_local_strip_set(layout, segmentId, trailMode, useBlinkVariant, set);

    return set;
}

static inline const u32 *resolve_local_strip_from_baseLane_type(const LocalStripSet *stripSet,
                                                              CellDecisionType type,
                                                              u8 capStartUsesBreak,
                                                              u8 capStartUsesTrail)
{
    const u32 *bodyStrip = stripSet->body;
    const u32 *endStrip = stripSet->end;
    const u32 *trailStrip = stripSet->trail;
    const u32 *bridgeStrip = stripSet->bridge;
    const u32 *capStartStrip = stripSet->capStart;
    const u32 *capStartBreakStrip = stripSet->capStartBreak;
    const u32 *capStartTrailStrip = stripSet->capStartTrail;
    const u32 *capEndStrip = stripSet->capEnd;

    switch (type)
    {
    case CELL_DECISION_CAP_END:
        return capEndStrip ? capEndStrip : (endStrip ? endStrip : bodyStrip);
    case CELL_DECISION_CAP_START:
        if (capStartUsesTrail)
            return capStartTrailStrip ? capStartTrailStrip : (capStartStrip ? capStartStrip : bodyStrip);
        if (capStartUsesBreak)
            return capStartBreakStrip ? capStartBreakStrip : (capStartStrip ? capStartStrip : bodyStrip);
        return capStartStrip ? capStartStrip : bodyStrip;
    case CELL_DECISION_BRIDGE:
        return bridgeStrip ? bridgeStrip : bodyStrip;
    case CELL_DECISION_PARTIAL_END:
        return endStrip ? endStrip : bodyStrip;
    case CELL_DECISION_STANDARD_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL:
    case CELL_DECISION_PARTIAL_TRAIL2:
        return trailStrip ? trailStrip : bodyStrip;
    case CELL_DECISION_STANDARD_FULL:
    case CELL_DECISION_STANDARD_EMPTY:
    case CELL_DECISION_PARTIAL_VALUE:
    default:
        return bodyStrip;
    }
}

static inline u8 compute_linkedLane_terminal_end_index(const GaugeLaneLayout *layout,
                                                       u16 currentValuePixels,
                                                       u16 trailPixelsRendered,
                                                       u8 *outStripIndex)
{
    if (!layout || layout->length == 0)
        return 0;

    const u8 terminalFillIndex = (u8)(layout->length - 1);
    const u8 terminalCellIndex = layout->cellIndexByFillIndex[terminalFillIndex];
    u8 valuePxInTerminalCell = 0;
    u8 trailPxInTerminalCell = 0;

    compute_fill_for_cell(layout, terminalCellIndex,
                          currentValuePixels, trailPixelsRendered,
                          &valuePxInTerminalCell, &trailPxInTerminalCell);

    if (trailPxInTerminalCell == 0)
        return 0;

    if (outStripIndex)
    {
        *outStripIndex = s_tileIndexByValueTrail[valuePxInTerminalCell]
                                                [trailPxInTerminalCell];
    }

    return 1;
}

typedef struct
{
    u8 terminalOverrideActive;
    u8 terminalForcedIndex;
} LinkedLaneTerminalOverrideContext;

typedef struct FillLaneResolveContext FillLaneResolveContext;
typedef struct FillLaneCellDecision FillLaneCellDecision;

static void gauge_build_baseLane_pip_states(Gauge *gauge,
                                          u16 valuePixels,
                                          u16 trailPixelsRendered,
                                          u16 trailPixelsActual,
                                          u8 blinkOffActive,
                                          u8 trailMode)
{
    GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
    if (!baseLane)
        return;

    const GaugeLaneLayout *baseLaneLayout = baseLane->layout;
    memset(gauge->baseLanePipStateByFillIndex,
           PIP_STATE_EMPTY,
           sizeof(gauge->baseLanePipStateByFillIndex));

    for (u8 fillIndex = 0; fillIndex < baseLaneLayout->length; fillIndex++)
    {
        const u8 cellIndex = baseLaneLayout->cellIndexByFillIndex[fillIndex];
        gauge->baseLanePipStateByFillIndex[fillIndex] =
            select_pip_state_for_cell(baseLaneLayout, cellIndex,
                                      valuePixels, trailPixelsRendered, trailPixelsActual,
                                      blinkOffActive, trailMode);
    }
}

typedef struct FillLaneResolveContext
{
    const Gauge *gauge;
    const GaugeLaneLayout *layout;
    u8 trailMode;
    u8 terminalFillIndex;
    LinkedLaneTerminalOverrideContext terminalCtx;
    LocalStripCache stripCache;
} FillLaneResolveContext;

typedef struct FillLaneCellDecision
{
    u8 segmentId;
    CellDecisionType baseLaneType;
    u8 baseLaneIdx;
    u8 terminalOverrideApplied;
    CellDecision decision;
} FillLaneCellDecision;

static inline u8 init_fill_lane_resolve_context(GaugeLaneInstance *lane,
                                                u16 valuePixels,
                                                u16 trailPixelsRendered,
                                                u8 trailMode,
                                                FillLaneResolveContext *out)
{
    const Gauge *gauge = lane->gauge;
    if (!gauge || !out)
        return 0;

    out->gauge = gauge;
    out->layout = lane->layout;
    out->trailMode = trailMode;
    out->terminalFillIndex = lane->layout ? (u8)(lane->layout->length - 1) : 0;
    out->terminalCtx.terminalOverrideActive = 0;
    out->terminalCtx.terminalForcedIndex = STRIP_INDEX_FULL;
    memset(&out->stripCache, 0, sizeof(out->stripCache));

    if (lane != gauge_get_baseLane_instance(gauge) &&
        lane->layout &&
        lane->layout->endOverrideEnabled &&
        lane->layout->length != 0)
    {
        out->terminalCtx.terminalOverrideActive = compute_linkedLane_terminal_end_index(
            lane->layout,
            valuePixels,
            trailPixelsRendered,
            &out->terminalCtx.terminalForcedIndex);
    }
    return 1;
}

static inline void resolve_fill_lane_cell_decision(GaugeLaneInstance *lane,
                                                   FillLaneResolveContext *context,
                                                   u8 cellIndex,
                                                   FillLaneCellDecision *out)
{
    const GaugeLaneLayout *layout = context->layout;
    const u8 cellFillIndex = layout->fillIndexByCell[cellIndex];
    const u8 mappedBaseLaneFillIndex = lane->baseLaneFillIndexByCell[cellIndex];
    const u8 segmentId = layout->segmentIdByCell[cellIndex];
    const CellDecisionType baseLaneType =
        (CellDecisionType)context->gauge->baseLaneDecisionTypeByFillIndex[mappedBaseLaneFillIndex];
    const u8 baseLaneIdx =
        context->gauge->baseLaneDecisionIdxByFillIndex[mappedBaseLaneFillIndex];
    const u8 capStartUsesBreak =
        context->gauge->baseLaneDecisionCapStartBreakByFillIndex[mappedBaseLaneFillIndex];
    const u8 capStartUsesTrail =
        context->gauge->baseLaneDecisionCapStartTrailByFillIndex[mappedBaseLaneFillIndex];
    const u8 useBlinkVariant =
        context->gauge->baseLaneDecisionUseBlinkVariantByFillIndex[mappedBaseLaneFillIndex];
    const LocalStripSet *stripSet = get_local_strip_set(layout,
                                                        segmentId,
                                                        context->trailMode,
                                                        useBlinkVariant,
                                                        &context->stripCache);
    const u32 *fallbackBody = stripSet->body;
    const u32 *fallbackTrail = resolve_local_strip_from_baseLane_type(
        stripSet, CELL_DECISION_PARTIAL_TRAIL, 0, 0);

    out->segmentId = segmentId;
    out->baseLaneType = baseLaneType;
    out->baseLaneIdx = baseLaneIdx;
    out->terminalOverrideApplied = 0;
    out->decision.type = baseLaneType;
    out->decision.strip = NULL;
    out->decision.fillStripIndex = baseLaneIdx;
    out->decision.capStartUsesBreak = capStartUsesBreak;
    out->decision.capStartUsesTrail = capStartUsesTrail;
    out->decision.useBlinkVariant = useBlinkVariant;
    out->decision.strip = resolve_local_strip_from_baseLane_type(stripSet,
                                                                 baseLaneType,
                                                                 capStartUsesBreak,
                                                                 capStartUsesTrail);

    if (!fallbackBody)
    {
        fallbackBody = select_base_strip(layout->tilesetBySegment[segmentId],
                                         layout->gainTilesetBySegment[segmentId],
                                         context->trailMode);
        if (!fallbackTrail)
            fallbackTrail = fallbackBody;
    }

    if (context->terminalCtx.terminalOverrideActive &&
        cellFillIndex == context->terminalFillIndex &&
        stripSet->end)
    {
        out->decision.type = CELL_DECISION_PARTIAL_END;
        out->decision.fillStripIndex = context->terminalCtx.terminalForcedIndex;
        out->decision.capStartUsesBreak = 0;
        out->decision.capStartUsesTrail = 0;
        out->terminalOverrideApplied = 1;
        out->decision.strip = resolve_local_strip_from_baseLane_type(
            stripSet, CELL_DECISION_PARTIAL_END, 0, 0);
    }

    if (!out->decision.strip)
    {
        out->decision.strip = fallbackBody;
        out->decision.type = CELL_DECISION_PARTIAL_VALUE;
    }

    if (out->decision.fillStripIndex > 63)
        out->decision.fillStripIndex = 63;

    if ((out->decision.strip != fallbackTrail || fallbackTrail == fallbackBody) &&
        out->decision.fillStripIndex > 44)
    {
        out->decision.fillStripIndex = 44;
    }
}

/**
 * Process fill mode: stream tiles for all cells via DMA.
 *
 * Each cell has its own dedicated VRAM tile. When the gauge value changes,
 * only the affected cells get new tile data DMA'd from ROM strips.
 * Change detection per cell (cachedFillIndex + cachedStrip) avoids
 * redundant DMA transfers.
 *
 * Cell classification is computed once on the baseLane for the frame.
 * This function only projects local cells to baseLane fill indices, resolves
 * the local strip fallback, then uploads the selected tile if needed.
 *
 * @param lane                 Lane to render
 * @param valuePixels          Current value fill in pixels
 * @param trailPixelsRendered  Current trail fill in pixels (after blink)
 * @param trailPixelsActual    Current trail fill in pixels (before blink)
 * @param blinkOffActive       1 if blink-off rendering is active this frame
 * @param trailMode            Current trail mode (DAMAGE/GAIN/NONE)
 *
 * Cost: ~200 cycles per cell (DMA setup dominant), ~50 cycles for trivial cells
 */
static void process_fill_mode(GaugeLaneInstance *lane,
                              u16 valuePixels,
                              u16 trailPixelsRendered,
                              u16 trailPixelsActual,
                              u8 blinkOffActive,
                              u8 blinkOnChanged,
                              u8 trailMode,
                              u8 trailModeChanged)
{
    (void)trailPixelsActual;
    (void)blinkOffActive;
    (void)blinkOnChanged;
    (void)trailModeChanged;
    const GaugeLaneLayout *layout = lane->layout;
    GaugeTileUploadSpan uploadSpan;
    FillLaneResolveContext resolveContext;
    if (!init_fill_lane_resolve_context(lane,
                                        valuePixels,
                                        trailPixelsRendered,
                                        trailMode,
                                        &resolveContext))
        return;

    tile_upload_span_reset(&uploadSpan);

#if GAUGE_ENABLE_TRACE
    const u8 traceEnabled = s_traceContext.active;
    GaugeTraceCell traceByCell[GAUGE_MAX_LENGTH];

    if (traceEnabled)
        memset(traceByCell, 0, sizeof(traceByCell));
#endif

    for (u8 i = 0; i < lane->cellCount; i++)
    {
        GaugeStreamCell *cell = &lane->cells[i];
        const u8 cellIndex = cell->cellIndex;
        FillLaneCellDecision cellDecision;
        resolve_fill_lane_cell_decision(lane, &resolveContext, cellIndex, &cellDecision);

#if GAUGE_ENABLE_TRACE
        if (traceEnabled)
        {
            traceByCell[cellIndex].used = 1;
            traceByCell[cellIndex].segmentId = cellDecision.segmentId;
            traceByCell[cellIndex].baseLaneType = cellDecision.baseLaneType;
            traceByCell[cellIndex].baseLaneIdx = cellDecision.baseLaneIdx;
            traceByCell[cellIndex].terminalOverrideApplied =
                cellDecision.terminalOverrideApplied;
            traceByCell[cellIndex].decision = cellDecision.decision;
        }
#endif

        if (cellDecision.decision.strip &&
            (cell->cachedFillIndex != cellDecision.decision.fillStripIndex ||
             cell->cachedStrip != cellDecision.decision.strip))
        {
            tile_upload_span_push(&uploadSpan,
                                  cellDecision.decision.strip,
                                  cell->vramTileIndex,
                                  cellDecision.decision.fillStripIndex);
            cell->cachedFillIndex = cellDecision.decision.fillStripIndex;
            cell->cachedStrip = cellDecision.decision.strip;
        }
    }

    tile_upload_span_flush(&uploadSpan);

#if GAUGE_ENABLE_TRACE
    if (traceEnabled)
    {
        for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
        {
            if (!traceByCell[cellIndex].used)
                continue;

            const CellDecision *decision = &traceByCell[cellIndex].decision;
            kprintf(
                "GAUGE_TRACE CELL lane=%u cell=%u seg=%u class=%s type=%s strip=0x%08lX idx=%u ovr=%s baseLaneType=%s baseLaneIdx=%u\n",
                (unsigned int)s_traceContext.laneIndex,
                (unsigned int)cellIndex,
                (unsigned int)traceByCell[cellIndex].segmentId,
                decision_class_to_text(decision->type,
                                       decision->capStartUsesBreak,
                                       decision->capStartUsesTrail),
                trace_text_lookup(s_decisionTypeText,
                                  GAUGE_ARRAY_LEN(s_decisionTypeText),
                                  decision->type,
                                  "UNKNOWN"),
                (unsigned long)(u32)decision->strip,
                (unsigned int)decision->fillStripIndex,
                traceByCell[cellIndex].terminalOverrideApplied ? "TERM_END" : "NONE",
                trace_text_lookup(s_decisionTypeText,
                                  GAUGE_ARRAY_LEN(s_decisionTypeText),
                                  traceByCell[cellIndex].baseLaneType,
                                  "UNKNOWN"),
                (unsigned int)traceByCell[cellIndex].baseLaneIdx
            );
        }
    }
#endif
}

/**
 * Determine the PIP visual state for a single cell.
 *
 * Priority rules:
 * 1. Cell fully covered by visible value -> PIP_STATE_VALUE
 * 2. Cell fully covered by visible trail -> PIP_STATE_LOSS / PIP_STATE_GAIN
 * 3. Hidden follow trail in blink-off    -> PIP_STATE_BLINK_OFF / PIP_STATE_EMPTY
 * 4. Default                             -> PIP_STATE_EMPTY
 *
 * @return One of PIP_STATE_* constants (0..4)
 */
static inline u8 select_pip_state_for_cell(const GaugeLaneLayout *layout,
                                           u8 cellIndex,
                                           u16 valuePixels,
                                           u16 trailPixelsRendered,
                                           u16 trailPixelsActual,
                                           u8 blinkOffActive,
                                           u8 trailMode)
{
    u8 valuePxInTile;
    u8 trailRenderedPxInTile;
    compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsRendered,
                          &valuePxInTile, &trailRenderedPxInTile);

    if (valuePxInTile == GAUGE_PIXELS_PER_TILE)
        return PIP_STATE_VALUE;

    if (trailRenderedPxInTile == GAUGE_PIXELS_PER_TILE)
    {
        if (trailMode == GAUGE_TRAIL_STATE_GAIN)
            return PIP_STATE_GAIN;
        if (trailMode == GAUGE_TRAIL_STATE_DAMAGE)
            return PIP_STATE_LOSS;
        return PIP_STATE_EMPTY;
    }

    u8 valueUnused;
    u8 trailActualPxInTile;
    compute_fill_for_cell(layout, cellIndex, valuePixels, trailPixelsActual,
                          &valueUnused, &trailActualPxInTile);

    if (trailActualPxInTile != GAUGE_PIXELS_PER_TILE)
        return PIP_STATE_EMPTY;

    if (blinkOffActive)
    {
        if (trailMode == GAUGE_TRAIL_STATE_GAIN)
            return PIP_STATE_EMPTY;
        return PIP_STATE_BLINK_OFF;
    }
    
    return PIP_STATE_EMPTY;
}

static inline u8 resolve_available_pip_state(u8 requestedState,
                                             u8 stateCount,
                                             u8 trailMode)
{
    if (stateCount < 2)
        return PIP_STATE_EMPTY;

    /* Gain mode contract:
     * - if GAIN state is missing, render gain zone as VALUE (no blinking)
     * - never use BLINK_OFF for gain mode in PIP
     */
    if (trailMode == GAUGE_TRAIL_STATE_GAIN)
    {
        if (stateCount <= PIP_STATE_GAIN)
        {
            switch (requestedState)
            {
            case PIP_STATE_GAIN:
            case PIP_STATE_LOSS:
            case PIP_STATE_BLINK_OFF:
                return PIP_STATE_VALUE;
            default:
                break;
            }
        }
        else if (requestedState == PIP_STATE_BLINK_OFF)
        {
            return PIP_STATE_EMPTY;
        }
    }

    switch (requestedState)
    {
    case PIP_STATE_EMPTY:
        return PIP_STATE_EMPTY;
    case PIP_STATE_VALUE:
        return PIP_STATE_VALUE;
    case PIP_STATE_LOSS:
        return (stateCount > PIP_STATE_LOSS) ? PIP_STATE_LOSS : PIP_STATE_EMPTY;
    case PIP_STATE_GAIN:
        if (stateCount > PIP_STATE_GAIN)
            return PIP_STATE_GAIN;
        if (stateCount > PIP_STATE_LOSS)
            return PIP_STATE_LOSS;
        return PIP_STATE_VALUE;
    case PIP_STATE_BLINK_OFF:
        if (stateCount > PIP_STATE_BLINK_OFF)
            return PIP_STATE_BLINK_OFF;
        if (stateCount > PIP_STATE_LOSS)
            return PIP_STATE_LOSS;
        return PIP_STATE_EMPTY;
    default:
        return PIP_STATE_EMPTY;
    }
}

typedef struct
{
    u8 requestedState;
    u8 resolvedState;
    u8 stateCount;
    u16 renderStateIndex;
} PipResolvedRenderState;

static inline void resolve_pip_render_state(const Gauge *gauge,
                                            const GaugeLaneInstance *lane,
                                            const GaugeLaneLayout *layout,
                                            u8 cellIndex,
                                            u8 segmentId,
                                            u8 renderIndex,
                                            u8 trailMode,
                                            PipResolvedRenderState *out)
{
    const u8 mappedBaseLaneFillIndex = lane->baseLaneFillIndexByCell[cellIndex];
    out->requestedState = gauge->baseLanePipStateByFillIndex[mappedBaseLaneFillIndex];
    out->stateCount = layout->pipStateCountBySegment[segmentId];
    out->resolvedState = resolve_available_pip_state(out->requestedState,
                                                     out->stateCount,
                                                     trailMode);
    out->renderStateIndex =
        compute_pip_render_state_lut_index(out->resolvedState, renderIndex);
}

/**
 * PIP-mode renderer: update each cell's tile from compact strip based on pip state.
 *
 * For each cell, computes its PIP state (VALUE/EMPTY/LOSS/GAIN/BLINK_OFF),
 * then selects the corresponding tile from precomputed render LUTs.
 *
 * Uses the fixed VRAM path only: one uploaded tile per rendered PIP tile.
 */
static void process_pip_mode(GaugeLaneInstance *lane,
                             u16 valuePixels,
                             u16 trailPixelsRendered,
                             u16 trailPixelsActual,
                             u8 blinkOffActive,
                             u8 blinkOnChanged,
                             u8 trailMode,
                             u8 trailModeChanged)
{
    (void)valuePixels;
    (void)trailPixelsRendered;
    (void)trailPixelsActual;
    (void)blinkOffActive;
    (void)blinkOnChanged;
    (void)trailModeChanged;
    const GaugeLaneLayout *layout = lane->layout;
    const Gauge *gauge = lane->gauge;
    if (!gauge)
        return;

    GaugeTileUploadSpan uploadSpan;
    u8 currentRow = CACHE_INVALID_U8;

    tile_upload_span_reset(&uploadSpan);

    for (u8 i = 0; i < lane->cellCount; i++)
    {
        GaugeStreamCell *cell = &lane->cells[i];
        const u8 cellIndex = cell->cellIndex;
        const u8 segmentId = layout->segmentIdByCell[cellIndex];
        const u32 *pipStrip = layout->pipTilesetBySegment[segmentId];
        if (!pipStrip)
            continue;

        if (cell->pipFillIndex == CACHE_INVALID_U8)
            continue;
        const u8 renderIndex = cell->pipRenderIndex;
        if (renderIndex == CACHE_INVALID_U8)
            continue;

        if (currentRow != CACHE_INVALID_U8 && cell->pipRow != currentRow)
            tile_upload_span_flush(&uploadSpan);
        currentRow = cell->pipRow;

        PipResolvedRenderState renderState;
        resolve_pip_render_state(gauge,
                                 lane,
                                 layout,
                                 cellIndex,
                                 segmentId,
                                 renderIndex,
                                 trailMode,
                                 &renderState);
        const u8 stripIndex = layout->pipRenderStripIndexByState[renderState.renderStateIndex];
        const u8 changed =
            (cell->cachedFillIndex != stripIndex) ||
            (cell->cachedStrip != pipStrip);

        if (changed)
        {
            tile_upload_span_push(&uploadSpan,
                                  pipStrip,
                                  cell->vramTileIndex,
                                  stripIndex);
            cell->cachedFillIndex = stripIndex;
            cell->cachedStrip = pipStrip;
        }

#if GAUGE_ENABLE_TRACE
        if (s_traceContext.active)
        {
            kprintf(
                "GAUGE_TRACE CELL lane=%u cell=%u seg=%u class=PIP strip=0x%08lX idx=%u req=%s res=%s tile=%u render=%u changed=%u states=%u\n",
                (unsigned int)s_traceContext.laneIndex,
                (unsigned int)cellIndex,
                (unsigned int)segmentId,
                (unsigned long)(u32)pipStrip,
                (unsigned int)stripIndex,
                trace_text_lookup(s_pipStateText,
                                  GAUGE_ARRAY_LEN(s_pipStateText),
                                  renderState.requestedState,
                                  "EMPTY"),
                trace_text_lookup(s_pipStateText,
                                  GAUGE_ARRAY_LEN(s_pipStateText),
                                  renderState.resolvedState,
                                  "EMPTY"),
                (unsigned int)cell->vramTileIndex,
                (unsigned int)renderIndex,
                (unsigned int)changed,
                (unsigned int)renderState.stateCount
            );
        }
#endif
    }

    tile_upload_span_flush(&uploadSpan);
}


/* =============================================================================
   Render/update dispatchers
   ============================================================================= */

/**
 * Select the correct render handler based on value mode.
 */
static GaugeLaneRenderHandler *resolve_lane_render_handler(GaugeValueMode valueMode)
{
    if (valueMode == GAUGE_VALUE_MODE_PIP)
        return process_pip_mode;

    return process_fill_mode;
}

static void gauge_tick_and_render_fill(Gauge *gauge);
static void gauge_tick_and_render_pip(Gauge *gauge);

/** Select the tick-and-render handler: fill (continuous) or pip (discrete). */
static GaugeTickAndRenderHandler *resolve_tick_and_render_handler(GaugeValueMode valueMode)
{
    return (valueMode == GAUGE_VALUE_MODE_PIP)
        ? gauge_tick_and_render_pip
        : gauge_tick_and_render_fill;
}


/* =============================================================================
   GaugeLaneInstance initialization (internal)
   ============================================================================= */

/**
 * Initialize a GaugeLaneInstance with all parameters.
 */
static u8 GaugeLaneInstance_initInternal(GaugeLaneInstance *lane,
                                         const Gauge *gauge,
                                         GaugeLaneLayout *layout,
                                         u16 originX,
                                         u16 originY,
                                         u16 vramBase,
                                         GaugeRuntimeArena *runtimeArena)
{
    if (!lane || !gauge || !layout || !runtimeArena || layout->length == 0)
        return 0;

    lane->originX = originX;
    lane->originY = originY;
    lane->vramBase = vramBase;
    lane->renderHandler = resolve_lane_render_handler(gauge->valueMode);
    lane->gauge = gauge;
    lane->layout = layout;
    lane->cells = NULL;
    lane->cellCount = 0;

    GaugeLaneLayout_retain(layout);

    const u8 streamCellCapacity = gauge_compute_stream_cell_capacity(gauge->valueMode, layout);

    lane->cells = (GaugeStreamCell *)gauge_runtime_arena_alloc(
        runtimeArena,
        (u16)(streamCellCapacity * (u8)sizeof(GaugeStreamCell)),
        2);
    if (!lane->cells)
    {
        GaugeLaneLayout_release(layout);
        return 0;
    }

    if (gauge->valueMode == GAUGE_VALUE_MODE_PIP)
    {
        write_tilemap_pip_init(lane);
        return 1;
    }

    write_tilemap_fill_init(lane);
    return 1;
}

static void GaugeLaneInstance_releaseInternal(GaugeLaneInstance *lane)
{
    if (!lane)
        return;

    if (lane->layout)
    {
        GaugeLaneLayout_release((GaugeLaneLayout *)lane->layout);
        lane->layout = NULL;
    }

    lane->cells = NULL;
    lane->cellCount = 0;
}



/* =============================================================================
   Gauge runtime support
   ============================================================================= */

static u8 gauge_compute_stream_cell_capacity(GaugeValueMode valueMode,
                                             const GaugeLaneLayout *layout)
{
    if (!layout)
        return 0;

    if (valueMode == GAUGE_VALUE_MODE_PIP && layout->pipRenderCount > layout->length)
        return layout->pipRenderCount;

    return layout->length;
}

static u16 gauge_compute_runtime_arena_size(GaugeLaneLayout * const *builtLayouts,
                                            u8 laneCount,
                                            GaugeValueMode valueMode,
                                            u16 maxFillPixels)
{
    u16 totalBytes = 0;

    totalBytes = gauge_runtime_arena_align(totalBytes, 4);
    totalBytes = (u16)(totalBytes + (u16)(laneCount * (u8)sizeof(GaugeLaneInstance *)));

    totalBytes = gauge_runtime_arena_align(totalBytes, 4);
    totalBytes = (u16)(totalBytes + (u16)(laneCount * (u16)sizeof(GaugeLaneInstance)));

    totalBytes = gauge_runtime_arena_align(totalBytes, 2);
    totalBytes = (u16)(totalBytes + (u16)((GAUGE_LUT_CAPACITY + 1) * (u8)sizeof(u16)));

    if (valueMode == GAUGE_VALUE_MODE_PIP)
    {
        totalBytes = gauge_runtime_arena_align(totalBytes, 2);
        totalBytes = (u16)(totalBytes + (u16)((maxFillPixels + 1) * (u8)sizeof(u16)));
    }

    for (u8 laneIndex = 0; laneIndex < laneCount; laneIndex++)
    {
        const GaugeLaneLayout *layout = builtLayouts[laneIndex];
        if (!layout)
            continue;

        totalBytes = gauge_runtime_arena_align(totalBytes, 2);
        totalBytes = (u16)(totalBytes + (u16)(gauge_compute_stream_cell_capacity(valueMode, layout) *
                                              (u8)sizeof(GaugeStreamCell)));
    }

    return totalBytes;
}

/**
 * Populate a value-to-pixels LUT for FILL mode scaling.
 * Formula: dest[v] = round(v * maxFillPixels / maxValue).
 * The +maxValue/2 provides rounding instead of truncation.
 *
 * @param dest          Destination array (must have maxValue + 1 entries)
 * @param maxValue      Maximum logical value
 * @param maxFillPixels Total pixel span
 */
static void fill_value_to_pixels_lut(u16 *dest, u16 maxValue, u16 maxFillPixels)
{
    if (maxValue == 0)
    {
        dest[0] = 0;
        return;
    }

    const u16 halfMax = maxValue >> 1;
    for (u16 v = 0; v <= maxValue; v++)
    {
        const u32 numerator = (u32)v * (u32)maxFillPixels + halfMax;
        dest[v] = (u16)(numerator / maxValue);
    }
}

static u16 compute_pip_total_pixels(const GaugeLaneLayout *layout)
{
    u16 totalPixels = 0;
    for (u8 pipIndex = 0; pipIndex < layout->pipCount; pipIndex++)
    {
        totalPixels = (u16)(totalPixels + ((u16)layout->pipWidthByPipIndex[pipIndex] << TILE_TO_PIXEL_SHIFT));
    }
    return totalPixels;
}

static inline u16 clamp_max_value_to_capacity(u16 maxValue)
{
    return (maxValue > GAUGE_LUT_CAPACITY) ? GAUGE_LUT_CAPACITY : maxValue;
}

/*
 * Populate a non-structural PIP LUT.
 *
 * The visual pip topology is fixed by the built layout. `maxValue` can still
 * change later, so logical values are projected onto the existing pip
 * boundaries with floor(value * pipCount / maxValue).
 */
static void fill_pip_value_lut(u16 *dest, u16 maxValue, const GaugeLaneLayout *layout)
{
    u16 pipBoundaryPixels[GAUGE_MAX_LENGTH + 1];
    pipBoundaryPixels[0] = 0;

    u16 cumulativePixels = 0;
    for (u8 pipIndex = 0; pipIndex < layout->pipCount; pipIndex++)
    {
        cumulativePixels = (u16)(cumulativePixels + ((u16)layout->pipWidthByPipIndex[pipIndex] << TILE_TO_PIXEL_SHIFT));
        pipBoundaryPixels[pipIndex + 1] = cumulativePixels;
    }

    if (maxValue == 0)
    {
        dest[0] = 0;
        return;
    }

    for (u16 value = 0; value <= maxValue; value++)
    {
        u16 boundaryIndex = (u16)(((u32)value * (u32)layout->pipCount) / maxValue);
        if (boundaryIndex > layout->pipCount)
            boundaryIndex = layout->pipCount;
        dest[value] = pipBoundaryPixels[boundaryIndex];
    }
}

/*
 * Build the inverse PIP quantization LUT used by quantize_pixels_to_pip_step().
 *
 * dest[pixels] = largest quantized pip boundary <= pixels
 *
 * The build cost is paid once at Gauge_build() time, then again only when
 * Gauge_setMaxValue() changes the direct PIP LUT.
 */
static void fill_pip_pixels_to_quantized_pixels_lut(u16 *dest,
                                                    u16 maxFillPixels,
                                                    const u16 *valueToPixelsLUT,
                                                    u16 maxValue)
{
    if (!dest)
        return;

    if (!valueToPixelsLUT || maxValue == 0)
    {
        for (u16 pixels = 0; pixels <= maxFillPixels; pixels++)
            dest[pixels] = (u16)((pixels >> TILE_TO_PIXEL_SHIFT) << TILE_TO_PIXEL_SHIFT);
        return;
    }

    u16 previousBoundary = valueToPixelsLUT[0];
    u16 fillPixels = 0;

    for (u16 value = 0; value < maxValue; value++)
    {
        const u16 nextBoundary = valueToPixelsLUT[value + 1];
        while (fillPixels < nextBoundary && fillPixels <= maxFillPixels)
        {
            dest[fillPixels] = previousBoundary;
            fillPixels++;
        }
        previousBoundary = nextBoundary;
    }

    while (fillPixels <= maxFillPixels)
    {
        dest[fillPixels] = previousBoundary;
        fillPixels++;
    }
}

static inline u8 gauge_layout_has_bridge(const GaugeLaneLayout *layout)
{
    if (!layout || layout->segmentCount == 0)
        return 0;

    return (segment_tileset_array_has_any(layout->tilesetBridgeBySegment, layout->segmentCount) ||
            segment_tileset_array_has_any(layout->gainTilesetBridgeBySegment, layout->segmentCount) ||
            segment_tileset_array_has_any(layout->blinkOffTilesetBridgeBySegment, layout->segmentCount) ||
            segment_tileset_array_has_any(layout->gainBlinkOffTilesetBridgeBySegment, layout->segmentCount))
        ? 1 : 0;
}

static void build_base_lane_projection_maps(Gauge *gauge)
{
    GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
    if (!gauge || !baseLane || !baseLane->layout)
        return;

    const GaugeLaneLayout *baseLaneLayout = baseLane->layout;
    for (u8 laneIndex = 0; laneIndex < gauge->laneCount; laneIndex++)
    {
        GaugeLaneInstance *lane = gauge->lanes[laneIndex];
        if (!lane || !lane->layout)
            continue;

        const GaugeLaneLayout *layout = lane->layout;
        for (u8 cellIndex = 0; cellIndex < layout->length; cellIndex++)
        {
            const u8 localFillIndex = layout->fillIndexByCell[cellIndex];
            const u16 samplePixel = (u16)(layout->fillOffset +
                                          FILL_IDX_TO_OFFSET(localFillIndex) + 4);
            u8 dummyPx;
            u8 mappedFillIndex = compute_fill_index_and_px(baseLaneLayout, samplePixel, &dummyPx);
            if (mappedFillIndex >= baseLaneLayout->length)
                mappedFillIndex = (u8)(baseLaneLayout->length - 1);
            lane->baseLaneFillIndexByCell[cellIndex] = mappedFillIndex;
        }

        for (u8 cellIndex = layout->length; cellIndex < GAUGE_MAX_LENGTH; cellIndex++)
            lane->baseLaneFillIndexByCell[cellIndex] = 0;
    }
}

static void gauge_rebuild_value_to_pixels_lut(Gauge *gauge)
{
    if (!gauge || !gauge->logic.valueToPixelsData)
        return;

    GaugeLogic *logic = &gauge->logic;
    if (gauge->valueMode == GAUGE_VALUE_MODE_PIP)
    {
        GaugeLaneInstance *baseLane = gauge_get_baseLane_instance(gauge);
        if (!baseLane || !baseLane->layout)
            return;

        fill_pip_value_lut(logic->valueToPixelsData, logic->maxValue, baseLane->layout);
        logic->valueToPixelsLUT = logic->valueToPixelsData;
        return;
    }

    fill_value_to_pixels_lut(logic->valueToPixelsData, logic->maxValue, logic->maxFillPixels);
    logic->valueToPixelsLUT = (logic->maxValue == logic->maxFillPixels)
        ? NULL
        : logic->valueToPixelsData;
}

static void gauge_rebuild_pip_quantize_lut(Gauge *gauge)
{
    if (!gauge || gauge->valueMode != GAUGE_VALUE_MODE_PIP)
        return;

    GaugeLogic *logic = &gauge->logic;
    if (!logic->pixelsToQuantizedPixelsLUT)
        return;

    fill_pip_pixels_to_quantized_pixels_lut(logic->pixelsToQuantizedPixelsLUT,
                                            logic->maxFillPixels,
                                            logic->valueToPixelsLUT,
                                            logic->maxValue);
}

static void gauge_apply_behavior_from_definition(Gauge *gauge,
                                                 const GaugeBehavior *behavior)
{
    GaugeLogic *logic = &gauge->logic;
    GaugeTrailMode trailMode = GAUGE_TRAIL_MODE_DISABLED;
    GaugeGainMode gainMode = GAUGE_GAIN_MODE_DISABLED;

    if (behavior)
    {
        trailMode = (behavior->damageMode <= GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK)
            ? behavior->damageMode
            : GAUGE_TRAIL_MODE_FOLLOW;
        gainMode = (behavior->gainMode <= GAUGE_GAIN_MODE_RESERVED_2)
            ? behavior->gainMode
            : GAUGE_GAIN_MODE_DISABLED;

        logic->valueAnimEnabled = behavior->valueAnimEnabled ? 1 : 0;
        logic->valueAnimShift = (behavior->valueAnimShift == 0)
            ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT
            : behavior->valueAnimShift;
        logic->trailAnimShift = (behavior->damageAnimShift == 0)
            ? GAUGE_DEFAULT_TRAIL_ANIM_SHIFT
            : behavior->damageAnimShift;
        logic->blinkShift = (behavior->damageBlinkShift == 0)
            ? GAUGE_DEFAULT_BLINK_SHIFT
            : behavior->damageBlinkShift;
        logic->gainAnimShift = (behavior->gainAnimShift == 0)
            ? GAUGE_DEFAULT_VALUE_ANIM_SHIFT
            : behavior->gainAnimShift;
        logic->gainBlinkShift = (behavior->gainBlinkShift == 0)
            ? GAUGE_DEFAULT_BLINK_SHIFT
            : behavior->gainBlinkShift;
        logic->criticalValue = (behavior->criticalValue > logic->maxValue)
            ? logic->maxValue
            : behavior->criticalValue;
    }

    logic->configuredTrailMode = (u8)trailMode;
    logic->configuredGainMode = (u8)gainMode;
    logic->trailEnabled = compute_trail_enabled(trailMode, gainMode);
    set_mode_flags(logic, trailMode);
    gauge->logicTickHandler = resolve_logic_tick_handler(trailMode);
    apply_configured_trail_mode_state(logic, 0);
    invalidate_render_cache(logic);
}

static u8 gauge_is_zeroed(const Gauge *gauge)
{
    const u8 *rawGauge = (const u8 *)gauge;
    for (u16 byteIndex = 0; byteIndex < (u16)sizeof(*gauge); byteIndex++)
    {
        if (rawGauge[byteIndex] != 0)
            return 0;
    }
    return 1;
}

static u16 reproject_trail_pixels_by_ratio(u16 oldTrailPixels,
                                           u16 oldMaxFillPixels,
                                           u16 newMaxFillPixels)
{
    if (oldMaxFillPixels == 0)
        return 0;

    return (u16)((((u32)oldTrailPixels * (u32)newMaxFillPixels) +
                  (u32)(oldMaxFillPixels >> 1)) / oldMaxFillPixels);
}

static inline void gauge_refresh_value_target_pixels(GaugeLogic *logic)
{
    logic->valueTargetPixels = value_to_pixels(logic, logic->currentValue);
    if (logic->valueTargetPixels > logic->maxFillPixels)
        logic->valueTargetPixels = logic->maxFillPixels;
}

static inline void gauge_set_current_value_and_target(GaugeLogic *logic,
                                                      u16 newValue,
                                                      u8 syncDisplayedValue)
{
    logic->currentValue = (newValue > logic->maxValue) ? logic->maxValue : newValue;
    gauge_refresh_value_target_pixels(logic);
    if (syncDisplayedValue)
        logic->valuePixels = logic->valueTargetPixels;
}

/* =============================================================================
   Direct GaugeDefinition build helpers
   ============================================================================= */

static inline u8 sanitize_value_mode(u8 mode)
{
    return (mode == GAUGE_VALUE_MODE_PIP) ? GAUGE_VALUE_MODE_PIP : GAUGE_VALUE_MODE_FILL;
}

static inline u8 sanitize_orientation(u8 orientation)
{
    return (orientation == GAUGE_ORIENT_VERTICAL) ? GAUGE_ORIENT_VERTICAL : GAUGE_ORIENT_HORIZONTAL;
}

static inline u8 sanitize_fill_direction(u8 fillDirection)
{
    return (fillDirection == GAUGE_FILL_REVERSE) ? GAUGE_FILL_REVERSE : GAUGE_FILL_FORWARD;
}

static inline u8 sanitize_palette_index(u8 palette)
{
    return (u8)(palette & 3);
}

static u8 definition_lane_is_empty(const GaugeLane *lane)
{
    if (!lane)
        return 1;

    return (lane->segments[0].cells == 0 || lane->segments[0].skin == NULL) ? 1 : 0;
}

static u8 compute_segment_display_cells(GaugeMode mode,
                                        const GaugeSegment *segment,
                                        u16 *outDisplayCells)
{
    if (!segment || !segment->skin || !outDisplayCells || segment->cells == 0)
        return 0;

    if (mode == GAUGE_MODE_PIP)
    {
        const GaugePipSkin *pip = &segment->skin->pip;
        const u8 pipWidth = pip->pipWidth ? pip->pipWidth : 1;
        *outDisplayCells = (u16)(segment->cells * pipWidth);
    }
    else
    {
        *outDisplayCells = segment->cells;
    }

    return 1;
}

static u16 compute_lane_display_cells(const GaugeDefinition *definition,
                                      const GaugeLane *lane)
{
    if (!definition || definition_lane_is_empty(lane))
        return 0;

    u16 totalCells = 0;
    for (u8 segmentIndex = 0; segmentIndex < GAUGE_MAX_SEGMENTS; segmentIndex++)
    {
        const GaugeSegment *segment = &lane->segments[segmentIndex];
        if (segment->cells == 0 || !segment->skin)
            break;

        u16 displayCells = 0;
        if (!compute_segment_display_cells(definition->mode, segment, &displayCells))
            return 0;

        totalCells = (u16)(totalCells + displayCells);
    }

    return totalCells;
}

static s16 compute_lane_origin_axis(u16 base, s8 offset)
{
    return (s16)base + (s16)offset;
}

static u8 find_baseLane_index(const GaugeDefinition *definition,
                              u8 *outLaneIndex)
{
    if (!definition || !outLaneIndex)
        return 0;

    u16 bestSpan = 0;
    u8 bestIndex = 0xFF;

    for (u8 laneIndex = 0; laneIndex < GAUGE_MAX_LANES; laneIndex++)
    {
        const GaugeLane *lane = &definition->lanes[laneIndex];
        if (definition_lane_is_empty(lane))
            continue;

        const u16 span = compute_lane_display_cells(definition, lane);
        if (span > bestSpan || bestIndex == 0xFF)
        {
            bestSpan = span;
            bestIndex = laneIndex;
        }
    }

    if (bestIndex == 0xFF)
        return 0;

    *outLaneIndex = bestIndex;
    return 1;
}

static u16 compute_fill_offset_pixels_for_lane(const GaugeDefinition *definition,
                                               const GaugeLane *baseLane,
                                               u8 firstValueCell)
{
    if (!definition || !baseLane || firstValueCell == 0)
        return 0;

    u8 remainingCells = firstValueCell;
    u16 displayCellsBefore = 0;

    for (u8 segmentIndex = 0; segmentIndex < GAUGE_MAX_SEGMENTS; segmentIndex++)
    {
        const GaugeSegment *segment = &baseLane->segments[segmentIndex];
        if (segment->cells == 0 || !segment->skin)
            break;

        if (remainingCells == 0)
            break;

        if (remainingCells >= segment->cells)
        {
            u16 displayCells = 0;
            if (!compute_segment_display_cells(definition->mode, segment, &displayCells))
                return 0;
            displayCellsBefore = (u16)(displayCellsBefore + displayCells);
            remainingCells = (u8)(remainingCells - segment->cells);
        }
        else
        {
            if (definition->mode == GAUGE_MODE_PIP)
            {
                const u8 pipWidth = segment->skin->pip.pipWidth ? segment->skin->pip.pipWidth : 1;
                displayCellsBefore = (u16)(displayCellsBefore + ((u16)remainingCells * pipWidth));
            }
            else
            {
                displayCellsBefore = (u16)(displayCellsBefore + remainingCells);
            }
            remainingCells = 0;
            break;
        }
    }

    return (u16)(displayCellsBefore << TILE_TO_PIXEL_SHIFT);
}

static u8 resolve_lane_palette(const GaugeDefinition *definition,
                               const GaugeLane *lane)
{
    if (!definition || !lane)
        return 0;

    if (lane->overridePalette)
        return sanitize_palette_index(lane->palette);

    return sanitize_palette_index(definition->palette);
}

static u8 resolve_pip_strip_geometry_from_skin(const GaugePipSkin *skin,
                                               u8 *outStateCount,
                                               u8 *outSourceWidth,
                                               u8 *outSourceHeight,
                                               u8 *outHalfAxis)
{
    if (!skin || !skin->tileset)
        return 0;

    const u8 pipWidth = skin->pipWidth ? skin->pipWidth : 1;
    u8 pipHeight = skin->pipHeight ? skin->pipHeight : 1;
    if (pipHeight == 0 || pipHeight > 4)
        return 0;

    const u16 pipTileCount = skin->tileset->numTile;
    if (pipTileCount == 0 || pipTileCount > 255)
        return 0;

    const u8 stripCoverage = skin->coverage;
    if (stripCoverage > GAUGE_STRIP_COVERAGE_QUARTER)
        return 0;

    u8 sourceWidth = pipWidth;
    u8 sourceHeight = pipHeight;
    u8 halfAxis = PIP_HALF_AXIS_HORIZONTAL;
    u16 tilesPerState = 0;

    if (stripCoverage == GAUGE_STRIP_COVERAGE_HALF)
    {
        if ((pipWidth & 1) || (pipHeight & 1))
            return 0;

        const u16 horizontalTilesPerState = (u16)pipWidth * (u16)(pipHeight >> 1);
        const u16 verticalTilesPerState = (u16)(pipWidth >> 1) * pipHeight;
        const u8 horizontalValid = (horizontalTilesPerState != 0) && ((pipTileCount % horizontalTilesPerState) == 0);
        const u8 verticalValid = (verticalTilesPerState != 0) && ((pipTileCount % verticalTilesPerState) == 0);

        if (!horizontalValid && !verticalValid)
            return 0;

        if (horizontalValid)
        {
            sourceWidth = pipWidth;
            sourceHeight = (u8)(pipHeight >> 1);
            halfAxis = PIP_HALF_AXIS_HORIZONTAL;
            tilesPerState = horizontalTilesPerState;
        }
        else
        {
            sourceWidth = (u8)(pipWidth >> 1);
            sourceHeight = pipHeight;
            halfAxis = PIP_HALF_AXIS_VERTICAL;
            tilesPerState = verticalTilesPerState;
        }
    }
    else if (stripCoverage == GAUGE_STRIP_COVERAGE_QUARTER)
    {
        if ((pipWidth & 1) || (pipHeight & 1))
            return 0;

        sourceWidth = (u8)(pipWidth >> 1);
        sourceHeight = (u8)(pipHeight >> 1);
        tilesPerState = (u16)sourceWidth * sourceHeight;
    }
    else
    {
        tilesPerState = (u16)pipWidth * pipHeight;
    }

    if (tilesPerState == 0)
        return 0;
    if ((pipTileCount % tilesPerState) != 0)
        return 0;

    const u16 stateCount = (u16)(pipTileCount / tilesPerState);
    if (stateCount < 2 || stateCount > 255)
        return 0;

    if (outStateCount)
        *outStateCount = (u8)stateCount;
    if (outSourceWidth)
        *outSourceWidth = sourceWidth;
    if (outSourceHeight)
        *outSourceHeight = sourceHeight;
    if (outHalfAxis)
        *outHalfAxis = halfAxis;
    return 1;
}

static void release_built_layout(GaugeLaneLayout **layoutPtr)
{
    if (!layoutPtr || !*layoutPtr)
        return;

    GaugeLaneLayout *layout = *layoutPtr;
    if (layout->length != 0 || layout->refCount != 0)
        GaugeLaneLayout_release(layout);
    gauge_free_ptr((void **)layoutPtr);
}

static void release_built_layouts(GaugeLaneLayout **builtLayouts)
{
    if (!builtLayouts)
        return;

    for (u8 laneIndex = 0; laneIndex < GAUGE_MAX_LANES; laneIndex++)
        release_built_layout(&builtLayouts[laneIndex]);
}

static u8 build_segment_id_by_cell_from_lane(GaugeMode mode,
                                             GaugeFillDirection fillDirection,
                                             const GaugeLane *lane,
                                             u8 *outSegmentIdByCell,
                                             u8 *outLength,
                                             u8 *outSegmentCount)
{
    if (!lane || !outSegmentIdByCell || !outLength || !outSegmentCount)
        return 0;

    u8 segmentByFillIndex[GAUGE_MAX_LENGTH] = {0};
    u8 cursor = 0;
    u8 segmentCount = 0;

    for (u8 segmentIndex = 0; segmentIndex < GAUGE_MAX_SEGMENTS; segmentIndex++)
    {
        const GaugeSegment *segment = &lane->segments[segmentIndex];
        if (segment->cells == 0 || !segment->skin)
            break;

        u16 expandedCellCount = segment->cells;
        if (mode == GAUGE_MODE_PIP)
        {
            const GaugePipSkin *pip = &segment->skin->pip;
            if (!pip->tileset)
                return 0;
            expandedCellCount = (u16)(segment->cells * (pip->pipWidth ? pip->pipWidth : 1));
        }

        if ((u16)cursor + expandedCellCount > GAUGE_MAX_LENGTH)
            return 0;

        for (u16 i = 0; i < expandedCellCount; i++)
            segmentByFillIndex[cursor++] = segmentCount;

        segmentCount++;
    }

    if (cursor == 0 || segmentCount == 0)
        return 0;

    *outLength = cursor;
    *outSegmentCount = segmentCount;

    for (u8 cellIndex = 0; cellIndex < cursor; cellIndex++)
    {
        const u8 fillIndex = (fillDirection == GAUGE_FILL_REVERSE)
            ? (u8)(cursor - 1 - cellIndex)
            : cellIndex;
        outSegmentIdByCell[cellIndex] = segmentByFillIndex[fillIndex];
    }

    return 1;
}

static inline const u32 *tileset_asset_to_rom(const TileSet *tileset)
{
    return tileset ? tileset->tiles : NULL;
}

typedef struct
{
    const u32 *body;
    const u32 *end;
    const u32 *trail;
    const u32 *bridge;
    const u32 *capStart;
    const u32 *capEnd;
    const u32 *capStartBreak;
    const u32 *capStartTrail;
} ResolvedFillAssets;

typedef struct
{
    const u32 *tiles;
    u8 pipWidth;
    u8 pipHeight;
    u8 stateCount;
    u8 coverage;
    u8 sourceWidth;
    u8 sourceHeight;
    u8 halfAxis;
} ResolvedPipStyle;

static inline void resolve_fill_assets(const GaugeFillAssets *assets,
                                       u8 fixedStartCap,
                                       u8 fixedEndCap,
                                       ResolvedFillAssets *resolved)
{
    if (!resolved)
        return;

    resolved->body = assets ? tileset_asset_to_rom(assets->body) : NULL;
    resolved->end = assets ? tileset_asset_to_rom(assets->end) : NULL;
    resolved->trail = assets ? tileset_asset_to_rom(assets->trail) : NULL;
    resolved->bridge = assets ? tileset_asset_to_rom(assets->bridge) : NULL;
    resolved->capStart = fixedStartCap ? (resolved->end ? resolved->end : resolved->body) : NULL;
    resolved->capEnd = fixedEndCap ? (resolved->end ? resolved->end : resolved->body) : NULL;
    resolved->capStartBreak = fixedStartCap ? resolved->body : NULL;
    resolved->capStartTrail = fixedStartCap ? resolved->trail : NULL;
}

static inline const u32 *resolved_fill_asset_for_slot(const ResolvedFillAssets *resolved,
                                                      LayoutTilesetSlot slot)
{
    return (!resolved || slot >= LAYOUT_TILESET_SLOT_COUNT)
        ? NULL
        : ((const u32 * const *)resolved)[slot];
}

static void scan_fill_assets_usage(const ResolvedFillAssets *resolved,
                                   SkinSetUsageFlags *usageFlags)
{
    if (!usageFlags)
        return;

    for (u8 slot = 0; slot < LAYOUT_TILESET_SLOT_COUNT; slot++)
        ((u8 *)usageFlags)[slot] |= (resolved_fill_asset_for_slot(resolved, (LayoutTilesetSlot)slot) != NULL);
}

static inline const GaugeFillAssets *resolve_fill_assets_for_group(const GaugeFillSkin *skin,
                                                                   LayoutTilesetGroupId group)
{
    if (!skin)
        return NULL;
    switch (group)
    {
    case LAYOUT_TILESET_GROUP_GAIN:
        return &skin->gain;
    case LAYOUT_TILESET_GROUP_BLINK:
    case LAYOUT_TILESET_GROUP_GAIN_BLINK:
        return &skin->blinkOff;
    case LAYOUT_TILESET_GROUP_BASE:
    default:
        return &skin->normal;
    }
}

static const LayoutTilesetGroupId s_fillTilesetGroups[4] = {
    LAYOUT_TILESET_GROUP_BASE,
    LAYOUT_TILESET_GROUP_GAIN,
    LAYOUT_TILESET_GROUP_BLINK,
    LAYOUT_TILESET_GROUP_GAIN_BLINK
};

static void assign_layout_tileset_group_slot(GaugeLaneLayout *layout,
                                             LayoutTilesetGroupId group,
                                             LayoutTilesetSlot slot,
                                             u8 segmentId,
                                             const u32 *tileset)
{
    if (!layout)
        return;

    const u32 ***targetField = layout_get_group_slot_target(layout, group, slot);
    if (!targetField || !*targetField)
        return;

    if (*targetField == s_nullSegmentTilesets && slot != LAYOUT_TILESET_SLOT_BODY)
        return;

    (*targetField)[segmentId] = tileset;
}

static void populate_layout_tileset_group_from_fill_assets(GaugeLaneLayout *layout,
                                                           LayoutTilesetGroupId group,
                                                           u8 segmentId,
                                                           const ResolvedFillAssets *resolved)
{
    for (u8 slot = 0; slot < LAYOUT_TILESET_SLOT_COUNT; slot++)
    {
        assign_layout_tileset_group_slot(layout,
                                         group,
                                         (LayoutTilesetSlot)slot,
                                         segmentId,
                                         resolved_fill_asset_for_slot(resolved,
                                                                      (LayoutTilesetSlot)slot));
    }
}

static inline u8 resolve_pip_style(const GaugePipSkin *pip, ResolvedPipStyle *resolved)
{
    if (!pip || !resolved)
        return 0;

    resolved->tiles = pip->tileset ? pip->tileset->tiles : NULL;
    resolved->pipWidth = pip->pipWidth ? pip->pipWidth : 1;
    resolved->pipHeight = pip->pipHeight ? pip->pipHeight : 1;
    resolved->coverage = pip->coverage;
    resolved->sourceWidth = 1;
    resolved->sourceHeight = 1;
    resolved->halfAxis = PIP_HALF_AXIS_HORIZONTAL;

    return resolve_pip_strip_geometry_from_skin(pip,
                                                &resolved->stateCount,
                                                &resolved->sourceWidth,
                                                &resolved->sourceHeight,
                                                &resolved->halfAxis);
}

static inline void scan_pip_style_usage(const ResolvedPipStyle *resolved,
                                        u8 *hasPipStyles,
                                        u8 *hasCustomWidths,
                                        u8 *hasCustomHeights,
                                        u8 *hasCustomCoverage,
                                        u8 *hasCustomHalfAxis,
                                        u8 *hasCustomSourceWidths,
                                        u8 *hasCustomSourceHeights)
{
    if (!resolved)
        return;

    *hasPipStyles = 1;
    *hasCustomWidths |= (resolved->pipWidth > 1);
    *hasCustomHeights |= (resolved->pipHeight > 1);
    *hasCustomCoverage |= (resolved->coverage != GAUGE_STRIP_COVERAGE_FULL);
    *hasCustomHalfAxis |= (resolved->halfAxis != PIP_HALF_AXIS_HORIZONTAL);
    *hasCustomSourceWidths |= (resolved->sourceWidth > 1);
    *hasCustomSourceHeights |= (resolved->sourceHeight > 1);
}

static inline void assign_pip_style_to_layout(GaugeLaneLayout *layout,
                                              u8 segmentIndex,
                                              const ResolvedPipStyle *resolved)
{
    u8 coverage = resolved->coverage;
    if (coverage > GAUGE_STRIP_COVERAGE_QUARTER)
        coverage = GAUGE_STRIP_COVERAGE_FULL;

    if (layout->pipTilesetBySegment != s_nullSegmentTilesets)
        layout->pipTilesetBySegment[segmentIndex] = resolved->tiles;
    if (layout->pipWidthBySegment != s_oneSegmentFlags)
        layout->pipWidthBySegment[segmentIndex] = resolved->pipWidth;
    if (layout->pipHeightBySegment != s_oneSegmentFlags)
        layout->pipHeightBySegment[segmentIndex] =
            (resolved->pipHeight > 4) ? 4 : resolved->pipHeight;
    if (layout->pipStateCountBySegment != s_zeroSegmentFlags)
        layout->pipStateCountBySegment[segmentIndex] = resolved->stateCount;
    if (layout->pipStripCoverageBySegment != s_zeroSegmentFlags)
        layout->pipStripCoverageBySegment[segmentIndex] = coverage;
    if (layout->pipHalfAxisBySegment != s_zeroSegmentFlags)
        layout->pipHalfAxisBySegment[segmentIndex] = resolved->halfAxis;
    if (layout->pipSourceWidthBySegment != s_oneSegmentFlags)
        layout->pipSourceWidthBySegment[segmentIndex] = resolved->sourceWidth;
    if (layout->pipSourceHeightBySegment != s_oneSegmentFlags)
        layout->pipSourceHeightBySegment[segmentIndex] = resolved->sourceHeight;
}

static u8 init_layout_from_lane_plan(const GaugeDefinition *definition,
                                     const GaugeBuildLanePlan *lanePlan,
                                     GaugeLaneLayout *layout,
                                     const u32 * const *bodyTilesets)
{
    GaugeLaneLayout_initEx(layout,
                           lanePlan->length,
                           definition->fillDirection,
                           bodyTilesets,
                           NULL,
                           NULL,
                           NULL,
                           lanePlan->segmentIdByCell,
                           definition->orientation,
                           lanePlan->palette,
                           definition->priority,
                           definition->verticalFlip,
                           definition->horizontalFlip);

    return (layout->length != 0 && layout->segmentCount != 0) ? 1 : 0;
}

static u8 build_fill_layout_direct(const GaugeDefinition *definition,
                                   const GaugeBuildLanePlan *lanePlan,
                                   GaugeLaneLayout *layout)
{
    const u8 segmentCount = lanePlan->segmentCount;
    const u32 *baseBodyTilesets[GAUGE_MAX_SEGMENTS] = {0};
    ResolvedFillAssets resolvedAssets;
    SkinSetUsageFlags groupUsage[GAUGE_ARRAY_LEN(s_fillTilesetGroups)];
    const u8 capEndFlags = definition->fixedEndCap ? 1 : 0;

    memset(groupUsage, 0, sizeof(groupUsage));

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
    {
        const GaugeSegment *segment = &lanePlan->lane->segments[segmentIndex];
        if (!segment->skin || !segment->skin->fill.normal.body)
            return 0;

        baseBodyTilesets[segmentIndex] = segment->skin->fill.normal.body->tiles;
        for (u8 groupIndex = 0; groupIndex < GAUGE_ARRAY_LEN(s_fillTilesetGroups); groupIndex++)
        {
            resolve_fill_assets(resolve_fill_assets_for_group(&segment->skin->fill,
                                                              s_fillTilesetGroups[groupIndex]),
                                definition->fixedStartCap,
                                definition->fixedEndCap,
                                &resolvedAssets);
            scan_fill_assets_usage(&resolvedAssets, &groupUsage[groupIndex]);
        }
    }

    if (!init_layout_from_lane_plan(definition, lanePlan, layout, baseBodyTilesets))
        return 0;

    sync_base_allocations(layout, &groupUsage[0], capEndFlags);
    for (u8 groupIndex = 1; groupIndex < GAUGE_ARRAY_LEN(s_fillTilesetGroups); groupIndex++)
    {
        sync_layout_tileset_group_slots_from_usage(layout,
                                                   s_fillTilesetGroups[groupIndex],
                                                   &groupUsage[groupIndex],
                                                   LAYOUT_TILESET_SLOT_MASK_ALL);
    }

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
    {
        const GaugeFillSkin *skin = &lanePlan->lane->segments[segmentIndex].skin->fill;

        for (u8 groupIndex = 0; groupIndex < GAUGE_ARRAY_LEN(s_fillTilesetGroups); groupIndex++)
        {
            resolve_fill_assets(resolve_fill_assets_for_group(skin,
                                                              s_fillTilesetGroups[groupIndex]),
                                definition->fixedStartCap,
                                definition->fixedEndCap,
                                &resolvedAssets);
            populate_layout_tileset_group_from_fill_assets(layout,
                                                           s_fillTilesetGroups[groupIndex],
                                                           segmentIndex,
                                                           &resolvedAssets);
        }

        if (layout->capEndBySegment != s_zeroSegmentFlags)
            layout->capEndBySegment[segmentIndex] = definition->fixedEndCap ? 1 : 0;
    }

    finalize_layout_derived_state(layout);
    return 1;
}

static u8 build_pip_layout_direct(const GaugeDefinition *definition,
                                  const GaugeBuildLanePlan *lanePlan,
                                  GaugeLaneLayout *layout)
{
    const u8 segmentCount = lanePlan->segmentCount;
    ResolvedPipStyle resolvedStyles[GAUGE_MAX_SEGMENTS];
    u8 hasPipStyles = 0;
    u8 hasCustomWidths = 0;
    u8 hasCustomHeights = 0;
    u8 hasCustomCoverage = 0;
    u8 hasCustomHalfAxis = 0;
    u8 hasCustomSourceWidths = 0;
    u8 hasCustomSourceHeights = 0;

    memset(resolvedStyles, 0, sizeof(resolvedStyles));

    if (!init_layout_from_lane_plan(definition, lanePlan, layout, NULL))
        return 0;

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
    {
        const GaugeSegment *segment = &lanePlan->lane->segments[segmentIndex];
        if (!segment->skin)
            return 0;

        const GaugePipSkin *pip = &segment->skin->pip;
        ResolvedPipStyle *resolved = &resolvedStyles[segmentIndex];
        if (!resolve_pip_style(pip, resolved))
        {
            return 0;
        }

        scan_pip_style_usage(resolved,
                             &hasPipStyles,
                             &hasCustomWidths,
                             &hasCustomHeights,
                             &hasCustomCoverage,
                             &hasCustomHalfAxis,
                             &hasCustomSourceWidths,
                             &hasCustomSourceHeights);
    }

    layout_sync_optional_segment_tilesets_by_usage(
        hasPipStyles,
        &layout->pipTilesetBySegment,
        layout->segmentCount);
    layout_sync_optional_segment_flags(
        hasPipStyles && hasCustomWidths,
        &layout->pipWidthBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);
    layout_sync_optional_segment_flags(
        hasPipStyles && hasCustomHeights,
        &layout->pipHeightBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);
    layout_sync_optional_segment_flags(
        hasPipStyles,
        &layout->pipStateCountBySegment,
        layout->segmentCount,
        s_zeroSegmentFlags,
        0);
    layout_sync_optional_segment_flags(
        hasPipStyles && hasCustomCoverage,
        &layout->pipStripCoverageBySegment,
        layout->segmentCount,
        s_zeroSegmentFlags,
        GAUGE_STRIP_COVERAGE_FULL);
    layout_sync_optional_segment_flags(
        hasPipStyles && hasCustomHalfAxis,
        &layout->pipHalfAxisBySegment,
        layout->segmentCount,
        s_zeroSegmentFlags,
        PIP_HALF_AXIS_HORIZONTAL);
    layout_sync_optional_segment_flags(
        hasPipStyles && hasCustomSourceWidths,
        &layout->pipSourceWidthBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);
    layout_sync_optional_segment_flags(
        hasPipStyles && hasCustomSourceHeights,
        &layout->pipSourceHeightBySegment,
        layout->segmentCount,
        s_oneSegmentFlags,
        1);

    for (u8 segmentIndex = 0; segmentIndex < segmentCount; segmentIndex++)
        assign_pip_style_to_layout(layout, segmentIndex, &resolvedStyles[segmentIndex]);

    finalize_layout_derived_state(layout);
    return 1;
}

static u8 build_lane_plan(const GaugeDefinition *definition,
                          u8 laneIndex,
                          const GaugeLane *lane,
                          const GaugeLane *baseLane,
                          GaugeBuildLanePlan *outPlan)
{
    if (!definition || !lane || !baseLane || !outPlan)
        return 0;

    memset(outPlan, 0, sizeof(*outPlan));

    const s16 originX = compute_lane_origin_axis(definition->originX, lane->offsetX);
    const s16 originY = compute_lane_origin_axis(definition->originY, lane->offsetY);
    if (originX < 0 || originY < 0)
        return 0;

    outPlan->lane = lane;
    outPlan->laneIndex = laneIndex;
    outPlan->originX = (u16)originX;
    outPlan->originY = (u16)originY;
    outPlan->fillOffset = compute_fill_offset_pixels_for_lane(definition,
                                                              baseLane,
                                                              lane->firstValueCell);
    outPlan->palette = resolve_lane_palette(definition, lane);

    if (!build_segment_id_by_cell_from_lane(definition->mode,
                                            definition->fillDirection,
                                            lane,
                                            outPlan->segmentIdByCell,
                                            &outPlan->length,
                                            &outPlan->segmentCount))
    {
        return 0;
    }

    if (definition->orientation == GAUGE_ORIENT_VERTICAL &&
        outPlan->originY < (u16)(outPlan->length - 1))
    {
        return 0;
    }

    return 1;
}

static u8 build_layout_from_lane_plan(const GaugeDefinition *definition,
                                      const GaugeBuildLanePlan *lanePlan,
                                      GaugeLaneLayout **outLayout)
{
    if (!definition || !lanePlan || !outLayout)
        return 0;

    GaugeLaneLayout *layout = (GaugeLaneLayout *)gauge_alloc_bytes((u16)sizeof(GaugeLaneLayout));
    if (!layout)
        return 0;

    const u8 buildOk = (definition->mode == GAUGE_MODE_FILL)
        ? build_fill_layout_direct(definition, lanePlan, layout)
        : build_pip_layout_direct(definition, lanePlan, layout);

    if (!buildOk)
    {
        release_built_layout(&layout);
        return 0;
    }

    if (lanePlan->fillOffset != 0)
        GaugeLaneLayout_setFillOffset(layout, lanePlan->fillOffset);

    *outLayout = layout;
    return 1;
}

/**
 * FILL-mode update path: tick logic + render all lanes.
 * Call once per frame in the game loop.
 *
 * Execution flow:
 * 1. logicTickHandler() - advance value animation + trail mode state
 * 2. Compute render state (valuePixels, trailPixelsRendered, blinkOn)
 * 3. Change detection: compare against lastValuePixels, lastTrailPixelsRendered,
 *    lastBlinkOn, and check if value animation is still converging
 * 4. If nothing changed -> early return (zero CPU cost after initial comparison)
 * 5. If changed -> render all lanes via lane->renderHandler
 *
 * The change detection cache (lastValuePixels, lastTrailPixelsRendered, lastBlinkOn)
 * is initialized to CACHE_INVALID_U16/U8 to force the first render.
 *
 * When trail is disabled, trailPixelsRendered == valuePixels naturally
 * (enforced by logic tick), so the blink/trail paths are no-ops.
 *
 * Cost: ~20 cycles (early return) to ~2000+ cycles (full render with DMA)
 */

/* --- Shared tick-and-render helpers (fill & pip) --- */

/** Blink and trail mode state, computed once per tick. */
typedef struct {
    u8 blinkOn;
    u8 blinkOffActive;
    u8 blinkOnChanged;
    u8 useValueBlinkRendering;
    u8 trailMode;
    u8 trailModeChanged;
} BlinkState;

/**
 * Compute blink and trail mode state from logic timer state.
 * Shared between gauge_tick_and_render_fill() and gauge_tick_and_render_pip().
 */
static inline BlinkState compute_blink_state(const GaugeLogic *logic)
{
    BlinkState s;
    const u8 activeBlinkShift =
        (logic->activeTrailState == GAUGE_TRAIL_STATE_GAIN &&
         logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW)
        ? logic->gainBlinkShift
        : logic->blinkShift;
    const u8 criticalModeActive =
        logic->modeUsesCriticalThreshold ? is_critical_mode_active(logic) : 0;

    s.blinkOn = 1;
    if (logic->blinkFramesRemaining > 0)
    {
        /* Toggle blink on/off at a rate controlled by blinkShift.
         * blinkTimer increments each frame; shifting right by blinkShift
         * divides by 2^blinkShift, so bit 0 of the result toggles every
         * 2^blinkShift frames.  blinkShift=1 -> toggle every 2 frames,
         * blinkShift=2 -> every 4 frames, etc. */
        s.blinkOn = (u8)(((logic->blinkTimer >> activeBlinkShift) & 1) == 0);
    }
    s.blinkOnChanged = (logic->lastBlinkOn != s.blinkOn);

    s.useValueBlinkRendering =
        (logic->activeTrailState != GAUGE_TRAIL_STATE_GAIN &&
         logic->modeUsesValueBlink &&
         criticalModeActive) ? 1 : 0;

    s.trailMode = logic->activeTrailState;
    if (s.useValueBlinkRendering)
        s.trailMode = GAUGE_TRAIL_STATE_DAMAGE;
    s.trailModeChanged = (logic->lastActiveTrailState != s.trailMode);
    s.blinkOffActive = ((logic->trailEnabled || s.useValueBlinkRendering) &&
                        logic->blinkFramesRemaining > 0 &&
                        s.blinkOn == 0);
    return s;
}

static inline u8 compute_pip_blink_off_active(const GaugeLogic *logic,
                                              const BlinkState *blinkState)
{
    if (!logic || !blinkState || !blinkState->blinkOffActive)
        return 0;

    if (blinkState->trailMode == GAUGE_TRAIL_STATE_GAIN)
        return (logic->configuredGainMode == GAUGE_GAIN_MODE_FOLLOW) ? 1 : 0;

    if (blinkState->useValueBlinkRendering)
        return 0;

    if (blinkState->trailMode == GAUGE_TRAIL_STATE_DAMAGE)
        return (logic->configuredTrailMode == GAUGE_TRAIL_MODE_FOLLOW) ? 1 : 0;

    return 0;
}

typedef struct
{
    BlinkState blinkState;
    u16 valuePixelsRaw;
    u16 trailPixelsRaw;
    u16 valuePixels;
    u16 trailPixelsRendered;
    u16 trailPixelsActual;
    u16 valueTargetForEarlyReturn;
    u8 blinkOffActive;
} GaugeRenderFrame;

static inline u8 gauge_prepare_frame_common(Gauge *gauge,
                                            GaugeRenderFrame *frame)
{
    GaugeLogic *logic = &gauge->logic;

    if (!logic->needUpdate)
        return 0;

    if (gauge->logicTickHandler)
        gauge->logicTickHandler(logic);

    frame->valuePixelsRaw = logic->valuePixels;
    frame->trailPixelsRaw = logic->trailPixels;
    if (frame->trailPixelsRaw < frame->valuePixelsRaw)
        frame->trailPixelsRaw = frame->valuePixelsRaw;

    frame->blinkState = compute_blink_state(logic);
    return 1;
}

static void gauge_render_prepared_frame(Gauge *gauge,
                                        const GaugeRenderFrame *frame)
{
#if GAUGE_ENABLE_TRACE
    s_traceContext.active = 0;
    s_traceContext.laneIndex = 0;
    if (gauge && gauge->debugMode)
    {
        s_traceContext.active = 1;
        kprintf(
            "GAUGE_TRACE FRAME valuePixels=%u trailRendered=%u trailActual=%u trailMode=%s state=%s\n",
            (unsigned int)frame->valuePixels,
            (unsigned int)frame->trailPixelsRendered,
            (unsigned int)frame->trailPixelsActual,
            trace_text_lookup(s_trailModeText,
                              GAUGE_ARRAY_LEN(s_trailModeText),
                              frame->blinkState.trailMode,
                              "NONE"),
            render_state_to_text(frame->blinkState.trailMode,
                                 frame->blinkOffActive,
                                 frame->valuePixels,
                                 frame->trailPixelsRendered)
        );
    }
#endif

    u8 laneIndex = gauge->laneCount;
    while (laneIndex--)
    {
        GaugeLaneInstance *lane = gauge->lanes[laneIndex];
        if (!lane)
            continue;
#if GAUGE_ENABLE_TRACE
        s_traceContext.laneIndex = laneIndex;
#endif
        lane->renderHandler(lane,
                            frame->valuePixels,
                            frame->trailPixelsRendered,
                            frame->trailPixelsActual,
                            frame->blinkOffActive,
                            frame->blinkState.blinkOnChanged,
                            frame->blinkState.trailMode,
                            frame->blinkState.trailModeChanged);
    }

#if GAUGE_ENABLE_TRACE
    s_traceContext.active = 0;
    s_traceContext.laneIndex = 0;
#endif
}

/**
 * Update render cache, check for early return, and detect idle state.
 *
 * @param valueTargetForEarlyReturn  Quantized target for pip, logic->valueTargetPixels for fill.
 * @param valueForIdle               Raw valuePixels for pip idle detect, valuePixels for fill.
 * @param trailForIdle               Raw trailPixels for pip idle detect, trailPixels for fill.
 * @return 1 if nothing changed (caller should return early), 0 if render needed.
 */
static inline u8 update_render_cache_and_check(
    GaugeLogic *logic,
    u16 valuePixels, u16 trailPixelsRendered,
    const BlinkState *bs,
    u16 valueTargetForEarlyReturn,
    u16 valueForIdle, u16 trailForIdle)
{
    const u8 criticalModeActive =
        logic->modeUsesCriticalThreshold ? is_critical_mode_active(logic) : 0;

    if (logic->lastValuePixels == valuePixels &&
        logic->lastTrailPixelsRendered == trailPixelsRendered &&
        logic->lastBlinkOn == bs->blinkOn &&
        valueTargetForEarlyReturn == valuePixels &&
        !bs->trailModeChanged)
    {
        return 1;
    }

    logic->lastValuePixels = valuePixels;
    logic->lastTrailPixelsRendered = trailPixelsRendered;
    logic->lastBlinkOn = bs->blinkOn;
    logic->lastActiveTrailState = bs->trailMode;

    /* Detect fully idle state: value at target and no active timed work.
     * We still render this frame, then next Gauge_update returns via needUpdate. */
    if (!criticalModeActive &&
        valueForIdle == logic->valueTargetPixels &&
        logic->holdFramesRemaining == 0 &&
        logic->blinkFramesRemaining == 0)
    {
        if (logic->modeKeepsStaticTrail)
        {
            logic->needUpdate = 0;
        }
        else if (trailForIdle == valueForIdle &&
                 logic->activeTrailState == GAUGE_TRAIL_STATE_NONE)
        {
            logic->needUpdate = 0;
        }
    }

    return 0;
}

static inline void gauge_prepare_fill_frame(const GaugeLogic *logic,
                                            GaugeRenderFrame *frame)
{
    const BlinkState *blinkState = &frame->blinkState;

    frame->valuePixels = frame->valuePixelsRaw;
    frame->trailPixelsRendered = blinkState->blinkOn
        ? frame->trailPixelsRaw
        : frame->valuePixelsRaw;
    frame->trailPixelsActual = frame->trailPixelsRaw;
    frame->valueTargetForEarlyReturn = logic->valueTargetPixels;
    frame->blinkOffActive = blinkState->blinkOffActive;

    if (blinkState->useValueBlinkRendering && !blinkState->blinkOn)
    {
        frame->valuePixels = 0;
        frame->trailPixelsRendered = frame->valuePixelsRaw;
        frame->trailPixelsActual = frame->valuePixelsRaw;
        frame->valueTargetForEarlyReturn = 0;
    }
}

static void gauge_tick_and_render_fill(Gauge *gauge)
{
    GaugeRenderFrame frame;
    GaugeLogic *logic = &gauge->logic;

    if (!gauge_prepare_frame_common(gauge, &frame))
        return;

    gauge_prepare_fill_frame(logic, &frame);

    if (update_render_cache_and_check(logic,
                                      frame.valuePixels,
                                      frame.trailPixelsRendered,
                                      &frame.blinkState,
                                      frame.valueTargetForEarlyReturn,
                                      frame.valuePixelsRaw,
                                      frame.trailPixelsRaw))
        return;

    gauge_build_baseLane_fill_decisions(gauge,
                                        frame.valuePixels,
                                        frame.trailPixelsRendered,
                                        frame.trailPixelsActual,
                                        frame.blinkOffActive,
                                        frame.blinkState.trailMode);
    gauge_render_prepared_frame(gauge, &frame);
}

/**
 * Quantize a pixel position to the nearest PIP step boundary.
 *
 * Fast path:
 *   direct lookup in the inverse PIP LUT built at Gauge_build() time
 *
 * Fallback:
 *   scan the direct valueToPixelsLUT if the inverse LUT could not be allocated
 *
 * @param logic   Logic with valueToPixelsLUT and maxValue
 * @param pixels  Raw pixel value to quantize
 * @return Quantized pixel value aligned to a pip boundary
 */
static inline u16 quantize_pixels_to_pip_step(const GaugeLogic *logic, u16 pixels)
{
    if (pixels >= logic->maxFillPixels)
        return logic->maxFillPixels;

    if (logic->pixelsToQuantizedPixelsLUT)
        return logic->pixelsToQuantizedPixelsLUT[pixels];

    const u16 *lut = logic->valueToPixelsLUT;
    if (!lut)
    {
        /* Fallback (should not happen in strict PIP config): tile-aligned quantization. */
        return (u16)((pixels >> TILE_TO_PIXEL_SHIFT) << TILE_TO_PIXEL_SHIFT);
    }

    u16 value = 0;
    while (value < logic->maxValue && lut[value + 1] <= pixels)
        value++;

    return lut[value];
}

static inline void gauge_prepare_pip_frame(const GaugeLogic *logic,
                                           GaugeRenderFrame *frame)
{
    const BlinkState *blinkState = &frame->blinkState;
    const u16 valuePixelsQuantized =
        quantize_pixels_to_pip_step(logic, frame->valuePixelsRaw);
    u16 trailPixels =
        quantize_pixels_to_pip_step(logic, frame->trailPixelsRaw);

    if (trailPixels < valuePixelsQuantized)
        trailPixels = valuePixelsQuantized;

    frame->valuePixels = valuePixelsQuantized;
    frame->trailPixelsRendered = blinkState->blinkOn
        ? trailPixels
        : valuePixelsQuantized;
    frame->trailPixelsActual = trailPixels;
    frame->valueTargetForEarlyReturn =
        quantize_pixels_to_pip_step(logic, logic->valueTargetPixels);
    frame->blinkOffActive = compute_pip_blink_off_active(logic, blinkState);

    if (blinkState->useValueBlinkRendering && blinkState->blinkOn)
    {
        frame->valuePixels = 0;
        frame->trailPixelsRendered = valuePixelsQuantized;
        frame->trailPixelsActual = valuePixelsQuantized;
        frame->valueTargetForEarlyReturn = 0;
    }
}

/**
 * PIP-mode update path: tick logic + render all lanes.
 *
 * Same flow as fill mode, but pixel values are quantized to pip boundaries
 * before rendering. This ensures each pip snaps to full/empty states rather
 * than showing partial fills. Uses the inverse PIP LUT when available, with a
 * fallback scan on the direct LUT if allocation failed.
 */
static void gauge_tick_and_render_pip(Gauge *gauge)
{
    GaugeRenderFrame frame;
    GaugeLogic *logic = &gauge->logic;

    if (!gauge_prepare_frame_common(gauge, &frame))
        return;

    gauge_prepare_pip_frame(logic, &frame);

    if (update_render_cache_and_check(logic,
                                      frame.valuePixels,
                                      frame.trailPixelsRendered,
                                      &frame.blinkState,
                                      frame.valueTargetForEarlyReturn,
                                      frame.valuePixelsRaw,
                                      frame.trailPixelsRaw))
        return;

    gauge_build_baseLane_pip_states(gauge,
                                    frame.valuePixels,
                                    frame.trailPixelsRendered,
                                    frame.trailPixelsActual,
                                    frame.blinkOffActive,
                                    frame.blinkState.trailMode);
    gauge_render_prepared_frame(gauge, &frame);
}
/* =============================================================================
   Utility functions
   ============================================================================= */

/**
 * Compute how many VRAM tiles are needed for a layout in fixed-only mode.
 *
 * VRAM budget:
 * - PIP:  1 tile per rendered physical PIP tile (height-expanded)
 * - FILL: 1 tile per cell with a valid tileset
 *
 * @return Number of VRAM tiles required
 */
static u16 compute_vram_size_for_layout(const GaugeLaneLayout *layout,
                                        u8 trailEnabled,
                                        GaugeValueMode valueMode)
{
    (void)trailEnabled;

    if (valueMode == GAUGE_VALUE_MODE_PIP)
        return layout->pipRenderCount;

    u16 count = 0;
    for (u8 i = 0; i < layout->length; i++)
    {
        const u8 segmentId = layout->segmentIdByCell[i];
        if (layout->tilesetBySegment[segmentId] || layout->gainTilesetBySegment[segmentId])
            count++;
    }

    return count;
}

