#ifndef GAUGE_TEST_H
#define GAUGE_TEST_H

#include <genesis.h>
#include "gauge.h"

/* ============================================================================
   Gauge test module
   ----------------------------------------------------------------------------
   Provides:
   - Runtime-generated ROM strips (45 tiles per strip) for:
       * Yellow gauge (horizontal + vertical)
       * Blue thin gauge (horizontal + vertical)
   - Base layouts for 3 examples:
       Example 1: Yellow, 1 lane, length 12
       Example 2: Yellow, 2 lanes, length 12
       Example 3: Yellow, first 4 cells = 2 lanes, next 8 cells = 1 lane
                 + Blue thin gauge under/aside the 1-lane part (length 8)

   Notes:
   - Yellow = value pixels (yellow) + trail pixels (red, blinking on damage).
   - Blue gauge wraps automatically 0..100% (no trail/blink usage; trail==value).
   - Blue gauge thickness is 4 pixels (not "stuck" to yellow lane):
       * Horizontal: blue pixels only in bottom 4 rows of its tile (top 4 rows empty)
       * Vertical:   blue pixels only in right 4 columns of its tile (left 4 columns empty)
   ============================================================================ */

#define GAUGE_TEST_YELLOW_LENGTH_TILES      12
#define GAUGE_TEST_EX3_HEAD_2LANE_TILES      4
#define GAUGE_TEST_EX3_TAIL_1LANE_TILES      (GAUGE_TEST_YELLOW_LENGTH_TILES - GAUGE_TEST_EX3_HEAD_2LANE_TILES)
#define GAUGE_TEST_BLUE_LENGTH_TILES         GAUGE_TEST_EX3_TAIL_1LANE_TILES /* 8 */

/* A single background tile to visualize non-overwritten areas */
#define GAUGE_TEST_VRAM_BG_TILE  (TILE_USER_INDEX + 256)

/* Precomputed LUT: percent 0..100 -> pixels 0..(GAUGE_TEST_BLUE_LENGTH_TILES*8) */
const u16* GaugeTest_getBluePercentToPixelsLUT(void);

/* Call once at startup (palette + assets generation + background tile) */
void GaugeTest_init(void);

/* -------------------------
   ROM getters
   ------------------------- */
const GaugeRom* GaugeTest_getYellowRom(GaugeOrientation orientation);
const GaugeRom* GaugeTest_getBlueRom(GaugeOrientation orientation);

/* -------------------------
   Layout getters (base P1 layouts)
   ------------------------- */
const GaugeLayout* GaugeTest_getLayoutExample1(GaugeOrientation orientation); /* yellow 1 lane */
const GaugeLayout* GaugeTest_getLayoutExample2(GaugeOrientation orientation); /* yellow 2 lanes */
const GaugeLayout* GaugeTest_getLayoutExample3Yellow(GaugeOrientation orientation); /* yellow 2 lanes then 1 lane */
const GaugeLayout* GaugeTest_getLayoutExample3Blue(GaugeOrientation orientation);   /* blue gauge (length 8, 1 lane) */

#endif /* GAUGE_TEST_H */
