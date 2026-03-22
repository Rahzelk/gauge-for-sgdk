#include <genesis.h>
#include "showcase_internal.h"
#include "gauge_assets.h"

/* =============================================================================
   Gauge showcase data

   This file is the showcase's main teaching document.
   HUD descriptions stay intentionally short, so the comments below explain why
   each case exists, which fields deserve attention, and what visual result a
   junior developer should expect.

   Reading a GaugeDefinition:
   - top-level fields place the gauge and choose the renderer family
   - lanes / segments describe geometry and art ownership
   - behavior controls how value, damage, gain, and blink evolve over time
   ============================================================================= */

/* -----------------------------------------------------------------------------
   Reusable skins
   ----------------------------------------------------------------------------- */
/* Straight fill strips.
 * These are the simplest art families and make it easy to isolate runtime
 * behavior from stylized rendering details.
 */
static const GaugeSkin g_skinYellowStraight = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_yellow_strip
        }
    }
};

static const GaugeSkin g_skinYellowStraightGain = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_yellow_strip
        },
        .gain = {
            .body = &gauge_h_straight_yellow_strip_gain
        }
    }
};

static const GaugeSkin g_skinYellowStraightBlinkOff = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_yellow_strip
        },
        .blinkOff = {
            .body = &gauge_h_straight_yellow_strip_blinkoff
        }
    }
};

static const GaugeSkin g_skinBlueStraight = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_blue_strip
        }
    }
};

/* Small straight strip.
 * This asset intentionally exposes the minimum authored surface: one body strip
 * and no dedicated trail / end variants.
 */
static const GaugeSkin g_skinBlueStraightSmall = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_small_blue_strip
        }
    }
};

static const GaugeSkin g_skinLightBlueStraight = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_lightblue_strip
        }
    }
};

/* Bevel and bevelback strips.
 * These families are used to teach body / trail / end / bridge selection and
 * how gain strips can mirror the normal art family.
 */
static const GaugeSkin g_skinBevelLightBlue = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_lightblue_strip_body,
            .trail = &gauge_h_bevel_lightblue_strip_trail,
            .end = &gauge_h_bevel_lightblue_strip_end,
            .bridge = &gauge_h_bevel_lightblue_to_blue_strip_bridge
        },
        .gain = {
            .body = &gauge_h_bevel_lightblue_gain_strip_body,
            .trail = &gauge_h_bevel_lightblue_gain_strip_trail,
            .end = &gauge_h_bevel_lightblue_gain_strip_end,
            .bridge = &gauge_h_bevel_lightblue_gain_to_blue_strip_bridge
        }
    }
};

static const GaugeSkin g_skinBevelBlue = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_blue_strip_body,
            .trail = &gauge_h_bevel_blue_strip_trail,
            .end = &gauge_h_bevel_blue_strip_end,
            .bridge = &gauge_h_bevel_blue_to_yellow_strip_bridge
        },
        .gain = {
            .body = &gauge_h_bevel_blue_gain_strip_body,
            .trail = &gauge_h_bevel_blue_gain_strip_trail,
            .end = &gauge_h_bevel_blue_gain_strip_end,
            .bridge = &gauge_h_bevel_blue_to_yellow_gain_strip_bridge
        }
    }
};

static const GaugeSkin g_skinBevelYellow = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_yellow_strip_body,
            .trail = &gauge_h_bevel_yellow_strip_trail,
            .end = &gauge_h_bevel_yellow_strip_end
        },
        .gain = {
            .body = &gauge_h_bevel_yellow_gain_strip_body,
            .trail = &gauge_h_bevel_yellow_gain_strip_trail,
            .end = &gauge_h_bevel_yellow_gain_strip_end
        }
    }
};

static const GaugeSkin g_skinBevelbackLightBlue = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevelback_lightblue_strip_body,
            .trail = &gauge_h_bevelback_lightblue_strip_trail,
            .end = &gauge_h_bevelback_lightblue_strip_end,
            .bridge = &gauge_h_bevelback_lightblue_to_blue_strip_bridge
        },
        .gain = {
            .body = &gauge_h_bevelback_lightblue_gain_strip_body,
            .trail = &gauge_h_bevelback_lightblue_gain_strip_trail,
            .end = &gauge_h_bevelback_lightblue_gain_strip_end,
            .bridge = &gauge_h_bevelback_lightblue_gain_to_blue_strip_bridge
        }
    }
};

static const GaugeSkin g_skinBevelbackBlue = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevelback_blue_strip_body,
            .trail = &gauge_h_bevelback_blue_strip_trail,
            .end = &gauge_h_bevelback_blue_strip_end
        },
        .gain = {
            .body = &gauge_h_bevelback_blue_gain_strip_body,
            .trail = &gauge_h_bevelback_blue_gain_strip_trail,
            .end = &gauge_h_bevelback_blue_gain_strip_end
        }
    }
};

/* Border variants.
 * These skins exist to highlight fixed caps and blink-off art on the same
 * underlying fill logic.
 */
static const GaugeSkin g_skinBorderBlueCapStart = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_blue_with_border_cap_start_strip_body,
            .trail = &gauge_h_bevel_blue_with_border_cap_start_strip_trail,
            .end = &gauge_h_bevel_blue_with_border_cap_start_strip_end
        },
        .blinkOff = {
            .body = &gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_body,
            .trail = &gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_trail,
            .end = &gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_end
        }
    }
};

static const GaugeSkin g_skinBorderBlue = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_blue_with_border_strip_body,
            .trail = &gauge_h_bevel_blue_with_border_strip_trail,
            .end = &gauge_h_bevel_blue_with_border_strip_end,
            .bridge = &gauge_h_bevel_blue_to_yellow_with_border_strip_bridge
        },
        .blinkOff = {
            .body = &gauge_h_bevel_blue_with_border_blink_off_strip_body,
            .trail = &gauge_h_bevel_blue_with_border_blink_off_strip_trail,
            .end = &gauge_h_bevel_blue_with_border_blink_off_strip_end,
            .bridge = &gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge
        }
    }
};

static const GaugeSkin g_skinBorderYellowCapStart = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_yellow_with_border_cap_start_strip_body,
            .trail = &gauge_h_bevel_yellow_with_border_cap_start_strip_trail,
            .end = &gauge_h_bevel_yellow_with_border_cap_start_strip_end
        },
        .blinkOff = {
            .body = &gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_body,
            .trail = &gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail,
            .end = &gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end
        }
    }
};

static const GaugeSkin g_skinBorderYellow = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_yellow_with_border_strip_body,
            .trail = &gauge_h_bevel_yellow_with_border_strip_trail,
            .end = &gauge_h_bevel_yellow_with_border_strip_end
        },
        .blinkOff = {
            .body = &gauge_h_bevel_yellow_with_border_blink_off_strip_body,
            .trail = &gauge_h_bevel_yellow_with_border_blink_off_strip_trail,
            .end = &gauge_h_bevel_yellow_with_border_blink_off_strip_end
        }
    }
};

static const GaugeSkin g_skinBorderYellowCapEnd = {
    .fill = {
        .normal = {
            .body = &gauge_h_bevel_yellow_with_border_strip_body,
            .trail = &gauge_h_bevel_yellow_with_border_strip_trail,
            .end = &gauge_h_bevel_yellow_with_border_cap_end_strip_end
        },
        .blinkOff = {
            .body = &gauge_h_bevel_yellow_with_border_blink_off_strip_body,
            .trail = &gauge_h_bevel_yellow_with_border_blink_off_strip_trail,
            .end = &gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end
        }
    }
};

/* Vertical fill strips.
 * Vertical gauges keep the same Gauge behaviors but need dedicated art because
 * Mega Drive background tiles cannot be rotated at runtime.
 */
static const GaugeSkin g_skinVerticalBlue = {
    .fill = {
        .normal = {
            .body = &gauge_v_straight_blue_strip
        }
    }
};

static const GaugeSkin g_skinVerticalLightBlue = {
    .fill = {
        .normal = {
            .body = &gauge_v_straight_lightblue_strip
        }
    }
};

static const GaugeSkin g_skinVerticalYellow = {
    .fill = {
        .normal = {
            .body = &gauge_v_straight_yellow_strip
        }
    }
};

static const GaugeSkin g_skinVerticalYellowGainBlinkOff = {
    .fill = {
        .normal = {
            .body = &gauge_v_straight_yellow_strip
        },
        .gain = {
            .body = &gauge_v_straight_yellow_strip_gain
        },
        .blinkOff = {
            .body = &gauge_v_straight_yellow_strip_blinkoff
        }
    }
};

/* PIP skins.
 * These cases teach quantization and shared-source rendering in PIP mode,
 * including HALF and QUARTER mirroring.
 */
static const GaugeSkin g_skinPipDoubleHalf = {
    .pip = {
        .tileset = &gauge_h_pip_double_half_strip,
        .pipWidth = 2,
        .pipHeight = 1,
        .coverage = GAUGE_PIP_COVERAGE_HALF
    }
};

static const GaugeSkin g_skinPipMiniBar = {
    .pip = {
        .tileset = &gauge_h_pip_mini_bar_strip,
        .pipWidth = 2,
        .pipHeight = 1,
        .coverage = GAUGE_PIP_COVERAGE_FULL
    }
};

static const GaugeSkin g_skinPipDoubleQuarter = {
    .pip = {
        .tileset = &gauge_h_pip_double_quarter_strip,
        .pipWidth = 2,
        .pipHeight = 2,
        .coverage = GAUGE_PIP_COVERAGE_QUARTER
    }
};

static const GaugeSkin g_skinPipSingle = {
    .pip = {
        .tileset = &gauge_h_pip_strip,
        .pipWidth = 1,
        .pipHeight = 1,
        .coverage = GAUGE_PIP_COVERAGE_FULL
    }
};

/* -----------------------------------------------------------------------------
   Screen 1: basic fill horizontal

   Teaching goal:
   - establish the baseline mental model for fill gauges
   - separate direction, gain, blink, and lane composition
   - keep the art simple so the runtime rules stay easy to see
   ----------------------------------------------------------------------------- */
/* Single-lane baseline.
 * Read mode, orientation, fillDirection, and the one-lane / one-segment setup.
 * This is the first case to inspect when learning the module.
 */
static const GaugeDefinition g_screen1BasicSingleDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 12,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinYellowStraight }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

/* Multi-lane baseline with the same straight art family.
 * This case shows that one Gauge definition can drive several aligned lanes.
 * Focus on offsetY and how all lanes share the same value logic.
 */
static const GaugeDefinition g_screen1BasicTwoLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 18,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinBlueStraight }
            }
        },
        {
            .offsetY = 1,
            .segments = {
                { .cells = 12, .skin = &g_skinBlueStraight }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW
    }
};

/* Mirrored direction on the same simple strip.
 * Compare this with the first case to see how fillDirection and horizontalFlip
 * change the visual direction without changing the overall fill rules.
 */
static const GaugeDefinition g_screen1BasicMirrorDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_REVERSE,
    .originX = 22,
    .originY = 12,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .horizontalFlip = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinYellowStraight }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_DISABLED
    }
};

/* Lane windows and palette overrides.
 * These extra lanes start later than the base lane and switch palette to make
 * firstValueCell and overridePalette easy to spot.
 */
static const GaugeDefinition g_screen1PaletteLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 22,
    .originY = 18,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinYellowStraight }
            }
        },
        {
            .offsetY = 1,
            .firstValueCell = 1,
            .overridePalette = 1,
            .palette = PAL1,
            .segments = {
                { .cells = 10, .skin = &g_skinBlueStraight }
            }
        },
        {
            .offsetY = 2,
            .firstValueCell = 2,
            .overridePalette = 1,
            .palette = PAL2,
            .segments = {
                { .cells = 8, .skin = &g_skinLightBlueStraight }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK,
        .criticalValue = 32,
        .damageBlinkShift = 3
    }
};

/* Gain follow on simple art.
 * This case exists to isolate gainMode behavior from more stylized strip sets.
 */
static const GaugeDefinition g_screen1GainDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 15,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinYellowStraightGain }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .damageAnimShift = 3,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW,
        .gainAnimShift = 3,
        .gainBlinkShift = 2
    }
};

/* Critical blink with a dedicated blinkOff strip family.
 * Lower the value past criticalValue to see where blinkOff art replaces the
 * regular strip family.
 */
static const GaugeDefinition g_screen1BlinkOffDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 22,
    .originY = 15,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinYellowStraightBlinkOff }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 36,
        .damageBlinkShift = 2
    }
};

/* Minimal authored strip.
 * This case teaches that a usable fill gauge can be built from a single body
 * strip when the art does not need dedicated trail or end variants.
 */
static const GaugeDefinition g_screen1SmallBlueDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 23,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinBlueStraightSmall }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_DISABLED
    }
};

/* Screen 1 HUD summaries.
 * Keep these lines short: they only help the player identify the selected case.
 * The detailed teaching notes live above the definitions.
 */
static const DemoCaseSource g_screen1Cases[] = {
    {
        .descriptionLine1 = "One fill forward lane, one segment.",
        .descriptionLine2 = "Watch follow damage trail on loss.",
        .descriptionLine3 = "Check one lane and one segment.",
        .cursorTileX = 1,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen1BasicSingleDefinition
    },
    {
        .descriptionLine1 = "Reverse fill with horizontal mirror.",
        .descriptionLine2 = "No trail definition.",
        .descriptionLine3 = "Check fillDirection and hFlip.",
        .cursorTileX = 20,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen1BasicMirrorDefinition
    },
    {
        .descriptionLine1 = "Gain mode follow adds a leading trail.",
        .descriptionLine2 = "Increase to watch value catch it.",
        .descriptionLine3 = "Check gainMode and gainAnimShift.",
        .cursorTileX = 1,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen1GainDefinition
    },
    {
        .descriptionLine1 = "Critical static trail uses blinkOff.",
        .descriptionLine2 = "Lower below threshold to see it.",
        .descriptionLine3 = "Check blinkOff and criticalValue.",
        .cursorTileX = 20,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen1BlinkOffDefinition
    },
    {
        .descriptionLine1 = "Two aligned lanes share one value.",
        .descriptionLine2 = "Inspect offsetY and shared updates.",
        .descriptionLine3 = "Check the second lane offsetY.",
        .cursorTileX = 1,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen1BasicTwoLanesDefinition
    },
    {
        .descriptionLine1 = "Three lanes use windows and palettes.",
        .descriptionLine2 = "Lower to see critical trail blink.",
        .descriptionLine3 = "Check firstValueCell and palette.",
        .cursorTileX = 20,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen1PaletteLanesDefinition
    },
    {
        .descriptionLine1 = "Small blue strip uses one body tile set.",
        .descriptionLine2 = "No trail or end art is authored here.",
        .descriptionLine3 = "Check the minimal body-only skin.",
        .cursorTileX = 1,
        .cursorTileY = 23,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen1SmallBlueDefinition
    }
};

/* -----------------------------------------------------------------------------
   Screen 2: stylized fill horizontal

   Teaching goal:
   - reuse the same Gauge rules with more expressive art families
   - show bridges, caps, lane windows, and gain families on stylized strips
   - make strip selection visible without changing the HUD format
   ----------------------------------------------------------------------------- */
/* Yellow border caps without bridges.
 * This case isolates fixed start / end caps inside one uniform yellow border
 * family so a junior can inspect cap art without color transitions.
 */
static const GaugeDefinition g_screen2BorderYellowCapsDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 12,
    .maxValue = 96,
    .fixedStartCap = 1,
    .fixedEndCap = 1,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 1, .skin = &g_skinBorderYellowCapStart },
                { .cells = 10, .skin = &g_skinBorderYellow },
                { .cells = 1, .skin = &g_skinBorderYellowCapEnd }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 32,
        .damageBlinkShift = 2
    }
};

/* Bevel strips and bridges.
 * Start with one stylized family, then add segment bridges to show where the
 * renderer swaps bridge art instead of the normal body / end strips.
 */
static const GaugeDefinition g_screen2BridgeDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 22,
    .originY = 12,
    .maxValue = 100,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 4, .skin = &g_skinBevelLightBlue },
                { .cells = 4, .skin = &g_skinBevelBlue },
                { .cells = 4, .skin = &g_skinBevelYellow }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK,
        .criticalValue = 32,
        .damageAnimShift = 3,
        .damageBlinkShift = 2,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW,
        .gainAnimShift = 4,
        .gainBlinkShift = 3
    }
};

/* Fixed caps on stylized art.
 * These two definitions compare forward and reverse fills while the fixed cap
 * art stays obvious at both ends.
 */
static const GaugeDefinition g_screen2CapsLeftDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 15,
    .maxValue = 96,
    .fixedStartCap = 1,
    .fixedEndCap = 1,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 1, .skin = &g_skinBorderBlueCapStart },
                { .cells = 5, .skin = &g_skinBorderBlue },
                { .cells = 5, .skin = &g_skinBorderYellow },
                { .cells = 1, .skin = &g_skinBorderYellowCapEnd }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 32,
        .damageBlinkShift = 2
    }
};

/* Reverse counterpart of the previous cap example.
 * Read fillDirection and horizontalFlip together with the fixed caps.
 */
static const GaugeDefinition g_screen2CapsRightDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_REVERSE,
    .originX = 23,
    .originY = 15,
    .maxValue = 96,
    .fixedStartCap = 1,
    .fixedEndCap = 1,
    .palette = PAL0,
    .priority = 1,
    .horizontalFlip = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 1, .skin = &g_skinBorderBlueCapStart },
                { .cells = 5, .skin = &g_skinBorderBlue },
                { .cells = 5, .skin = &g_skinBorderYellow },
                { .cells = 1, .skin = &g_skinBorderYellowCapEnd }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 32,
        .damageBlinkShift = 2
    }
};

/* Lower linked lane with stylized bridges.
 * This case focuses on offsetY, firstValueCell, and how a shorter lower lane
 * reuses the base lane's value mapping.
 */
static const GaugeDefinition g_screen2LowerBridgeDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 22,
    .originY = 18,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 4, .skin = &g_skinBevelLightBlue },
                { .cells = 4, .skin = &g_skinBevelBlue },
                { .cells = 4, .skin = &g_skinBevelYellow }
            }
        },
        {
            .offsetY = 1,
            .firstValueCell = 1,
            .segments = {
                { .cells = 3, .skin = &g_skinBevelLightBlue },
                { .cells = 2, .skin = &g_skinBevelBlue }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 36,
        .damageBlinkShift = 2
    }
};

/* Three stylized lanes with lane-local windows.
 * Read lane offsets, firstValueCell, and palette ownership on each lane.
 */
static const GaugeDefinition g_screen2ThreeLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 18,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinBevelYellow }
            }
        },
        {
            .offsetY = 1,
            .firstValueCell = 1,
            .segments = {
                { .cells = 8, .skin = &g_skinBevelBlue }
            }
        },
        {
            .offsetY = 2,
            .firstValueCell = 2,
            .segments = {
                { .cells = 4, .skin = &g_skinBevelLightBlue }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK,
        .criticalValue = 32,
        .damageBlinkShift = 2
    }
};

/* Bevelback variants.
 * These cases add another art family to compare gain follow and a clean
 * lightblue -> blue bridge on the same horizontal runtime rules.
 */
static const GaugeDefinition g_screen2BevelbackLightBlueDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 3,
    .originY = 23,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinBevelbackLightBlue }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW,
        .gainAnimShift = 3
    }
};

/* Two-segment bevelback bridge.
 * The interesting fields are the segment order and the gain bridge strips.
 */
static const GaugeDefinition g_screen2BevelbackBridgeDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 22,
    .originY = 23,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 6, .skin = &g_skinBevelbackLightBlue },
                { .cells = 6, .skin = &g_skinBevelbackBlue }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW,
        .gainAnimShift = 3
    }
};

/* Screen 2 HUD summaries.
 * The text stays compact on purpose, so the surrounding comments explain the
 * art families, fields, and behaviors in more detail.
 */
static const DemoCaseSource g_screen2Cases[] = {
    {
        .descriptionLine1 = "Yellow border caps frame one strip family.",
        .descriptionLine2 = "Lower below critical to see blinkOff caps.",
        .descriptionLine3 = "Check cap start, body, and cap end.",
        .cursorTileX = 1,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2BorderYellowCapsDefinition
    },
    {
        .descriptionLine1 = "Segment bridges link different skins.",
        .descriptionLine2 = "Watch bridge strips at boundaries.",
        .descriptionLine3 = "Check segment order and bridges.",
        .cursorTileX = 20,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2BridgeDefinition
    },
    {
        .descriptionLine1 = "Fixed caps on a forward fill gauge.",
        .descriptionLine2 = "Critical mode swaps in blink strips.",
        .descriptionLine3 = "Check fixedStartCap and fixedEndCap.",
        .cursorTileX = 1,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2CapsLeftDefinition
    },
    {
        .descriptionLine1 = "Fixed caps with mirrored reverse fill.",
        .descriptionLine2 = "Compare cap placement from the right.",
        .descriptionLine3 = "Check reverse fill with hFlip.",
        .cursorTileX = 20,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2CapsRightDefinition
    },
    {
        .descriptionLine1 = "Three stylized lanes use lane windows.",
        .descriptionLine2 = "Critical value mode blinks the value.",
        .descriptionLine3 = "Check lane windows and value blink.",
        .cursorTileX = 1,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2ThreeLanesDefinition
    },
    {
        .descriptionLine1 = "Lower linked lane adds a bridge strip.",
        .descriptionLine2 = "Watch base lane mapping at the join.",
        .descriptionLine3 = "Check lower lane firstValueCell.",
        .cursorTileX = 20,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2LowerBridgeDefinition
    },
    {
        .descriptionLine1 = "Bevelback lightblue uses gain strips.",
        .descriptionLine2 = "Increase to inspect the gain family.",
        .descriptionLine3 = "Check bevelback body, trail, and end.",
        .cursorTileX = 1,
        .cursorTileY = 23,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2BevelbackLightBlueDefinition
    },
    {
        .descriptionLine1 = "Bevelback lightblue bridges into blue.",
        .descriptionLine2 = "Increase to inspect the gain bridge.",
        .descriptionLine3 = "Check segment order and bridge strips.",
        .cursorTileX = 20,
        .cursorTileY = 23,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen2BevelbackBridgeDefinition
    }
};

/* -----------------------------------------------------------------------------
   Screen 3: basic fill vertical

   Teaching goal:
   - show the vertical counterpart of the horizontal fill rules
   - remind the reader that vertical gauges need dedicated art
   - reuse the same behavior vocabulary on a different orientation
   ----------------------------------------------------------------------------- */
/* Vertical baseline with gain follow.
 * This is the first vertical case to inspect because it combines a simple lane
 * setup with an easy-to-see gain trail.
 */
static const GaugeDefinition g_screen3VerticalSingleDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 6,
    .originY = 24,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinVerticalYellowGainBlinkOff }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .damageAnimShift = 3,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW,
        .gainAnimShift = 3,
        .gainBlinkShift = 2
    }
};

/* Two aligned vertical lanes.
 * Compare this to the horizontal two-lane case and focus on offsetX.
 */
static const GaugeDefinition g_screen3VerticalTwoLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 14,
    .originY = 24,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinVerticalBlue }
            }
        },
        {
            .offsetX = 1,
            .segments = {
                { .cells = 12, .skin = &g_skinVerticalBlue }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

/* Three vertical lanes with palette changes.
 * Read firstValueCell, offsetX, and overridePalette to see how the same ideas
 * from Screen 1 apply vertically.
 */
static const GaugeDefinition g_screen3VerticalThreeLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 22,
    .originY = 24,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinVerticalYellow }
            }
        },
        {
            .offsetX = 1,
            .firstValueCell = 1,
            .overridePalette = 1,
            .palette = PAL1,
            .segments = {
                { .cells = 10, .skin = &g_skinVerticalBlue }
            }
        },
        {
            .offsetX = 2,
            .firstValueCell = 2,
            .overridePalette = 1,
            .palette = PAL2,
            .segments = {
                { .cells = 8, .skin = &g_skinVerticalLightBlue }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK,
        .criticalValue = 32,
        .damageBlinkShift = 3
    }
};

/* Reverse vertical fill with blinkOff behavior.
 * This case exists to compare reverse direction and critical blink on a
 * vertical gauge.
 */
static const GaugeDefinition g_screen3VerticalMirrorDefinition = {
    .mode = GAUGE_MODE_FILL,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_REVERSE,
    .originX = 32,
    .originY = 24,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .verticalFlip = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinVerticalYellowGainBlinkOff }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 32,
        .damageBlinkShift = 2
    }
};

/* Screen 3 HUD summaries.
 * As on the other screens, the HUD lines only identify the selected case.
 */
static const DemoCaseSource g_screen3Cases[] = {
    {
        .descriptionLine1 = "Vertical fill with gain follow trail.",
        .descriptionLine2 = "Increase to see the trail rise first.",
        .descriptionLine3 = "Check gainMode on a vertical gauge.",
        .cursorTileX = 4,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen3VerticalSingleDefinition
    },
    {
        .descriptionLine1 = "Two vertical lanes share one value.",
        .descriptionLine2 = "Inspect offsetX and shared updates.",
        .descriptionLine3 = "Check the second lane offsetX.",
        .cursorTileX = 12,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen3VerticalTwoLanesDefinition
    },
    {
        .descriptionLine1 = "Three lanes stack with lane windows.",
        .descriptionLine2 = "Critical trail blink uses palettes.",
        .descriptionLine3 = "Check palettes and critical trail.",
        .cursorTileX = 20,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen3VerticalThreeLanesDefinition
    },
    {
        .descriptionLine1 = "Reverse vertical fill uses blinkOff.",
        .descriptionLine2 = "Lower below critical to see blinking.",
        .descriptionLine3 = "Check reverse fill and blinkOff.",
        .cursorTileX = 30,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .definition = &g_screen3VerticalMirrorDefinition
    }
};

/* -----------------------------------------------------------------------------
   Screen 4: PIP

   Teaching goal:
   - introduce PIP quantization as a distinct renderer
   - compare single-tile, HALF, and QUARTER coverage
   - keep the same showcase interaction while changing the rendering model
   ----------------------------------------------------------------------------- */
/* HALF coverage.
 * One logical pip reuses one source tile and mirrors it horizontally.
 */
static const GaugeDefinition g_screen4PipHalfDefinition = {
    .mode = GAUGE_MODE_PIP,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 4,
    .originY = 15,
    .maxValue = 6,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 6, .skin = &g_skinPipDoubleHalf }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW
    }
};

/* QUARTER coverage.
 * One logical pip reuses one source tile four times with H/V mirroring.
 */
static const GaugeDefinition g_screen4PipQuarterDefinition = {
    .mode = GAUGE_MODE_PIP,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 4,
    .originY = 18,
    .maxValue = 6,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 6, .skin = &g_skinPipDoubleQuarter }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW
    }
};

/* Compact two-lane PIP bar.
 * This case teaches lane windows and critical blink in PIP mode.
 */
static const GaugeDefinition g_screen4MiniPipTwoLanesDefinition = {
    .mode = GAUGE_MODE_PIP,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 4,
    .originY = 21,
    .maxValue = 6,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 6, .skin = &g_skinPipMiniBar }
            }
        },
        {
            .offsetY = 1,
            .firstValueCell = 1,
            .segments = {
                { .cells = 5, .skin = &g_skinPipMiniBar }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK,
        .criticalValue = 3,
        .damageBlinkShift = 2
    }
};

/* Simplest PIP baseline.
 * Start here to understand the one-cell -> one-tile version before reading
 * the mirrored HALF and QUARTER cases.
 */
static const GaugeDefinition g_screen4PipSingleTileDefinition = {
    .mode = GAUGE_MODE_PIP,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 4,
    .originY = 12,
    .maxValue = 12,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinPipSingle }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

/* Vertical PIP lanes.
 * This is the PIP counterpart of the vertical multi-lane fill cases.
 */
static const GaugeDefinition g_screen4VerticalPipDefinition = {
    .mode = GAUGE_MODE_PIP,
    .plane = WINDOW,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .originX = 29,
    .originY = 19,
    .maxValue = 8,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 8, .skin = &g_skinPipSingle }
            }
        },
        {
            .offsetX = 1,
            .firstValueCell = 1,
            .segments = {
                { .cells = 7, .skin = &g_skinPipSingle }
            }
        },
        {
            .offsetX = 2,
            .firstValueCell = 2,
            .segments = {
                { .cells = 6, .skin = &g_skinPipSingle }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 3,
        .damageBlinkShift = 2
    }
};

/* Screen 4 HUD summaries.
 * The short lines name the selected PIP case; the comments above explain the
 * underlying rendering strategy in more detail.
 */
static const DemoCaseSource g_screen4Cases[] = {
    {
        .descriptionLine1 = "One pip cell maps to one tile.",
        .descriptionLine2 = "Use it as the simplest PIP setup.",
        .descriptionLine3 = "Check basic PIP quantization.",
        .cursorTileX = 2,
        .cursorTileY = 12,
        .stepAmount = DEMO_PIP_STEP,
        .definition = &g_screen4PipSingleTileDefinition
    },
    {
        .descriptionLine1 = "HALF mirrors one source tile.",
        .descriptionLine2 = "Each pip draws twice with HFLIP.",
        .descriptionLine3 = "VRAM drops from 12 tiles to 6.",
        .cursorTileX = 2,
        .cursorTileY = 15,
        .stepAmount = DEMO_PIP_STEP,
        .definition = &g_screen4PipHalfDefinition
    },
    {
        .descriptionLine1 = "QUARTER mirrors one source tile.",
        .descriptionLine2 = "Each pip draws four mirrored tiles.",
        .descriptionLine3 = "VRAM drops from 24 tiles to 6.",
        .cursorTileX = 2,
        .cursorTileY = 18,
        .stepAmount = DEMO_PIP_STEP,
        .definition = &g_screen4PipQuarterDefinition
    },
    {
        .descriptionLine1 = "Two compact pip lanes share a value.",
        .descriptionLine2 = "Inspect lane windows in PIP mode.",
        .descriptionLine3 = "Check firstValueCell in PIP mode.",
        .cursorTileX = 2,
        .cursorTileY = 21,
        .stepAmount = DEMO_PIP_STEP,
        .definition = &g_screen4MiniPipTwoLanesDefinition
    },
    {
        .descriptionLine1 = "Three vertical pip lanes share steps.",
        .descriptionLine2 = "Critical blink affects the trail pips.",
        .descriptionLine3 = "Check vertical PIP lane windows.",
        .cursorTileX = 27,
        .cursorTileY = 15,
        .stepAmount = DEMO_PIP_STEP,
        .definition = &g_screen4VerticalPipDefinition
    }
};

/* -----------------------------------------------------------------------------
   Screen registry
   ----------------------------------------------------------------------------- */
const DemoScreenSource g_screens[DEMO_SCREEN_COUNT] = {
    {
        .title = "Screen 1/4 - Basic fill H",
        .cases = g_screen1Cases,
        .caseCount = (u8)ARRAY_LEN(g_screen1Cases)
    },
    {
        .title = "Screen 2/4 - Stylized fill H",
        .cases = g_screen2Cases,
        .caseCount = (u8)ARRAY_LEN(g_screen2Cases)
    },
    {
        .title = "Screen 3/4 - Basic fill V",
        .cases = g_screen3Cases,
        .caseCount = (u8)ARRAY_LEN(g_screen3Cases)
    },
    {
        .title = "Screen 4/4 - PIP",
        .cases = g_screen4Cases,
        .caseCount = (u8)ARRAY_LEN(g_screen4Cases)
    }
};
