#include <genesis.h>
#include "showcase_internal.h"
#include "gauge_assets.h"

/* =============================================================================
   Gauge showcase data
   ============================================================================= */

/* -----------------------------------------------------------------------------
   Reusable skins
   ----------------------------------------------------------------------------- */
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

static const GaugeSkin g_skinLightBlueStraight = {
    .fill = {
        .normal = {
            .body = &gauge_h_straight_lightblue_strip
        }
    }
};

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

static const GaugeSkin g_skinPipBasic = {
    .pip = {
        .tileset = &gauge_h_pip_basic_strip,
        .pipWidth = 2,
        .pipHeight = 1,
        .coverage = GAUGE_PIP_COVERAGE_FULL
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
   ----------------------------------------------------------------------------- */
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

static const DemoCaseSource g_screen1Cases[] = {
    {
        .descriptionLine1 = "One fill lane, one segment, forward.",
        .descriptionLine2 = "Watch follow damage trail on loss.",
        .descriptionLine3 = "Check one lane and one segment.",
        .cursorTileX = 1,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicSingleDefinition }
        }
    },
    {
        .descriptionLine1 = "Reverse fill with horizontal mirror.",
        .descriptionLine2 = "Value animates from right to left.",
        .descriptionLine3 = "Check fillDirection and hFlip.",
        .cursorTileX = 20,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicMirrorDefinition }
        }
    },
    {
        .descriptionLine1 = "Gain mode follow adds a leading trail.",
        .descriptionLine2 = "Increase to watch value catch it.",
        .descriptionLine3 = "Check gainMode and gainAnimShift.",
        .cursorTileX = 1,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1GainDefinition }
        }
    },
    {
        .descriptionLine1 = "Critical static trail uses blinkOff.",
        .descriptionLine2 = "Lower below threshold to see it.",
        .descriptionLine3 = "Check blinkOff and criticalValue.",
        .cursorTileX = 20,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BlinkOffDefinition }
        }
    },
    {
        .descriptionLine1 = "Two aligned lanes share one value.",
        .descriptionLine2 = "Inspect offsetY and shared updates.",
        .descriptionLine3 = "Check the second lane offsetY.",
        .cursorTileX = 1,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicTwoLanesDefinition }
        }
    },
    {
        .descriptionLine1 = "Three lanes use windows and palettes.",
        .descriptionLine2 = "Lower to see critical trail blink.",
        .descriptionLine3 = "Check firstValueCell and palette.",
        .cursorTileX = 20,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1PaletteLanesDefinition }
        }
    }
};

/* -----------------------------------------------------------------------------
   Screen 2: stylized fill horizontal
   ----------------------------------------------------------------------------- */
static const GaugeDefinition g_screen2BevelDefinition = {
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
                { .cells = 12, .skin = &g_skinBevelYellow }
            }
        }
    },
    .behavior = {
        DEMO_DEFAULT_BEHAVIOR_TIMINGS,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW
    }
};

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

static const DemoCaseSource g_screen2Cases[] = {
    {
        .descriptionLine1 = "Bevel skin shows body, trail, end.",
        .descriptionLine2 = "Use it to inspect strip selection.",
        .descriptionLine3 = "Check body, trail, and end strips.",
        .cursorTileX = 1,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2BevelDefinition }
        }
    },
    {
        .descriptionLine1 = "Segment bridges link different skins.",
        .descriptionLine2 = "Watch bridge strips at boundaries.",
        .descriptionLine3 = "Check segment order and bridges.",
        .cursorTileX = 20,
        .cursorTileY = 12,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2BridgeDefinition }
        }
    },
    {
        .descriptionLine1 = "Fixed caps on a forward fill gauge.",
        .descriptionLine2 = "Critical mode swaps in blink strips.",
        .descriptionLine3 = "Check fixedStartCap and fixedEndCap.",
        .cursorTileX = 1,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2CapsLeftDefinition }
        }
    },
    {
        .descriptionLine1 = "Fixed caps with mirrored reverse fill.",
        .descriptionLine2 = "Compare cap placement from the right.",
        .descriptionLine3 = "Check reverse fill with hFlip.",
        .cursorTileX = 20,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2CapsRightDefinition }
        }
    },
    {
        .descriptionLine1 = "Three stylized lanes use lane windows.",
        .descriptionLine2 = "Critical value mode blinks the value.",
        .descriptionLine3 = "Check lane windows and value blink.",
        .cursorTileX = 1,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2ThreeLanesDefinition }
        }
    },
    {
        .descriptionLine1 = "Lower linked lane adds a bridge strip.",
        .descriptionLine2 = "Watch base lane mapping at the join.",
        .descriptionLine3 = "Check lower lane firstValueCell.",
        .cursorTileX = 20,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2LowerBridgeDefinition }
        }
    }
};

/* -----------------------------------------------------------------------------
   Screen 3: basic fill vertical
   ----------------------------------------------------------------------------- */
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

static const DemoCaseSource g_screen3Cases[] = {
    {
        .descriptionLine1 = "Vertical fill with gain follow trail.",
        .descriptionLine2 = "Increase to see the trail rise first.",
        .descriptionLine3 = "Check gainMode on a vertical gauge.",
        .cursorTileX = 4,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen3VerticalSingleDefinition }
        }
    },
    {
        .descriptionLine1 = "Two vertical lanes share one value.",
        .descriptionLine2 = "Inspect offsetX and shared updates.",
        .descriptionLine3 = "Check the second lane offsetX.",
        .cursorTileX = 12,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen3VerticalTwoLanesDefinition }
        }
    },
    {
        .descriptionLine1 = "Three lanes stack with lane windows.",
        .descriptionLine2 = "Critical trail blink uses palettes.",
        .descriptionLine3 = "Check palettes and critical trail.",
        .cursorTileX = 20,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen3VerticalThreeLanesDefinition }
        }
    },
    {
        .descriptionLine1 = "Reverse vertical fill uses blinkOff.",
        .descriptionLine2 = "Lower below critical to see blinking.",
        .descriptionLine3 = "Check reverse fill and blinkOff.",
        .cursorTileX = 30,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen3VerticalMirrorDefinition }
        }
    }
};

/* -----------------------------------------------------------------------------
   Screen 4: PIP
   ----------------------------------------------------------------------------- */
static const GaugeDefinition g_screen4PipBasicDefinition = {
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
                { .cells = 6, .skin = &g_skinPipBasic }
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

static const DemoCaseSource g_screen4Cases[] = {
    {
        .descriptionLine1 = "One pip cell maps to one tile.",
        .descriptionLine2 = "Use it as the simplest PIP setup.",
        .descriptionLine3 = "Check basic PIP quantization.",
        .cursorTileX = 2,
        .cursorTileY = 12,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipSingleTileDefinition }
        }
    },
    {
        .descriptionLine1 = "Basic horizontal PIP value states.",
        .descriptionLine2 = "Compare gain and damage trail pips.",
        .descriptionLine3 = "Check fill mode set to PIP.",
        .cursorTileX = 2,
        .cursorTileY = 15,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipBasicDefinition }
        }
    },
    {
        .descriptionLine1 = "Each pip uses a 2x2 tile block.",
        .descriptionLine2 = "Good for multi-tile PIP composition.",
        .descriptionLine3 = "Check pip width and pip height.",
        .cursorTileX = 2,
        .cursorTileY = 18,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipQuarterDefinition }
        }
    },
    {
        .descriptionLine1 = "Two compact pip lanes share a value.",
        .descriptionLine2 = "Inspect lane windows in PIP mode.",
        .descriptionLine3 = "Check firstValueCell in PIP mode.",
        .cursorTileX = 2,
        .cursorTileY = 21,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4MiniPipTwoLanesDefinition }
        }
    },
    {
        .descriptionLine1 = "Three vertical pip lanes share steps.",
        .descriptionLine2 = "Critical blink affects the trail pips.",
        .descriptionLine3 = "Check vertical PIP lane windows.",
        .cursorTileX = 27,
        .cursorTileY = 15,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4VerticalPipDefinition }
        }
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
