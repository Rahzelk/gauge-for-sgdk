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
    .originX = 4,
    .originY = 8,
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
    .originX = 4,
    .originY = 21,
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
    .originX = 24,
    .originY = 8,
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
    .originX = 24,
    .originY = 21,
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
    .originX = 4,
    .originY = 14,
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
    .originX = 24,
    .originY = 14,
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
        .label = "Basic 1 lane",
        .cursorTileX = 2,
        .cursorTileY = 8,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicSingleDefinition }
        }
    },
    {
        .label = "Mirror fill",
        .cursorTileX = 22,
        .cursorTileY = 8,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicMirrorDefinition }
        }
    },
    {
        .label = "Gain follow",
        .cursorTileX = 2,
        .cursorTileY = 14,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1GainDefinition }
        }
    },
    {
        .label = "Blink off",
        .cursorTileX = 22,
        .cursorTileY = 14,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BlinkOffDefinition }
        }
    },
    {
        .label = "Basic 2 lanes",
        .cursorTileX = 2,
        .cursorTileY = 21,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicTwoLanesDefinition }
        }
    },
    {
        .label = "3 lanes + palettes",
        .cursorTileX = 22,
        .cursorTileY = 21,
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
    .originY = 8,
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
    .originY = 8,
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
    .originY = 14,
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
    .originY = 14,
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
    .originY = 20,
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
    .originY = 20,
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
        .label = "Bevel",
        .cursorTileX = 1,
        .cursorTileY = 8,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2BevelDefinition }
        }
    },
    {
        .label = "Bridges",
        .cursorTileX = 20,
        .cursorTileY = 8,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2BridgeDefinition }
        }
    },
    {
        .label = "Caps + mirror blink",
        .cursorTileX = 1,
        .cursorTileY = 14,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 2,
        .gauges = {
            { .definition = &g_screen2CapsLeftDefinition },
            { .definition = &g_screen2CapsRightDefinition }
        }
    },
    {
        .label = "3 stylized lanes",
        .cursorTileX = 1,
        .cursorTileY = 21,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2ThreeLanesDefinition }
        }
    },
    {
        .label = "Lower lane bridge",
        .cursorTileX = 20,
        .cursorTileY = 20,
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
        .label = "Vertical gain",
        .cursorTileX = 4,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen3VerticalSingleDefinition }
        }
    },
    {
        .label = "Vertical 2 lanes",
        .cursorTileX = 12,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen3VerticalTwoLanesDefinition }
        }
    },
    {
        .label = "Vertical 3 lanes",
        .cursorTileX = 20,
        .cursorTileY = 18,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen3VerticalThreeLanesDefinition }
        }
    },
    {
        .label = "Vertical critical mirror",
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
    .originY = 14,
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
    .originY = 17,
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
    .originY = 11,
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
        .label = "PIP 1 cell = 1 tile",
        .cursorTileX = 2,
        .cursorTileY = 11,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipSingleTileDefinition }
        }
    },
    {
        .label = "PIP basic",
        .cursorTileX = 2,
        .cursorTileY = 14,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipBasicDefinition }
        }
    },
    {
        .label = "PIP quarter 2x2",
        .cursorTileX = 2,
        .cursorTileY = 18,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipQuarterDefinition }
        }
    },
    {
        .label = "Mini PIP 2 lanes",
        .cursorTileX = 2,
        .cursorTileY = 21,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4MiniPipTwoLanesDefinition }
        }
    },
    {
        .label = "Vertical PIP 3 lanes",
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
