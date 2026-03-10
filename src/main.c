#include <genesis.h>
#include "gauge.h"
#include "gauge_assets.h"

/* =============================================================================
   Gauge demo
   ============================================================================= */

#define GAUGE_VRAM_BASE            TILE_USER_INDEX
#define DEMO_GAUGE_VRAM_MODE       GAUGE_VRAM_DYNAMIC

#define DEMO_SCREEN_COUNT          4
#define DEMO_MAX_CASES_PER_SCREEN  6
#define DEMO_MAX_GAUGES_PER_CASE   2
#define DEMO_MAX_ACTIVE_GAUGES     8

#define DEMO_FILL_STEP             4
#define DEMO_PIP_STEP              1
#define DEMO_REPEAT_DELAY          12
#define DEMO_REPEAT_MASK           3

#define DEMO_INCREASE_HOLD         24
#define DEMO_INCREASE_BLINK        16
#define DEMO_DECREASE_HOLD         20
#define DEMO_DECREASE_BLINK        48

#define ARRAY_LEN(array)           ((u16)(sizeof(array) / sizeof((array)[0])))

typedef struct
{
    const GaugeDefinition *definition;
} DemoGaugeSource;

typedef struct
{
    const char *label;
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
    const char *label;
    u8 cursorTileX;
    u8 cursorTileY;
    u16 stepAmount;
    u8 gaugeCount;
    Gauge *gauges[DEMO_MAX_GAUGES_PER_CASE];
} DemoCaseRuntime;

static Gauge g_runtimeGaugePool[DEMO_MAX_ACTIVE_GAUGES];
static DemoCaseRuntime g_activeCases[DEMO_MAX_CASES_PER_SCREEN];
static u8 g_activeCaseCount = 0;
static u8 g_activeGaugeCount = 0;

static u16 g_previousPad = 0;
static u8 g_selectedScreen = 0;
static u8 g_selectedCase = 0;
static u8 g_holdA = 0;
static u8 g_holdB = 0;
static u16 g_frameCount = 0;
static Sprite *g_cursorSprite = NULL;

static const char s_blankLine[] = "                                        ";
static const char s_separator[] = "----------------------------------------";

/* -----------------------------------------------------------------------------
   Small helpers
   ----------------------------------------------------------------------------- */
static const char *getVramModeName(void)
{
    return (DEMO_GAUGE_VRAM_MODE == GAUGE_VRAM_FIXED) ? "FIXED" : "DYNAMIC";
}

static u8 buildGaugeFromDefinition(Gauge *gauge,
                                   const GaugeDefinition *definition,
                                   u16 *nextVram)
{
    const u16 vramBase = *nextVram;

    if (!Gauge_build(gauge, definition, vramBase))
        return 0;

    *nextVram = (u16)(vramBase + gauge->vramNextOffset);
    return 1;
}

static void clearHudLine(u16 y)
{
    VDP_drawText(s_blankLine, 0, y);
}

static void drawHudStatic(void)
{
    clearHudLine(0);
    clearHudLine(1);
    clearHudLine(2);
    clearHudLine(3);
    clearHudLine(4);
    clearHudLine(5);
    clearHudLine(6);

    VDP_drawText("A: increase   B: decrease", 1, 0);
    VDP_drawText("U/L prev  D/R next  C: change screen", 1, 1);
    VDP_drawText("Screen:", 1, 3);
    VDP_drawText("Selected:", 1, 4);
    VDP_drawText("VRAM mode:", 1, 5);
    VDP_drawText(s_separator, 0, 6);
}

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
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
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
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

static const GaugeDefinition g_screen1BasicTwoLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 24,
    .originY = 8,
    .maxValue = 80,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 10, .skin = &g_skinBlueStraight }
            }
        },
        {
            .offsetY = 1,
            .firstValueCell = 2,
            .segments = {
                { .cells = 6, .skin = &g_skinLightBlueStraight }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW
    }
};

static const GaugeDefinition g_screen1BasicMirrorDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_REVERSE,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 4,
    .originY = 14,
    .maxValue = 80,
    .palette = PAL0,
    .priority = 1,
    .horizontalFlip = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 10, .skin = &g_skinYellowStraight }
            }
        }
    },
    .behavior = {
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_DISABLED
    }
};

static const GaugeDefinition g_screen1PaletteLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 24,
    .originY = 14,
    .maxValue = 80,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 10, .skin = &g_skinYellowStraight }
            }
        },
        {
            .offsetY = 1,
            .firstValueCell = 1,
            .overridePalette = 1,
            .palette = PAL1,
            .segments = {
                { .cells = 8, .skin = &g_skinBlueStraight }
            }
        },
        {
            .offsetY = 2,
            .firstValueCell = 2,
            .overridePalette = 1,
            .palette = PAL2,
            .segments = {
                { .cells = 6, .skin = &g_skinLightBlueStraight }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK,
        .criticalValue = 24,
        .damageBlinkShift = 3
    }
};

static const GaugeDefinition g_screen1GainDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 4,
    .originY = 21,
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
        }
    },
    .behavior = {
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
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 24,
    .originY = 21,
    .maxValue = 64,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 4, .skin = &g_skinBorderBlue },
                { .cells = 4, .skin = &g_skinBorderYellow }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 24,
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
        .label = "Basic 2 lanes",
        .cursorTileX = 22,
        .cursorTileY = 8,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicTwoLanesDefinition }
        }
    },
    {
        .label = "Mirror fill",
        .cursorTileX = 2,
        .cursorTileY = 14,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BasicMirrorDefinition }
        }
    },
    {
        .label = "3 lanes + palettes",
        .cursorTileX = 22,
        .cursorTileY = 15,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1PaletteLanesDefinition }
        }
    },
    {
        .label = "Gain follow",
        .cursorTileX = 2,
        .cursorTileY = 21,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1GainDefinition }
        }
    },
    {
        .label = "Blink off",
        .cursorTileX = 22,
        .cursorTileY = 21,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen1BlinkOffDefinition }
        }
    }
};

/* -----------------------------------------------------------------------------
   Screen 2: stylized fill horizontal
   ----------------------------------------------------------------------------- */
static const GaugeDefinition g_screen2BevelDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
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
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW
    }
};

static const GaugeDefinition g_screen2BridgeDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
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
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 3,
    .originY = 14,
    .maxValue = 112,
    .fixedStartCap = 1,
    .fixedEndCap = 1,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 1, .skin = &g_skinBorderBlueCapStart },
                { .cells = 6, .skin = &g_skinBorderBlue },
                { .cells = 6, .skin = &g_skinBorderYellow },
                { .cells = 1, .skin = &g_skinBorderYellowCapEnd }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

static const GaugeDefinition g_screen2CapsRightDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_REVERSE,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 23,
    .originY = 14,
    .maxValue = 112,
    .fixedStartCap = 1,
    .fixedEndCap = 1,
    .palette = PAL0,
    .priority = 1,
    .horizontalFlip = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 1, .skin = &g_skinBorderBlueCapStart },
                { .cells = 6, .skin = &g_skinBorderBlue },
                { .cells = 6, .skin = &g_skinBorderYellow },
                { .cells = 1, .skin = &g_skinBorderYellowCapEnd }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

static const GaugeDefinition g_screen2Sample7Definition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 3,
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
                { .cells = 3, .skin = &g_skinBevelLightBlue }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK,
        .criticalValue = 40,
        .damageBlinkShift = 3
    }
};

static const GaugeDefinition g_screen2LowerBridgeDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
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
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 36,
        .damageBlinkShift = 2
    }
};

static const GaugeDefinition g_screen2ThreeLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 3,
    .originY = 25,
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
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_DISABLED
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
        .label = "Caps + mirror",
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
        .label = "2 lanes",
        .cursorTileX = 1,
        .cursorTileY = 20,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2Sample7Definition }
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
    },
    {
        .label = "3 stylized lanes",
        .cursorTileX = 1,
        .cursorTileY = 26,
        .stepAmount = DEMO_FILL_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen2ThreeLanesDefinition }
        }
    }
};

/* -----------------------------------------------------------------------------
   Screen 3: basic fill vertical
   ----------------------------------------------------------------------------- */
static const GaugeDefinition g_screen3VerticalSingleDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 6,
    .originY = 24,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 3, .skin = &g_skinVerticalBlue },
                { .cells = 5, .skin = &g_skinVerticalLightBlue },
                { .cells = 4, .skin = &g_skinVerticalYellow }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW
    }
};

static const GaugeDefinition g_screen3VerticalTwoLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 14,
    .originY = 24,
    .maxValue = 80,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 10, .skin = &g_skinVerticalBlue }
            }
        },
        {
            .offsetX = 1,
            .firstValueCell = 1,
            .segments = {
                { .cells = 8, .skin = &g_skinVerticalLightBlue }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

static const GaugeDefinition g_screen3VerticalThreeLanesDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 22,
    .originY = 24,
    .maxValue = 80,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 10, .skin = &g_skinVerticalYellow }
            }
        },
        {
            .offsetX = 1,
            .firstValueCell = 1,
            .overridePalette = 1,
            .palette = PAL1,
            .segments = {
                { .cells = 8, .skin = &g_skinVerticalBlue }
            }
        },
        {
            .offsetX = 2,
            .firstValueCell = 2,
            .overridePalette = 1,
            .palette = PAL2,
            .segments = {
                { .cells = 6, .skin = &g_skinVerticalLightBlue }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_TRAIL_BLINK,
        .criticalValue = 24,
        .damageBlinkShift = 3
    }
};

static const GaugeDefinition g_screen3VerticalMirrorDefinition = {
    .mode = GAUGE_MODE_FILL,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_REVERSE,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 32,
    .originY = 24,
    .maxValue = 96,
    .palette = PAL0,
    .priority = 1,
    .verticalFlip = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 12, .skin = &g_skinVerticalYellow }
            }
        }
    },
    .behavior = {
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_DISABLED
    }
};

static const DemoCaseSource g_screen3Cases[] = {
    {
        .label = "Vertical 1 lane",
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
        .label = "Vertical mirror",
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
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 4,
    .originY = 8,
    .maxValue = 7,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 7, .skin = &g_skinPipBasic }
            }
        }
    },
    .behavior = {
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW
    }
};

static const GaugeDefinition g_screen4PipQuarterDefinition = {
    .mode = GAUGE_MODE_PIP,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 22,
    .originY = 8,
    .maxValue = 4,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 4, .skin = &g_skinPipDoubleQuarter }
            }
        }
    },
    .behavior = {
        .valueAnimEnabled = 1,
        .valueAnimShift = 2,
        .damageMode = GAUGE_TRAIL_MODE_FOLLOW,
        .gainMode = GAUGE_GAIN_MODE_FOLLOW
    }
};

static const GaugeDefinition g_screen4MiniPipTwoLanesDefinition = {
    .mode = GAUGE_MODE_PIP,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 4,
    .originY = 14,
    .maxValue = 4,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 4, .skin = &g_skinPipMiniBar }
            }
        },
        {
            .offsetY = 1,
            .firstValueCell = 1,
            .segments = {
                { .cells = 3, .skin = &g_skinPipMiniBar }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_CRITICAL_VALUE_BLINK,
        .criticalValue = 2,
        .damageBlinkShift = 2
    }
};

static const GaugeDefinition g_screen4PipSingleTileDefinition = {
    .mode = GAUGE_MODE_PIP,
    .orientation = GAUGE_ORIENT_HORIZONTAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 22,
    .originY = 14,
    .maxValue = 6,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 6, .skin = &g_skinPipSingle }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL
    }
};

static const GaugeDefinition g_screen4VerticalPipDefinition = {
    .mode = GAUGE_MODE_PIP,
    .orientation = GAUGE_ORIENT_VERTICAL,
    .fillDirection = GAUGE_FILL_FORWARD,
    .vramMode = DEMO_GAUGE_VRAM_MODE,
    .originX = 31,
    .originY = 24,
    .maxValue = 5,
    .palette = PAL0,
    .priority = 1,
    .lanes = {
        {
            .segments = {
                { .cells = 5, .skin = &g_skinPipSingle }
            }
        },
        {
            .offsetX = 1,
            .firstValueCell = 1,
            .segments = {
                { .cells = 4, .skin = &g_skinPipSingle }
            }
        },
        {
            .offsetX = 2,
            .firstValueCell = 2,
            .segments = {
                { .cells = 3, .skin = &g_skinPipSingle }
            }
        }
    },
    .behavior = {
        .damageMode = GAUGE_TRAIL_MODE_STATIC_TRAIL_CRITICAL_BLINK,
        .criticalValue = 2,
        .damageBlinkShift = 2
    }
};

static const DemoCaseSource g_screen4Cases[] = {
    {
        .label = "PIP basic",
        .cursorTileX = 2,
        .cursorTileY = 8,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipBasicDefinition }
        }
    },
    {
        .label = "PIP quarter 2x2",
        .cursorTileX = 20,
        .cursorTileY = 9,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipQuarterDefinition }
        }
    },
    {
        .label = "Mini PIP 2 lanes",
        .cursorTileX = 2,
        .cursorTileY = 14,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4MiniPipTwoLanesDefinition }
        }
    },
    {
        .label = "PIP 1 cell = 1 tile",
        .cursorTileX = 20,
        .cursorTileY = 14,
        .stepAmount = DEMO_PIP_STEP,
        .gaugeCount = 1,
        .gauges = {
            { .definition = &g_screen4PipSingleTileDefinition }
        }
    },
    {
        .label = "Vertical PIP 3 lanes",
        .cursorTileX = 28,
        .cursorTileY = 18,
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
static const DemoScreenSource g_screens[DEMO_SCREEN_COUNT] = {
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

/* -----------------------------------------------------------------------------
   Runtime helpers
   ----------------------------------------------------------------------------- */
static void updateHudDynamic(void)
{
    const DemoScreenSource *screen = &g_screens[g_selectedScreen];
    const char *selectedLabel = "No case available";

    if (g_activeCaseCount > 0 && g_selectedCase < g_activeCaseCount)
        selectedLabel = g_activeCases[g_selectedCase].label;

    clearHudLine(2);
    clearHudLine(3);
    clearHudLine(4);
    clearHudLine(5);
    VDP_drawText("Screen:", 1, 3);
    VDP_drawText("Selected:", 1, 4);
    VDP_drawText("VRAM mode:", 1, 5);
    VDP_drawText(screen->title, 9, 3);
    VDP_drawText(selectedLabel, 11, 4);
    VDP_drawText(getVramModeName(), 12, 5);
}

static void updateCursorSprite(void)
{
    if (!g_cursorSprite)
        return;

    if (g_activeCaseCount == 0 || g_selectedCase >= g_activeCaseCount)
    {
        SPR_setVisibility(g_cursorSprite, HIDDEN);
        return;
    }

    {
        const DemoCaseRuntime *selectedCase = &g_activeCases[g_selectedCase];
        const s16 cursorX = (s16)(selectedCase->cursorTileX * 8);
        const s16 cursorY = (s16)(selectedCase->cursorTileY * 8);

        SPR_setPosition(g_cursorSprite, cursorX, cursorY);
        SPR_setVisibility(g_cursorSprite, VISIBLE);
    }
}

static void setupWindowFullScreen(void)
{
    VDP_setPlaneSize(64, 32, TRUE);
    VDP_setWindowHPos(TRUE, 0);
    VDP_setWindowVPos(TRUE, 0);
    VDP_setBackgroundColor(0);
    VDP_setTextPalette(PAL0);
    VDP_setTextPlane(WINDOW);
}

static void releaseActiveScreen(void)
{
    u8 gaugeIndex;

    for (gaugeIndex = 0; gaugeIndex < g_activeGaugeCount; gaugeIndex++)
    {
        Gauge_release(&g_runtimeGaugePool[gaugeIndex]);
        memset(&g_runtimeGaugePool[gaugeIndex], 0, sizeof(Gauge));
    }

    memset(g_activeCases, 0, sizeof(g_activeCases));
    g_activeCaseCount = 0;
    g_activeGaugeCount = 0;
}

static void clearScreenPlanes(void)
{
    VDP_clearPlane(BG_A, TRUE);
    VDP_clearPlane(BG_B, TRUE);
    VDP_clearPlane(WINDOW, TRUE);
}

static u8 tryBuildCase(const DemoCaseSource *sourceCase,
                       DemoCaseRuntime *runtimeCase,
                       u8 *gaugeCursor,
                       u16 *nextVram)
{
    const u8 startGaugeIndex = *gaugeCursor;
    u8 localGaugeCount = 0;
    u8 sourceGaugeIndex;

    memset(runtimeCase, 0, sizeof(*runtimeCase));
    runtimeCase->label = sourceCase->label;
    runtimeCase->cursorTileX = sourceCase->cursorTileX;
    runtimeCase->cursorTileY = sourceCase->cursorTileY;
    runtimeCase->stepAmount = sourceCase->stepAmount;

    for (sourceGaugeIndex = 0; sourceGaugeIndex < sourceCase->gaugeCount; sourceGaugeIndex++)
    {
        Gauge *gauge = NULL;
        const DemoGaugeSource *sourceGauge = &sourceCase->gauges[sourceGaugeIndex];

        if (*gaugeCursor >= DEMO_MAX_ACTIVE_GAUGES)
            break;

        gauge = &g_runtimeGaugePool[*gaugeCursor];
        if (!buildGaugeFromDefinition(gauge, sourceGauge->definition, nextVram))
            break;

        Gauge_setValue(gauge, gauge->logic.maxValue);
        runtimeCase->gauges[localGaugeCount] = gauge;
        localGaugeCount++;
        (*gaugeCursor)++;
    }

    if (localGaugeCount != sourceCase->gaugeCount)
    {
        while (*gaugeCursor > startGaugeIndex)
        {
            (*gaugeCursor)--;
            Gauge_release(&g_runtimeGaugePool[*gaugeCursor]);
            memset(&g_runtimeGaugePool[*gaugeCursor], 0, sizeof(Gauge));
        }
        memset(runtimeCase, 0, sizeof(*runtimeCase));
        return 0;
    }

    runtimeCase->gaugeCount = localGaugeCount;
    return 1;
}

static void loadScreen(u8 screenIndex)
{
    const DemoScreenSource *screen = &g_screens[screenIndex];
    u8 caseIndex;
    u8 activeCaseCount = 0;
    u8 gaugeCursor = 0;
    u16 nextVram = GAUGE_VRAM_BASE;

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

static DemoCaseRuntime *getSelectedCase(void)
{
    if (g_activeCaseCount == 0 || g_selectedCase >= g_activeCaseCount)
        return NULL;

    return &g_activeCases[g_selectedCase];
}

static void applyActionToSelectedCase(u8 isIncrease, u16 amount, u8 holdFrames, u8 blinkFrames)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();
    u8 gaugeIndex;

    if (!selectedCase)
        return;

    for (gaugeIndex = 0; gaugeIndex < selectedCase->gaugeCount; gaugeIndex++)
    {
        if (isIncrease)
            Gauge_increase(selectedCase->gauges[gaugeIndex], amount, holdFrames, blinkFrames);
        else
            Gauge_decrease(selectedCase->gauges[gaugeIndex], amount, holdFrames, blinkFrames);
    }
}

static void changeSelection(s16 direction)
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

static void changeScreen(void)
{
    g_selectedScreen = (u8)((g_selectedScreen + 1) % DEMO_SCREEN_COUNT);
    g_selectedCase = 0;
    g_holdA = 0;
    g_holdB = 0;
    loadScreen(g_selectedScreen);
}

static void handleInput(u16 pressed, u16 held)
{
    DemoCaseRuntime *selectedCase = getSelectedCase();
    u16 stepAmount = DEMO_FILL_STEP;

    if (pressed & BUTTON_START)
    {
        if (selectedCase && selectedCase->gaugeCount > 0)
        {
            const u8 enabled = selectedCase->gauges[0] ?
                (selectedCase->gauges[0]->debugMode ? 0 : 1) : 1;

            for (u8 gaugeIndex = 0; gaugeIndex < selectedCase->gaugeCount; gaugeIndex++)
            {
                Gauge *gauge = selectedCase->gauges[gaugeIndex];
                if (gauge)
                    gauge->debugMode = enabled;
            }
        }
    }

    if (pressed & BUTTON_C)
    {
        changeScreen();
        return;
    }

    if (pressed & (BUTTON_UP | BUTTON_LEFT))
        changeSelection(-1);
    else if (pressed & (BUTTON_DOWN | BUTTON_RIGHT))
        changeSelection(1);

    selectedCase = getSelectedCase();
    if (selectedCase)
        stepAmount = selectedCase->stepAmount;

    if (pressed & BUTTON_A)
    {
        applyActionToSelectedCase(1, stepAmount, DEMO_INCREASE_HOLD, DEMO_INCREASE_BLINK);
        g_holdA = 0;
    }

    if (pressed & BUTTON_B)
    {
        applyActionToSelectedCase(0, stepAmount, DEMO_DECREASE_HOLD, DEMO_DECREASE_BLINK);
        g_holdB = 0;
    }

    if (held & BUTTON_A)
    {
        g_holdA++;
        if (g_holdA >= DEMO_REPEAT_DELAY && (g_frameCount & DEMO_REPEAT_MASK) == 0)
            applyActionToSelectedCase(1, 1, 0, 0);
    }
    else
    {
        g_holdA = 0;
    }

    if (held & BUTTON_B)
    {
        g_holdB++;
        if (g_holdB >= DEMO_REPEAT_DELAY && (g_frameCount & DEMO_REPEAT_MASK) == 0)
            applyActionToSelectedCase(0, 1, 0, 0);
    }
    else
    {
        g_holdB = 0;
    }
}

static void updateActiveGauges(void)
{
    u8 gaugeIndex;

    for (gaugeIndex = 0; gaugeIndex < g_activeGaugeCount; gaugeIndex++)
        Gauge_update(&g_runtimeGaugePool[gaugeIndex]);
}

static void loadAllPalettes(void)
{
    PAL_setPalette(PAL0, gauge_palette.data, DMA);
    PAL_setPalette(PAL1, gauge_blue_palette.data, DMA);
    PAL_setPalette(PAL2, gauge_lightblue_palette.data, DMA);
    PAL_setPalette(PAL3, gauge_palette.data, DMA);
}

/* -----------------------------------------------------------------------------
   Main
   ----------------------------------------------------------------------------- */
int main(bool hardReset)
{
    if (!hardReset)
        SYS_hardReset();

    DMA_setMaxQueueSize(192);
    JOY_init();
    SPR_init();
    setupWindowFullScreen();
    loadAllPalettes();

    g_cursorSprite = SPR_addSprite(&menu_cursor_sprite,
                                   -32,
                                   -32,
                                   TILE_ATTR_FULL(PAL0, 1, FALSE, FALSE, 0));
    if (g_cursorSprite)
        SPR_setVisibility(g_cursorSprite, HIDDEN);

    updateCursorSprite();

    loadScreen(g_selectedScreen);
    SPR_update();
    SYS_doVBlankProcess();

    while (TRUE)
    {
        const u16 padState = JOY_readJoypad(JOY_1);
        const u16 pressed = (u16)((padState ^ g_previousPad) & padState);

        g_frameCount++;
        g_previousPad = padState;

        handleInput(pressed, padState);
        updateActiveGauges();
        SPR_update();
        SYS_doVBlankProcess();
    }

    return 0;
}
