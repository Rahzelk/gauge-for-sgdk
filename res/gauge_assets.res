
#=============================================================================
#   ROM ASSETS ORDERING (45 tiles per segment per lane)
#   -----------------------------------------------------------------------------
#   For each segmentId and lane used, provide a ROM strip of EXACTLY 45 tiles,
#  ordered as:
#
#     for valuePxInTile = 0..8:
#      for trailPxInTile = valuePxInTile..8:
#        append tile(valuePxInTile, trailPxInTile)
#
#  Visual mnemonic for a h_bevel gauge (8 pixels wide):
#    V = primary pixels (value)
#    T = trailing pixels (trail-only)
#    . = empty/background
#
#    value=3, trail=6 -> "VVVTTT.."
#
#  For a VERTICAL gauge, the ordering is IDENTICAL, but your tiles must represent
#  vertical fill (e.g., pixels from bottom to top, depending on your art).
#  You cannot rotate tiles 90° on Mega Drive BG, so vertical gauges need dedicated
#  vertical tile strips.
#=============================================================================

TILESET gauge_h_straight_yellow_strip "tilesets/gauge_h_straight_yellow_strip.png" NONE NONE
TILESET gauge_h_straight_yellow_strip_gain "tilesets/gauge_h_straight_yellow_strip_gain.png" NONE NONE
TILESET gauge_h_straight_yellow_strip_blinkoff "tilesets/gauge_h_straight_yellow_strip_blinkoff.png" NONE NONE
TILESET gauge_h_pip_basic_strip "tilesets/gauge_h_pip_basic_strip.png" NONE NONE
TILESET gauge_h_pip_strip "tilesets/gauge_h_pip_strip.png" NONE NONE
TILESET gauge_h_pip_double_strip "tilesets/gauge_h_pip_double_strip.png" NONE NONE
TILESET gauge_h_pip_double_half_strip "tilesets/gauge_h_pip_double_half_strip.png" NONE NONE
TILESET gauge_h_pip_double_quarter_strip "tilesets/gauge_h_pip_double_quarter_strip.png" NONE NONE
TILESET gauge_h_pip_mini_bar_strip "tilesets/gauge_h_pip_mini_bar_strip.png" NONE NONE
TILESET gauge_v_straight_yellow_strip "tilesets/gauge_v_straight_yellow_strip.png"   NONE NONE
TILESET gauge_v_straight_yellow_strip_gain "tilesets/gauge_v_straight_yellow_strip_gain.png" NONE NONE
TILESET gauge_v_straight_yellow_strip_blinkoff "tilesets/gauge_v_straight_yellow_strip_blinkoff.png" NONE NONE
TILESET gauge_h_straight_small_blue_strip "tilesets/gauge_h_straight_small_blue_strip.png" NONE NONE
SPRITE menu_cursor_sprite "tilesets/menu-cursor.png" 1 1 NONE




TILESET gauge_h_bevel_yellow_strip_trail "tilesets/gauge_h_bevel_yellow_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_yellow_strip_end "tilesets/gauge_h_bevel_yellow_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_strip_body "tilesets/gauge_h_bevel_yellow_strip_body.png"     NONE NONE



TILESET gauge_h_bevel_blue_strip_body "tilesets/gauge_h_bevel_blue_strip_body.png" NONE NONE
TILESET gauge_h_bevel_blue_strip_end "tilesets/gauge_h_bevel_blue_strip_end.png" NONE NONE
TILESET gauge_h_bevel_blue_strip_trail "tilesets/gauge_h_bevel_blue_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_blue_to_yellow_strip_bridge "tilesets/gauge_h_bevel_blue_to_yellow_strip_bridge.png" NONE NONE

TILESET gauge_h_bevel_lightblue_strip_body "tilesets/gauge_h_bevel_lightblue_strip_body.png" NONE NONE
TILESET gauge_h_bevel_lightblue_strip_end "tilesets/gauge_h_bevel_lightblue_strip_end.png" NONE NONE
TILESET gauge_h_bevel_lightblue_strip_trail "tilesets/gauge_h_bevel_lightblue_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_lightblue_to_blue_strip_bridge "tilesets/gauge_h_bevel_lightblue_to_blue_strip_bridge.png" NONE NONE

TILESET gauge_h_bevel_yellow_gain_strip_trail "tilesets/gauge_h_bevel_yellow_gain_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_yellow_gain_strip_end "tilesets/gauge_h_bevel_yellow_gain_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_gain_strip_body "tilesets/gauge_h_bevel_yellow_gain_strip_body.png"     NONE NONE

TILESET gauge_h_bevel_blue_gain_strip_body "tilesets/gauge_h_bevel_blue_gain_strip_body.png" NONE NONE
TILESET gauge_h_bevel_blue_gain_strip_end "tilesets/gauge_h_bevel_blue_gain_strip_end.png" NONE NONE
TILESET gauge_h_bevel_blue_gain_strip_trail "tilesets/gauge_h_bevel_blue_gain_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_blue_to_yellow_gain_strip_bridge "tilesets/gauge_h_bevel_blue_to_yellow_gain_strip_bridge.png" NONE NONE

TILESET gauge_h_bevel_lightblue_gain_strip_body "tilesets/gauge_h_bevel_lightblue_gain_strip_body.png" NONE NONE
TILESET gauge_h_bevel_lightblue_gain_strip_end "tilesets/gauge_h_bevel_lightblue_gain_strip_end.png" NONE NONE
TILESET gauge_h_bevel_lightblue_gain_strip_trail "tilesets/gauge_h_bevel_lightblue_gain_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_lightblue_gain_to_blue_strip_bridge "tilesets/gauge_h_bevel_lightblue_to_blue_gain_strip_bridge.png" NONE NONE



TILESET gauge_h_bevel_yellow_with_border_cap_start_strip_end "tilesets/gauge_h_bevel_yellow_with_border_cap_start_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_cap_end_strip_end "tilesets/gauge_h_bevel_yellow_with_border_cap_end_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_cap_start_strip_trail "tilesets/gauge_h_bevel_yellow_with_border_cap_start_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_cap_start_strip_body "tilesets/gauge_h_bevel_yellow_with_border_cap_start_strip_body.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_strip_body "tilesets/gauge_h_bevel_yellow_with_border_strip_body.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_strip_end "tilesets/gauge_h_bevel_yellow_with_border_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_strip_trail "tilesets/gauge_h_bevel_yellow_with_border_strip_trail.png" NONE NONE

TILESET gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end "tilesets/gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end "tilesets/gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail "tilesets/gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_body "tilesets/gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_body.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_blink_off_strip_body "tilesets/gauge_h_bevel_yellow_with_border_blink_off_strip_body.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_blink_off_strip_end "tilesets/gauge_h_bevel_yellow_with_border_blink_off_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_with_border_blink_off_strip_trail "tilesets/gauge_h_bevel_yellow_with_border_blink_off_strip_trail.png" NONE NONE

TILESET gauge_h_bevel_blue_with_border_strip_body "tilesets/gauge_h_bevel_blue_with_border_strip_body.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_strip_end "tilesets/gauge_h_bevel_blue_with_border_strip_end.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_strip_trail "tilesets/gauge_h_bevel_blue_with_border_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_cap_start_strip_body "tilesets/gauge_h_bevel_blue_with_border_cap_start_strip_body.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_cap_start_strip_trail "tilesets/gauge_h_bevel_blue_with_border_cap_start_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_cap_start_strip_end "tilesets/gauge_h_bevel_blue_with_border_cap_start_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_to_blue_with_border_strip_bridge "tilesets/gauge_h_bevel_yellow_to_blue_with_border_strip_bridge.png" NONE NONE
TILESET gauge_h_bevel_blue_to_yellow_with_border_strip_bridge "tilesets/gauge_h_bevel_blue_to_yellow_with_border_strip_bridge.png" NONE NONE

TILESET gauge_h_bevel_blue_with_border_blink_off_strip_body "tilesets/gauge_h_bevel_blue_with_border_blink_off_strip_body.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_blink_off_strip_end "tilesets/gauge_h_bevel_blue_with_border_blink_off_strip_end.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_blink_off_strip_trail "tilesets/gauge_h_bevel_blue_with_border_blink_off_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_body "tilesets/gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_body.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_trail "tilesets/gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_trail.png" NONE NONE
TILESET gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_end "tilesets/gauge_h_bevel_blue_with_border_cap_start_blink_off_strip_end.png" NONE NONE
TILESET gauge_h_bevel_yellow_to_blue_with_border_blink_off_strip_bridge "tilesets/gauge_h_bevel_yellow_to_blue_with_border_blink_off_strip_bridge.png" NONE NONE
TILESET gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge "tilesets/gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge.png" NONE NONE


TILESET gauge_h_straight_blue_strip "tilesets/gauge_h_straight_blue_strip.png" NONE NONE
TILESET gauge_v_straight_blue_strip "tilesets/gauge_v_straight_blue_strip.png" NONE NONE
TILESET gauge_h_straight_lightblue_strip "tilesets/gauge_h_straight_lightblue_strip.png" NONE NONE
TILESET gauge_v_straight_lightblue_strip "tilesets/gauge_v_straight_lightblue_strip.png" NONE NONE

PALETTE gauge_palette "tilesets/gauge_h_straight_yellow_strip.png" NONE NONE
PALETTE gauge_blue_palette "tilesets/gauge_h_straight_blue_strip.png" NONE NONE
PALETTE gauge_lightblue_palette "tilesets/gauge_h_straight_lightblue_strip.png" NONE NONE
