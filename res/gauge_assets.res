
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

TILESET gauge_h_straight_yellow_strip "gauge_h_straight_yellow_strip.png" NONE NONE 
TILESET gauge_h_pip_basic_strip "gauge_h_pip_basic_strip.png" NONE NONE 
TILESET gauge_v_straight_yellow_strip "gauge_v_straight_yellow_strip.png"   NONE NONE 
TILESET gauge_h_straight_small_blue_strip "gauge_h_straight_small_blue_strip.png" NONE NONE 

TILESET gauge_h_bevel_yellow_strip_trail "gauge_h_bevel_yellow_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_yellow_strip_end "gauge_h_bevel_yellow_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_strip_break "gauge_h_bevel_yellow_strip_break.png"     NONE NONE 

TILESET gauge_h_bevel_blue_strip_break "gauge_h_bevel_blue_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_blue_strip_end "gauge_h_bevel_blue_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_blue_strip_trail "gauge_h_bevel_blue_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_blue_to_yellow_strip_bridge "gauge_h_bevel_blue_to_yellow_strip_bridge.png" NONE NONE 

TILESET gauge_h_bevel_lightblue_strip_break "gauge_h_bevel_lightblue_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_lightblue_strip_end "gauge_h_bevel_lightblue_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_lightblue_strip_trail "gauge_h_bevel_lightblue_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_lightblue_to_blue_strip_bridge "gauge_h_bevel_lightblue_to_blue_strip_bridge.png" NONE NONE 

TILESET gauge_h_bevel_yellow_gain_strip_trail "gauge_h_bevel_yellow_gain_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_yellow_gain_strip_end "gauge_h_bevel_yellow_gain_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_gain_strip_break "gauge_h_bevel_yellow_gain_strip_break.png"     NONE NONE 

TILESET gauge_h_bevel_blue_gain_strip_break "gauge_h_bevel_blue_gain_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_blue_gain_strip_end "gauge_h_bevel_blue_gain_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_blue_gain_strip_trail "gauge_h_bevel_blue_gain_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_blue_to_yellow_gain_strip_bridge "gauge_h_bevel_blue_to_yellow_gain_strip_bridge.png" NONE NONE 

TILESET gauge_h_bevel_lightblue_gain_strip_break "gauge_h_bevel_lightblue_gain_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_lightblue_gain_strip_end "gauge_h_bevel_lightblue_gain_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_lightblue_gain_strip_trail "gauge_h_bevel_lightblue_gain_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_lightblue_gain_to_blue_strip_bridge "gauge_h_bevel_lightblue_to_blue_gain_strip_bridge.png" NONE NONE 
 


TILESET gauge_h_bevel_yellow_with_border_cap_start_strip_end "gauge_h_bevel_yellow_with_border_cap_start_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_cap_end_strip_end "gauge_h_bevel_yellow_with_border_cap_end_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_cap_start_strip_trail "gauge_h_bevel_yellow_with_border_cap_start_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_cap_start_strip_break "gauge_h_bevel_yellow_with_border_cap_start_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_strip_break "gauge_h_bevel_yellow_with_border_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_strip_end "gauge_h_bevel_yellow_with_border_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_strip_trail "gauge_h_bevel_yellow_with_border_strip_trail.png" NONE NONE 

TILESET gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end "gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end "gauge_h_bevel_yellow_with_border_cap_end_blink_off_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail "gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_break "gauge_h_bevel_yellow_with_border_cap_start_blink_off_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_blink_off_strip_break "gauge_h_bevel_yellow_with_border_blink_off_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_blink_off_strip_end "gauge_h_bevel_yellow_with_border_blink_off_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_yellow_with_border_blink_off_strip_trail "gauge_h_bevel_yellow_with_border_blink_off_strip_trail.png" NONE NONE 

TILESET gauge_h_bevel_blue_with_border_strip_break "gauge_h_bevel_blue_with_border_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_blue_with_border_strip_end "gauge_h_bevel_blue_with_border_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_blue_with_border_strip_trail "gauge_h_bevel_blue_with_border_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_yellow_to_blue_with_border_strip_bridge "gauge_h_bevel_yellow_to_blue_with_border_strip_bridge.png" NONE NONE 
TILESET gauge_h_bevel_blue_to_yellow_with_border_strip_bridge "gauge_h_bevel_blue_to_yellow_with_border_strip_bridge.png" NONE NONE 

TILESET gauge_h_bevel_blue_with_border_blink_off_strip_break "gauge_h_bevel_blue_with_border_blink_off_strip_break.png" NONE NONE 
TILESET gauge_h_bevel_blue_with_border_blink_off_strip_end "gauge_h_bevel_blue_with_border_blink_off_strip_end.png" NONE NONE 
TILESET gauge_h_bevel_blue_with_border_blink_off_strip_trail "gauge_h_bevel_blue_with_border_blink_off_strip_trail.png" NONE NONE 
TILESET gauge_h_bevel_yellow_to_blue_with_border_blink_off_strip_bridge "gauge_h_bevel_yellow_to_blue_with_border_blink_off_strip_bridge.png" NONE NONE 
TILESET gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge "gauge_h_bevel_blue_to_yellow_with_border_blink_off_strip_bridge.png" NONE NONE 


TILESET gauge_h_straight_blue_strip "gauge_h_straight_blue_strip.png" NONE NONE 
TILESET gauge_v_straight_blue_strip "gauge_v_straight_blue_strip.png" NONE NONE 
TILESET gauge_h_straight_lightblue_strip "gauge_h_straight_lightblue_strip.png" NONE NONE 
TILESET gauge_v_straight_lightblue_strip "gauge_v_straight_lightblue_strip.png" NONE NONE 

PALETTE gauge_palette "gauge_h_straight_yellow_strip.png" NONE NONE 
