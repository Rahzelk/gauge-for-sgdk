
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
#  Visual mnemonic for a HORIZONTAL gauge (8 pixels wide):
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

TILESET gauge_yellow_strip_h_segment1 "gauge_fill_strip_horizontal.png" NONE NONE 
TILESET gauge_yellow_strip_v_segment1 "gauge_fill_strip_vertical.png"   NONE NONE 

TILESET gauge_yellow_strip_h_segment_trail "gauge_fill_strip_horizontal_trail.png" NONE NONE 
TILESET gauge_yellow_strip_h_segment_end "gauge_fill_strip_horizontal-terminaison_trail.png" NONE NONE 
TILESET gauge_yellow_strip_h_segment_body "gauge_fill_strip_horizontal-frontier.png"     NONE NONE 

TILESET gauge_blue_strip_h_segment1 "gauge_fill_strip_blue_horizontal.png" NONE NONE 
TILESET gauge_blue_strip_v_segment1 "gauge_fill_strip_blue_vertical.png" NONE NONE 

TILESET gauge_b1_strip_h_segment1 "gauge_fill_strip_horizontal_b1.png" NONE NONE 
TILESET gauge_b1_strip_v_segment1 "gauge_fill_strip_vertical_b1.png" NONE NONE 
TILESET gauge_b2_strip_h_segment1 "gauge_fill_strip_horizontal_b2.png" NONE NONE 
TILESET gauge_b2_strip_v_segment1 "gauge_fill_strip_vertical_b2.png" NONE NONE 

PALETTE gauge_palette "gauge_fill_strip_horizontal.png" NONE NONE 
