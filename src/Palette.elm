module Palette exposing (background, hovered, referenceHover, sectionHeading, selected, white)

import Element exposing (Color, rgb)


background : Color
background =
    rgb 0.9 0.9 0.95


white : Color
white =
    rgb 1.0 1.0 1.0


selected : Color
selected =
    rgb 0.8 0.8 1.0


hovered : Color
hovered =
    rgb 1.0 1.0 0.8


referenceHover : Color
referenceHover =
    rgb 1.0 1.0 0.9


sectionHeading : Color
sectionHeading =
    rgb 0.9 0.9 1.0
