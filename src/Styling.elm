module Styling exposing (Styles(..), styling)

import Color exposing (black, rgb)
import Style.Font as Font
import Style exposing (StyleSheet, style, styleSheet)
import Style.Color as Color
import Style.Border as Border


type Styles
    = Button
    | Body
    | Link
    | None
    | StartCircle
    | ShareThis


font : Style.Property class variation
font =
    Font.typeface [ Font.font "VT323" ]


grn : Color.Color
grn =
    rgb 129 255 42


org : Color.Color
org =
    rgb 255 169 15


prp : Color.Color
prp =
    rgb 191 0 253


blu : Color.Color
blu =
    rgb 0 165 254


pnk : Color.Color
pnk =
    rgb 255 0 166


ylw : Color.Color
ylw =
    rgb 251 255 37


red : Color.Color
red =
    rgb 255 0 0


styling : StyleSheet Styles variation
styling =
    styleSheet
        [ style None []
        , style Body [ Color.background grn, font ]
        , style Link [ Color.background ylw ]
        , style ShareThis [ Color.background blu, Font.size 20 ]
        , style StartCircle
            [ Color.background org
            , Font.size 40
            , Style.pseudo "active"
                [ Color.background red ]
            ]
        , style Button
            [ Border.dashed
            , Color.border black
            , Color.background org
            , Font.size 20
            , font
            , Style.pseudo "active"
                [ Color.background red ]
            ]
        ]
