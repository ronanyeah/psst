module Styling exposing (Styles(..), Variations(..), styling)

import Color exposing (black, rgb)
import Style.Font as Font
import Style exposing (StyleSheet, style, styleSheet, variation)
import Style.Color as Color
import Style.Border as Border


type Styles
    = Button
    | Body
    | DeadConn
    | Link
    | MessageBox
    | MsgSelf
    | MsgThem
    | MsgSys
    | None
    | StartCircle
    | ShareThis


type Variations
    = Cool


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


styling : StyleSheet Styles Variations
styling =
    styleSheet
        [ style None []
        , style Body [ Color.background grn, font ]
        , style DeadConn [ Color.background red, font ]
        , style Link [ Color.background ylw ]
        , style MsgThem
            [ Color.background ylw
            , Font.size 35
            , Font.alignLeft
            ]
        , style MessageBox
            [ Border.rounded 10
            , Border.dashed
            , Color.border black
            ]
        , style MsgSelf
            [ Color.background org
            , Font.size 35
            , Font.alignRight
            ]
        , style MsgSys
            [ Color.background pnk
            , Font.size 35
            , Font.center
            ]
        , style ShareThis [ Color.background blu, Font.size 20 ]
        , style StartCircle
            [ Color.background org
            , Font.size 40
            , Style.pseudo "active"
                [ Color.background red ]
            ]
        , style Button
            [ Border.dashed
            , Border.rounded 10
            , Color.border black
            , Color.background org
            , Font.size 30
            , font
            , Style.pseudo "active"
                [ Color.background red ]
            ]
        ]
