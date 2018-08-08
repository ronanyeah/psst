module Style exposing (..)

import Color exposing (Color, rgb)
import Element exposing (Attribute)
import Element.Background as Bg
import Element.Font as Font


font : Attribute msg
font =
    Font.family
        [ Font.typeface "VT323"
        , Font.sansSerif
        ]


grn : Color
grn =
    rgb 129 255 42


org : Color
org =
    rgb 255 169 15


prp : Color
prp =
    rgb 191 0 253


blu : Color
blu =
    rgb 0 165 254


pnk : Color
pnk =
    rgb 255 0 166


ylw : Color
ylw =
    rgb 251 255 37


red : Color
red =
    rgb 255 0 0


msgThem : List (Attribute msg)
msgThem =
    [ Bg.color ylw
    , Font.size 35
    , Font.alignLeft
    ]


msgSelf : List (Attribute msg)
msgSelf =
    [ Bg.color org
    , Font.size 35
    , Font.alignRight
    ]


msgSys : List (Attribute msg)
msgSys =
    [ Bg.color pnk
    , Font.size 35
    , Font.center
    ]
