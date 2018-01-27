module Style exposing (..)

import Color exposing (Color, black, rgb)
import Element exposing (Attribute)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font


font : Attribute msg
font =
    Font.family
        [ Font.external
            { name = "VT323"
            , url = "https://fonts.googleapis.com/css?family=VT323"
            }
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


deadConn : List (Attribute msg)
deadConn =
    [ Bg.color red, font ]


msgThem : List (Attribute msg)
msgThem =
    [ Bg.color ylw
    , Font.size 35
    , Font.alignLeft
    ]


messageBox : List (Attribute msg)
messageBox =
    [ Border.rounded 10
    , Border.dashed
    , Border.color black
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


button : List (Attribute msg)
button =
    [ Border.dashed
    , Border.color black
    , Bg.color org
    , Font.size 30
    , font
    ]
