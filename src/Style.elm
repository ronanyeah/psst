module Style exposing (black, blu, font, grn, msgSelf, msgSys, msgThem, org, pnk, prp, red, ylw)

import Element exposing (Attribute, Color, rgb255)
import Element.Background as Bg
import Element.Font as Font


font : Attribute msg
font =
    Font.family
        [ Font.typeface "VT323"
        , Font.sansSerif
        ]


black : Color
black =
    rgb255 0 0 0


grn : Color
grn =
    rgb255 129 255 42


org : Color
org =
    rgb255 255 169 15


prp : Color
prp =
    rgb255 191 0 253


blu : Color
blu =
    rgb255 0 165 254


pnk : Color
pnk =
    rgb255 255 0 166


ylw : Color
ylw =
    rgb255 251 255 37


red : Color
red =
    rgb255 255 0 0


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
