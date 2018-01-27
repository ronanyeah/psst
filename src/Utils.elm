module Utils exposing (..)

import Element exposing (Element, empty)


when : Bool -> Element msg -> Element msg
when bool view =
    if bool then
        view
    else
        empty
