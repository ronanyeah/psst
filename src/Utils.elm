module Utils exposing (..)

import Element exposing (Element, none)
import Ports


when : Bool -> Element msg -> Element msg
when bool view =
    if bool then
        view
    else
        none


log : String -> a -> Cmd msg
log tag a =
    Ports.log ( tag, toString a )
