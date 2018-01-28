module Utils exposing (..)

import Element exposing (Element, empty)


when : Bool -> Element msg -> Element msg
when bool view =
    if bool then
        view
    else
        empty


log : String -> a -> Cmd msg
log tag a =
    let
        _ =
            Debug.log tag a
    in
    Cmd.none
