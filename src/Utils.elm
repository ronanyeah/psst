module Utils exposing (..)

import Element exposing (Element, none)


when : Bool -> Element msg -> Element msg
when bool view =
    if bool then
        view
    else
        none


log : String -> a -> Cmd msg
log tag a =
    Debug.log tag a |> always Cmd.none
