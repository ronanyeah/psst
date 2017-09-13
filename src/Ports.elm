port module Ports exposing (..)

import Json.Encode


port share : String -> Cmd msg


port decrypt : String -> Cmd msg


port encrypt : String -> Cmd msg


port loadPublicKey : Json.Encode.Value -> Cmd msg


port cbDecrypt : (String -> msg) -> Sub msg


port cbEncrypt : (String -> msg) -> Sub msg


port cbLoadPublicKey : (() -> msg) -> Sub msg
