port module Ports exposing (..)


port decrypt : String -> Cmd msg


port encrypt : String -> Cmd msg


port loadPublicKey : String -> Cmd msg


port cbDecrypt : (String -> msg) -> Sub msg


port cbEncrypt : (String -> msg) -> Sub msg
