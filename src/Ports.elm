port module Ports exposing (..)

import Json.Encode exposing (Value)


port log : ( String, String ) -> Cmd msg


port share : String -> Cmd msg


port decrypt : String -> Cmd msg


port encrypt : { plaintext : String, publicKey : Value } -> Cmd msg


port loadPublicKey : Value -> Cmd msg


port cbDecrypt : (String -> msg) -> Sub msg


port cbEncrypt : (String -> msg) -> Sub msg


port cbLoadPublicKey : (Value -> msg) -> Sub msg
