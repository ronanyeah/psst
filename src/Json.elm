module Json exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, fail, field, list, bool, map2, map6, map, string, succeed)
import Json.Encode as Encode
import Types exposing (ConnId(..), Flags, PublicKeyRecord, RoomId(..), ScrollData, SocketMessages(..))


decodeFlags : Decoder Flags
decodeFlags =
    map6 Flags
        (field "roomId" (Decode.nullable string))
        (field "publicKey" decodePublicKey)
        (field "origin" string)
        (field "wsUrl" string)
        (field "shareEnabled" bool)
        (field "copyEnabled" bool)


decodeScrollEvent : Decoder ScrollData
decodeScrollEvent =
    Decode.map3 ScrollData
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] (Decode.map round Decode.float))
        (Decode.at [ "target", "clientHeight" ] Decode.int)


encodePublicKey : PublicKeyRecord -> Encode.Value
encodePublicKey { alg, e, ext, key_ops, kty, n } =
    Encode.object
        [ ( "alg", Encode.string alg )
        , ( "e", Encode.string e )
        , ( "ext", Encode.bool ext )
        , ( "key_ops", Encode.list <| List.map Encode.string key_ops )
        , ( "kty", Encode.string kty )
        , ( "n", Encode.string n )
        ]


encodeDataTransmit : ConnId -> Encode.Value -> String
encodeDataTransmit (ConnId id) payload =
    [ ( "conn", Encode.string id )
    , ( "data", payload |> Encode.encode 0 |> Encode.string )
    ]
        |> Encode.object
        |> Encode.encode 0


decodePublicKey : Decoder PublicKeyRecord
decodePublicKey =
    map6 PublicKeyRecord
        (field "alg" string)
        (field "e" string)
        (field "ext" bool)
        (field "key_ops" (list string))
        (field "kty" string)
        (field "n" string)


decodeSocketText : Decoder SocketMessages
decodeSocketText =
    Decode.oneOf
        [ decodeWaiting
        , decodeStart
        , decodeError
        , decodeEnum
        , decodeKey
        , decodeMessage
        ]


decodeWaiting : Decoder SocketMessages
decodeWaiting =
    map2 Waiting
        (field "id" (map ConnId string))
        (field "room" (map RoomId string))


decodeMessage : Decoder SocketMessages
decodeMessage =
    map ReceiveMessage
        (field "message" string)


decodeStart : Decoder SocketMessages
decodeStart =
    map ReceiveAId
        (field "start" (map ConnId string))


decodeError : Decoder SocketMessages
decodeError =
    map Error
        (field "error" string)


decodeKey : Decoder SocketMessages
decodeKey =
    map Key
        (field "key" decodePublicKey)


decodeEnum : Decoder SocketMessages
decodeEnum =
    string
        |> andThen
            (\str ->
                case str of
                    "ROOM_UNAVAILABLE" ->
                        succeed RoomUnavailable

                    "TYPING" ->
                        succeed Typing

                    "DEAD" ->
                        succeed ConnectionDead

                    a ->
                        fail ("enum: \"" ++ a ++ "\" not recognised")
            )
