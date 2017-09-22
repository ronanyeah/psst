module Json exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, fail, field, list, bool, map2, map6, map, string, succeed)
import Json.Encode as Encode
import Types exposing (ChatCreate, ChatJoin, ConnId(..), PublicKeyRecord, ChatId(..), ScrollData, SocketMessage(..))


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


decodeSocketText : Decoder SocketMessage
decodeSocketText =
    Decode.oneOf
        [ decodeError
        , decodeEnum
        , decodeKey
        , decodeMessage
        ]


decodeChatCreate : Decoder ChatCreate
decodeChatCreate =
    map2 ChatCreate
        (field "bId" (map ConnId string))
        (field "chatId" (map ChatId string))


decodeChatJoin : Decoder ChatJoin
decodeChatJoin =
    map2 ChatJoin
        (field "aId" (map ConnId string))
        (field "chatId" (map ChatId string))


decodeMessage : Decoder SocketMessage
decodeMessage =
    map ReceiveMessage
        (field "message" string)


decodeError : Decoder SocketMessage
decodeError =
    map Error
        (field "error" string)


decodeKey : Decoder SocketMessage
decodeKey =
    map Key
        (field "key" decodePublicKey)


decodeEnum : Decoder SocketMessage
decodeEnum =
    string
        |> andThen
            (\str ->
                case str of
                    "ROOM_UNAVAILABLE" ->
                        succeed ChatUnavailable

                    "TYPING" ->
                        succeed Typing

                    "DEAD" ->
                        succeed ConnectionDead

                    a ->
                        fail ("enum: \"" ++ a ++ "\" not recognised")
            )
