module Json exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, bool, fail, field, list, map, map2, map6, nullable, string, succeed)
import Json.Encode as Encode
import Types exposing (ChatCreate, ChatId(..), ChatJoin, ConnId(..), CryptoKey, Flags, ScrollData, SocketMessage(..))


encodeChatId : ChatId -> Encode.Value
encodeChatId (ChatId chatId) =
    Encode.string chatId


decodeFlags : Decoder (Maybe Flags)
decodeFlags =
    Decode.map7 Flags
        (field "maybeChatId" (nullable string))
        (field "origin" string)
        (field "wsUrl" string)
        (field "restUrl" string)
        (field "shareEnabled" bool)
        (field "copyEnabled" bool)
        (field "publicKey" decodePublicKey)
        |> Decode.nullable


decodeScrollEvent : Decoder ScrollData
decodeScrollEvent =
    Decode.map3 ScrollData
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] (Decode.map round Decode.float))
        (Decode.at [ "target", "clientHeight" ] Decode.int)


encodePublicKey : CryptoKey -> Encode.Value
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


decodePublicKey : Decoder CryptoKey
decodePublicKey =
    map6 CryptoKey
        (field "alg" string)
        (field "e" string)
        (field "ext" bool)
        (field "key_ops" (list string))
        (field "kty" string)
        (field "n" string)


decodeSocketText : Decoder SocketMessage
decodeSocketText =
    Decode.oneOf
        [ decodeEnum
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
