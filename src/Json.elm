module Json exposing (decodeChatCreated, decodeEnum, decodeKey, decodeKeyAndConn, decodeMessage, decodePublicKey, decodeScrollEvent, decodeSocketText, encodeDataTransmit, encodePublicKey)

import Json.Decode as Decode exposing (Decoder, andThen, bool, fail, field, list, map, map2, map6, string, succeed)
import Json.Encode as Encode
import Types exposing (ConnId(..), CryptoKey, ScrollData, SocketMessage(..))


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
        , ( "key_ops", Encode.list Encode.string key_ops )
        , ( "kty", Encode.string kty )
        , ( "n", Encode.string n )
        ]


encodeDataTransmit : ConnId -> Encode.Value -> String
encodeDataTransmit (ConnId id) payload =
    [ ( "conn", Encode.string id )
    , ( "contents", payload |> Encode.encode 0 |> Encode.string )
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
        , decodeKeyAndConn
        , decodeKey
        , decodeMessage
        , decodeChatCreated
        , decodeChatMatched
        ]


decodeChatCreated : Decoder SocketMessage
decodeChatCreated =
    map ConnId string
        |> field "chatId"
        |> map ChatCreated


decodeChatMatched : Decoder SocketMessage
decodeChatMatched =
    map2 (\aId bId -> ChatMatched { aId = aId, bId = bId })
        (field "aId" (map ConnId string))
        (field "bId" (map ConnId string))


decodeMessage : Decoder SocketMessage
decodeMessage =
    map ReceiveMessage
        (field "message" string)


decodeKey : Decoder SocketMessage
decodeKey =
    map Key
        (field "key" decodePublicKey)


decodeKeyAndConn : Decoder SocketMessage
decodeKeyAndConn =
    map2 KeyAndConn
        (field "key" decodePublicKey)
        (field "pairId" (map ConnId string))


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
