port module Main exposing (main)

import Color exposing (black, grey)
import Element exposing (button, circle, column, el, text, viewport)
import Element.Attributes exposing (center, height, verticalCenter, width, percent)
import Element.Events exposing (onClick)
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, field, list, bool, map2, map6, map, string, decodeValue)
import Json.Encode as Encode exposing (Value)
import Style exposing (StyleSheet, style, styleSheet)
import Style.Color as Color
import Style.Border as Border
import WebSocket


main : Program ( Value, Maybe String, String ) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { wsApi } =
    Sub.batch
        [ WebSocket.listen wsApi CbWebsocketMessage
        , cbEncrypt CbEncrypt
        , cbDecrypt CbDecrypt
        ]



-- PORTS


port decrypt : String -> Cmd msg


port encrypt : ( String, String ) -> Cmd msg


port cbDecrypt : (String -> msg) -> Sub msg


port cbEncrypt : (String -> msg) -> Sub msg



-- MODEL


type PublicKey
    = PublicKey PublicKeyRecord String


type PairId
    = PairId String


type RoomId
    = RoomId String


type Status
    = PreInit PublicKey
    | WaitingForB PublicKey String String
    | Swapping String
    | Ready String PublicKey


type alias Model =
    { status : Status
    , messages : List String
    , input : String
    , wsApi : String
    }


type alias PublicKeyRecord =
    { alg : String
    , e : String
    , ext : Bool
    , key_ops : List String
    , kty : String
    , n : String
    }


type SocketText
    = Waiting String String
    | Message String
    | Error String
    | Start String
    | Key PublicKeyRecord



-- INIT


init : ( Value, Maybe String, String ) -> ( Model, Cmd Msg )
init ( jwk, maybeRoomId, url ) =
    let
        publicKeyRecord =
            jwk
                |> decodeValue decodePublicKey
                -- TODO Fail the app startup if this doesn't succeed
                |> Result.withDefault
                    { alg = ""
                    , e = ""
                    , ext = True
                    , key_ops = []
                    , kty = ""
                    , n = ""
                    }

        pkString =
            encodePublicKey publicKeyRecord
                |> Encode.encode 0

        cmd =
            case maybeRoomId of
                Just id ->
                    let
                        json =
                            Encode.object [ ( "roomId", Encode.string id ) ]
                                |> Encode.encode 0
                    in
                        WebSocket.send url json

                Nothing ->
                    Cmd.none
    in
        { status = PreInit (PublicKey publicKeyRecord pkString)
        , input = ""
        , messages = []
        , wsApi = url
        }
            ! [ cmd ]



-- MESSAGES


type Msg
    = Init
    | CbWebsocketMessage String
    | InputChange String
    | Send
    | CbEncrypt String
    | CbDecrypt String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            let
                json =
                    Encode.object [ ( "init", Encode.bool True ) ]
                        |> Encode.encode 0
            in
                model
                    ! [ WebSocket.send model.wsApi json ]

        InputChange str ->
            { model | input = str } ! []

        Send ->
            case model.status of
                Ready _ (PublicKey _ k) ->
                    model ! [ encrypt ( model.input, k ) ]

                a ->
                    model ! [ log "send, oops" a ]

        CbEncrypt txt ->
            case model.status of
                Ready id _ ->
                    let
                        json =
                            Encode.object
                                [ ( "conn", Encode.string id )
                                , ( "message", Encode.string txt )
                                ]
                                |> Encode.encode 0
                    in
                        { model | input = "" }
                            ! [ WebSocket.send model.wsApi json ]

                a ->
                    model ! [ log "cbEncrypt, oops" a ]

        CbDecrypt txt ->
            { model | messages = txt :: model.messages } ! []

        CbWebsocketMessage str ->
            case Decode.decodeString decodeSocketText str of
                Ok socketMsg ->
                    case socketMsg of
                        Waiting bId room ->
                            case model.status of
                                PreInit pk ->
                                    { model | status = WaitingForB pk bId room } ! []

                                a ->
                                    model ! [ log "waiting, oops" a ]

                        Message txt ->
                            model ! [ decrypt txt ]

                        Error err ->
                            model ! [ log "socket server error" err ]

                        Start id ->
                            case model.status of
                                PreInit (PublicKey mPk _) ->
                                    let
                                        pk =
                                            encodePublicKey mPk

                                        json =
                                            Encode.object
                                                [ ( "conn", Encode.string id )
                                                , ( "key", pk )
                                                ]
                                                |> Encode.encode 0
                                    in
                                        { model | status = Swapping id }
                                            ! [ WebSocket.send model.wsApi json ]

                                a ->
                                    model ! [ log "start, oops" a ]

                        Key k ->
                            case model.status of
                                WaitingForB (PublicKey mPk _) id _ ->
                                    let
                                        pk =
                                            encodePublicKey mPk

                                        json =
                                            Encode.object
                                                [ ( "conn", Encode.string id )
                                                , ( "key", pk )
                                                ]
                                                |> Encode.encode 0

                                        tPk =
                                            encodePublicKey k
                                                |> Encode.encode 0
                                    in
                                        { model
                                            | status = Ready id (PublicKey k tPk)
                                        }
                                            ! [ WebSocket.send model.wsApi json ]

                                Swapping id ->
                                    let
                                        tPk =
                                            encodePublicKey k
                                                |> Encode.encode 0
                                    in
                                        { model | status = Ready id (PublicKey k tPk) } ! []

                                a ->
                                    model ! [ log "oops" a ]

                Err err ->
                    model ! [ log "socket message error" err ]



-- VIEW


type Styles
    = Button
    | None
    | StartCircle


styling : StyleSheet Styles variation
styling =
    styleSheet
        [ style None []
        , style StartCircle [ Color.background grey ]
        , style Button [ Border.dashed, Color.border black ]
        ]


view : Model -> Html Msg
view model =
    viewport styling <|
        column None
            [ center, verticalCenter, width <| percent 100, height <| percent 100 ]
            [ case model.status of
                WaitingForB _ _ room ->
                    --newTab ("http://localhost:8080/?room-id=" ++ id) <| text "Share this link"
                    text ("http://localhost:8080/?room-id=" ++ room)

                Swapping _ ->
                    text "spinner"

                Ready _ _ ->
                    column None
                        []
                        (List.map (\x -> el None [] <| text x) model.messages
                            ++ [ Input.text None
                                    []
                                    { onChange = InputChange
                                    , value = model.input
                                    , label = Input.labelAbove <| text "say something!"
                                    , options = []
                                    }
                               , button None [ onClick Send ] <| text "send"
                               ]
                        )

                PreInit _ ->
                    circle 50
                        StartCircle
                        [ onClick Init, center, verticalCenter ]
                    <|
                        el None [ center, verticalCenter ] <|
                            text
                                "Start"
            ]



-- HELPERS


log : String -> a -> Cmd Msg
log tag a =
    let
        _ =
            Debug.log tag a
    in
        Cmd.none



-- ENCODERS & DECODERS


encodePublicKey : PublicKeyRecord -> Value
encodePublicKey { alg, e, ext, key_ops, kty, n } =
    Encode.object
        [ ( "alg", Encode.string alg )
        , ( "e", Encode.string e )
        , ( "ext", Encode.bool ext )
        , ( "key_ops", Encode.list <| List.map Encode.string key_ops )
        , ( "kty", Encode.string kty )
        , ( "n", Encode.string n )
        ]


decodePublicKey : Decoder PublicKeyRecord
decodePublicKey =
    map6 PublicKeyRecord
        (field "alg" string)
        (field "e" string)
        (field "ext" bool)
        (field "key_ops" (list string))
        (field "kty" string)
        (field "n" string)


decodeSocketText : Decoder SocketText
decodeSocketText =
    Decode.oneOf
        [ decodeWaiting
        , decodeMessage
        , decodeStart
        , decodeError
        , decodeKey
        ]


decodeWaiting : Decoder SocketText
decodeWaiting =
    map2 Waiting
        (field "id" string)
        (field "room" string)


decodeMessage : Decoder SocketText
decodeMessage =
    map Message
        (field "message" string)


decodeStart : Decoder SocketText
decodeStart =
    map Start
        (field "start" string)


decodeError : Decoder SocketText
decodeError =
    map Error
        (field "error" string)


decodeKey : Decoder SocketText
decodeKey =
    map Key
        (field "publicKey" decodePublicKey)
