port module Main exposing (main)

import Element exposing (Element, button, circle, column, el, row, text, screen, viewport)
import Element.Attributes exposing (alignBottom, alignLeft, attribute, center, class, height, padding, px, spacing, verticalCenter, width, percent)
import Element.Events exposing (onClick)
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, andThen, fail, field, list, bool, map2, map6, map, string, decodeValue, succeed)
import Json.Encode as Encode exposing (Value)
import Navigation exposing (newUrl)
import Styling exposing (Styles(..), styling)
import Task
import WebSocket
import Window


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


type alias Model =
    { status : Status
    , messages : List Message
    , input : String
    , wsApi : String
    , device : Element.Device
    }



-- TYPES


type alias Message =
    { self : Bool
    , content : String
    }


type PublicKeyString
    = PublicKeyString String


type ConnId
    = ConnId String


type RoomId
    = RoomId String


type alias PublicKeyRecord =
    { alg : String
    , e : String
    , ext : Bool
    , key_ops : List String
    , kty : String
    , n : String
    }


type Status
    = Start PublicKeyRecord
    | WaitingForBKey PublicKeyRecord ConnId RoomId
    | Joining PublicKeyRecord
    | WaitingForAKey ConnId
    | Ready ConnId PublicKeyString


type SocketMessages
    = Waiting ConnId RoomId
    | ReceiveMessage String
    | Error String
    | ReceiveAId ConnId
    | Key PublicKeyRecord
    | RoomUnavailable



-- INIT


init : ( Value, Maybe String, String ) -> ( Model, Cmd Msg )
init ( jwk, maybeRoomId, url ) =
    let
        myPublicKey =
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

        ( status, cmd ) =
            case maybeRoomId of
                Just id ->
                    let
                        roomJoinRequest =
                            Encode.object [ ( "roomId", Encode.string id ) ]
                                |> Encode.encode 0
                                |> WebSocket.send url
                    in
                        ( Joining myPublicKey, roomJoinRequest )

                Nothing ->
                    ( Start myPublicKey, Cmd.none )
    in
        { status = status
        , input = ""
        , messages = []
        , wsApi = url
        , device =
            { width = 0
            , height = 0
            , phone = False
            , tablet = False
            , desktop = False
            , bigDesktop = False
            , portrait = False
            }
        }
            ! [ cmd
              , Task.perform Resize Window.size
              ]



-- MESSAGES


type Msg
    = Init
    | CbWebsocketMessage String
    | InputChange String
    | Send
    | CbEncrypt String
    | CbDecrypt String
    | Resize Window.Size



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            let
                json =
                    Encode.string "START"
                        |> Encode.encode 0
            in
                model
                    ! [ WebSocket.send model.wsApi json ]

        InputChange str ->
            { model | input = str } ! []

        Send ->
            case model.status of
                Ready _ (PublicKeyString key) ->
                    { model
                        | input = ""
                        , messages = model.messages ++ [ { self = True, content = model.input } ]
                    }
                        ! [ encrypt ( model.input, key ) ]

                a ->
                    model ! [ log "send, oops" a ]

        Resize size ->
            { model | device = Element.classifyDevice size } ! []

        CbEncrypt txt ->
            case model.status of
                Ready (ConnId id) _ ->
                    let
                        json =
                            Encode.object
                                [ ( "conn", Encode.string id )
                                , ( "message", Encode.string txt )
                                ]
                                |> Encode.encode 0
                    in
                        model
                            ! [ WebSocket.send model.wsApi json ]

                a ->
                    model ! [ log "cbEncrypt, oops" a ]

        CbDecrypt txt ->
            { model | messages = model.messages ++ [ { self = False, content = txt } ] } ! []

        CbWebsocketMessage str ->
            case Decode.decodeString decodeSocketText str of
                Ok socketMsg ->
                    case socketMsg of
                        Waiting bId room ->
                            case model.status of
                                Start pk ->
                                    { model | status = WaitingForBKey pk bId room } ! []

                                a ->
                                    model ! [ log "waiting, oops" a ]

                        ReceiveMessage txt ->
                            model ! [ decrypt txt ]

                        Error err ->
                            model ! [ log "socket server error" err ]

                        RoomUnavailable ->
                            case model.status of
                                Joining myPublicKey ->
                                    { model | status = Start myPublicKey }
                                        ! [ log "room unavailable" 0, newUrl "/" ]

                                a ->
                                    model ! [ log "room unavailable, oops" a ]

                        ReceiveAId (ConnId id) ->
                            case model.status of
                                Joining myPublicKey ->
                                    let
                                        json =
                                            Encode.object
                                                [ ( "conn", Encode.string id )
                                                , ( "key", encodePublicKey myPublicKey )
                                                ]
                                                |> Encode.encode 0
                                    in
                                        { model | status = WaitingForAKey (ConnId id) }
                                            ! [ WebSocket.send model.wsApi json ]

                                a ->
                                    model ! [ log "start, oops" a ]

                        Key k ->
                            case model.status of
                                WaitingForBKey myPublicKey (ConnId id) _ ->
                                    let
                                        json =
                                            Encode.object
                                                [ ( "conn", Encode.string id )
                                                , ( "key", encodePublicKey myPublicKey )
                                                ]
                                                |> Encode.encode 0

                                        theirPk =
                                            encodePublicKey k
                                                |> Encode.encode 0
                                    in
                                        { model
                                            | status = Ready (ConnId id) (PublicKeyString theirPk)
                                        }
                                            ! [ WebSocket.send model.wsApi json ]

                                WaitingForAKey id ->
                                    let
                                        theirPk =
                                            encodePublicKey k
                                                |> Encode.encode 0
                                    in
                                        { model | status = Ready id (PublicKeyString theirPk) } ! []

                                a ->
                                    model ! [ log "key swap, oops" a ]

                Err err ->
                    model ! [ log "socket message error" err ]



-- VIEW


msgCard : Message -> Element Styles variation msg
msgCard { self, content } =
    column None
        [ alignLeft ]
        [ el None [] <|
            text
                (if self then
                    "Me:"
                 else
                    "Them:"
                )
        , el None [] <| text content
        ]


view : Model -> Html Msg
view { status, device, messages, input } =
    viewport styling <|
        column Body
            [ center, verticalCenter, width <| percent 100, height <| percent 100 ]
            [ case status of
                WaitingForBKey _ _ (RoomId roomId) ->
                    column None
                        [ center, spacing 20 ]
                        [ el ShareThis [ padding 5 ] <| text "Share this link with someone to begin chat:"
                        , el Link [ padding 10 ] <| text ("http://localhost:8080?room-id=" ++ roomId)
                        , button Button [ padding 10, class "copy-button", attribute "data-clipboard-text" ("http://localhost:8080?room-id=" ++ roomId) ] <|
                            text "COPY"
                        ]

                WaitingForAKey _ ->
                    text "spinner"

                Joining _ ->
                    text "spinner"

                Ready _ _ ->
                    column None
                        []
                        (List.map msgCard messages
                            ++ [ screen <|
                                    el None
                                        [ alignBottom ]
                                    <|
                                        row None
                                            []
                                            [ Input.text None
                                                [ height <| px 40
                                                , width <| px <| (device.width |> toFloat |> flip (/) 4 |> (*) 3)
                                                ]
                                                { onChange = InputChange
                                                , value = input
                                                , label = Input.hiddenLabel "input"
                                                , options = []
                                                }
                                            , button None
                                                [ onClick Send
                                                , width <| px <| (device.width |> toFloat |> flip (/) 4)
                                                , height <| px 40
                                                ]
                                              <|
                                                text "send"
                                            ]
                               ]
                        )

                Start _ ->
                    circle (device.width |> toFloat |> flip (/) 3)
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


decodeSocketText : Decoder SocketMessages
decodeSocketText =
    Decode.oneOf
        [ decodeWaiting
        , decodeMessage
        , decodeStart
        , decodeError
        , decodeKey
        , decodeEnum
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
        (field "publicKey" decodePublicKey)


decodeEnum : Decoder SocketMessages
decodeEnum =
    string
        |> andThen
            (\str ->
                case str of
                    "ROOM_UNAVAILABLE" ->
                        succeed RoomUnavailable

                    a ->
                        fail ("enum: \"" ++ a ++ "\" not recognised")
            )
