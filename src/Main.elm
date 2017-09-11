port module Main exposing (main)

import Animation
import Dom
import Dom.Scroll exposing (toBottom)
import Element exposing (Attribute, Element, button, circle, column, el, empty, image, paragraph, row, text, screen, viewport, when)
import Element.Attributes exposing (alignBottom, alignLeft, attribute, center, class, height, id, padding, px, spacing, moveUp, verticalCenter, width, percent, vary, scrollbars)
import Element.Events exposing (on, onClick, keyCode)
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, andThen, fail, field, list, bool, map2, map6, map, string, decodeValue, succeed)
import Json.Encode as Encode exposing (Value)
import Navigation exposing (newUrl)
import Styling exposing (Styles(..), Variations(..), styling)
import Task
import Time exposing (Time)
import WebSocket
import Window


main : Program ( Value, Maybe String, String, String ) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { wsApi, keySpin } =
    Sub.batch
        [ WebSocket.listen wsApi CbWebsocketMessage
        , cbEncrypt CbEncrypt
        , cbDecrypt CbDecrypt
        , Animation.subscription Animate [ keySpin ]
        , Time.every (100 * Time.millisecond) Tick
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
    , lastTyped : Time
    , lastTypedPing : Time
    , location : String
    , wsApi : String
    , device : Element.Device
    , keySpin : Animation.State
    , time : Time
    , arrow : Bool
    , scroll : ScrollStatus
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


type alias TypingAt =
    Time


type Status
    = Start PublicKeyRecord
    | WaitingForBKey PublicKeyRecord ConnId RoomId
    | Joining PublicKeyRecord
    | WaitingForAKey ConnId
    | Ready ConnId PublicKeyString TypingAt


type SocketMessages
    = Waiting ConnId RoomId
    | ReceiveMessage String
    | Error String
    | ReceiveAId ConnId
    | Key PublicKeyRecord
    | RoomUnavailable
    | Typing


type ScrollStatus
    = Static
    | Moving Time Int



-- INIT


init : ( Value, Maybe String, String, String ) -> ( Model, Cmd Msg )
init ( jwk, maybeRoomId, url, wsUrl ) =
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

        keyStart =
            Animation.style
                [ Animation.rotate <| Animation.deg 0
                ]

        ( status, keySpin, cmd ) =
            case maybeRoomId of
                Just roomId ->
                    let
                        spinInit =
                            Animation.interrupt
                                [ Animation.loop
                                    [ Animation.to
                                        [ Animation.rotate (Animation.turn 1) ]
                                    ]
                                ]
                                keyStart

                        roomJoinRequest =
                            Encode.object [ ( "roomId", Encode.string roomId ) ]
                                |> Encode.encode 0
                                |> WebSocket.send wsUrl
                    in
                        ( Joining myPublicKey, spinInit, roomJoinRequest )

                Nothing ->
                    ( Start myPublicKey
                    , keyStart
                    , Cmd.none
                    )
    in
        { status = status
        , input = ""
        , messages = []
        , lastTyped = 0
        , lastTypedPing = 0
        , location = url
        , wsApi = wsUrl
        , device =
            { width = 0
            , height = 0
            , phone = False
            , tablet = False
            , desktop = False
            , bigDesktop = False
            , portrait = False
            }
        , keySpin = keySpin
        , time = 0
        , arrow = False
        , scroll = Static
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
    | Animate Animation.Msg
    | Tick Time
    | CbScrollToBottom (Result Dom.Error ())
    | DisplayScrollButton Value
    | ScrollToBottom



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

        CbScrollToBottom _ ->
            { model | arrow = False } ! []

        InputChange str ->
            let
                ( pinged, cmd ) =
                    if (model.lastTyped - model.lastTypedPing) > 4000 then
                        case model.status of
                            Ready (ConnId connId) _ _ ->
                                ( model.time
                                , [ ( "conn", Encode.string connId )
                                  , ( "data", Encode.string ("TYPING" |> Encode.string |> Encode.encode 0) )
                                  ]
                                    |> Encode.object
                                    |> Encode.encode 0
                                    |> WebSocket.send model.wsApi
                                )

                            _ ->
                                ( model.lastTypedPing, Cmd.none )
                    else
                        ( model.lastTypedPing, Cmd.none )
            in
                { model | input = str, lastTyped = model.time, lastTypedPing = pinged } ! [ cmd ]

        Send ->
            if String.isEmpty model.input then
                model ! []
            else
                case model.status of
                    Ready _ (PublicKeyString key) _ ->
                        { model
                            | input = ""
                            , messages = model.messages ++ [ { self = True, content = model.input } ]
                        }
                            ! [ encrypt ( model.input, key ), scrollToBottom ]

                    a ->
                        model ! [ log "send, oops" a ]

        Resize size ->
            { model | device = Element.classifyDevice size } ! []

        CbEncrypt txt ->
            case model.status of
                Ready (ConnId connId) _ _ ->
                    let
                        json =
                            Encode.object
                                [ ( "conn", Encode.string connId )
                                , ( "data"
                                  , [ ( "message", Encode.string txt ) ]
                                        |> Encode.object
                                        |> Encode.encode 0
                                        |> Encode.string
                                  )
                                ]
                                |> Encode.encode 0
                    in
                        model
                            ! [ WebSocket.send model.wsApi json ]

                a ->
                    model ! [ log "cbEncrypt, oops" a ]

        CbDecrypt txt ->
            { model | messages = model.messages ++ [ { self = False, content = txt } ] }
                ! [ scrollToBottom ]

        Animate animMsg ->
            { model
                | keySpin = Animation.update animMsg model.keySpin
            }
                ! []

        Tick time ->
            { model | time = time } ! []

        ScrollToBottom ->
            model ! [ scrollToBottom ]

        DisplayScrollButton event ->
            case model.scroll of
                Static ->
                    let
                        ( h, arrow ) =
                            isBottom event
                    in
                        { model | arrow = not arrow, scroll = Moving model.time h } ! []

                Moving pre h ->
                    if (model.time - pre) > 50 then
                        let
                            ( newH, arrow ) =
                                isBottom event

                            scroll =
                                if h == newH then
                                    Static
                                else
                                    Moving model.time newH
                        in
                            { model | arrow = not arrow, scroll = scroll } ! []
                    else
                        model ! []

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

                        Typing ->
                            case model.status of
                                Ready connId key _ ->
                                    { model | status = Ready connId key model.time } ! []

                                _ ->
                                    model ! []

                        Error err ->
                            model ! [ log "socket server error" err ]

                        RoomUnavailable ->
                            case model.status of
                                Joining myPublicKey ->
                                    { model
                                        | status = Start myPublicKey
                                        , keySpin =
                                            Animation.interrupt
                                                [ Animation.set [ Animation.rotate (Animation.deg 0) ]
                                                ]
                                                model.keySpin
                                    }
                                        ! [ log "room unavailable" 0, newUrl "/" ]

                                a ->
                                    model ! [ log "room unavailable, oops" a ]

                        ReceiveAId (ConnId connId) ->
                            case model.status of
                                Joining myPublicKey ->
                                    let
                                        keyData =
                                            [ ( "key", encodePublicKey myPublicKey )
                                            ]
                                                |> Encode.object
                                                |> Encode.encode 0

                                        json =
                                            [ ( "conn", Encode.string connId )
                                            , ( "data", Encode.string keyData )
                                            ]
                                                |> Encode.object
                                                |> Encode.encode 0
                                    in
                                        { model | status = WaitingForAKey (ConnId connId) }
                                            ! [ WebSocket.send model.wsApi json ]

                                a ->
                                    model ! [ log "start, oops" a ]

                        Key k ->
                            case model.status of
                                WaitingForBKey myPublicKey (ConnId connId) _ ->
                                    let
                                        keyData =
                                            [ ( "key", encodePublicKey myPublicKey )
                                            ]
                                                |> Encode.object
                                                |> Encode.encode 0

                                        json =
                                            [ ( "conn", Encode.string connId )
                                            , ( "data", Encode.string keyData )
                                            ]
                                                |> Encode.object
                                                |> Encode.encode 0

                                        theirPk =
                                            encodePublicKey k
                                                |> Encode.encode 0
                                    in
                                        { model
                                            | status = Ready (ConnId connId) (PublicKeyString theirPk) 0
                                        }
                                            ! [ WebSocket.send model.wsApi json ]

                                WaitingForAKey connId ->
                                    let
                                        theirPk =
                                            encodePublicKey k
                                                |> Encode.encode 0

                                        keySpin =
                                            Animation.interrupt
                                                [ Animation.set [ Animation.rotate (Animation.deg 0) ]
                                                ]
                                                model.keySpin
                                    in
                                        { model
                                            | status = Ready connId (PublicKeyString theirPk) 0
                                            , keySpin = keySpin
                                        }
                                            ! [ newUrl "/" ]

                                a ->
                                    model ! [ log "key swap, oops" a ]

                Err err ->
                    model ! [ log "socket message error" err ]



-- VIEW


msgCard : Message -> Element Styles Variations msg
msgCard { self, content } =
    paragraph MsgCard [ padding 4, vary Self self ] [ text content ]


view : Model -> Html Msg
view { status, device, messages, input, keySpin, location, time, arrow } =
    let
        keySpinner =
            image None
                (List.concat
                    [ Animation.render keySpin |> List.map Element.Attributes.toAttr
                    , [ width <| px <| (device.width |> toFloat |> flip (/) 3) ]
                    ]
                )
                { src = "/car-key.svg", caption = "key-spinner" }

        wrapBody body =
            column Body
                [ center, verticalCenter, width <| percent 100, height <| percent 100 ]
                [ body ]
    in
        viewport styling <|
            case status of
                WaitingForBKey _ _ (RoomId roomId) ->
                    let
                        roomlink =
                            location ++ "#" ++ roomId
                    in
                        wrapBody <|
                            column None
                                [ center ]
                                [ paragraph ShareThis
                                    []
                                    [ text "Share this link with someone to begin chat:" ]
                                , paragraph Link [ padding 10 ] [ text roomlink ]
                                , button Button
                                    [ class "copy-button"
                                    , attribute "data-clipboard-text" roomlink
                                    ]
                                  <|
                                    text "COPY"
                                ]

                WaitingForAKey _ ->
                    wrapBody <| keySpinner

                Joining _ ->
                    wrapBody <| keySpinner

                Ready _ _ typingAt ->
                    column Body
                        [ width <| percent 100, height <| percent 100 ]
                        [ column Body [ spacing 7, padding 7, id "messages", scrollbars, onScroll DisplayScrollButton ] <|
                            List.map msgCard messages
                                ++ (if (time - typingAt) < 5000 then
                                        [ image None
                                            []
                                            { src = "/typing.svg", caption = "is-typing" }
                                        ]
                                    else
                                        []
                                   )
                        , when arrow <|
                            screen <|
                                circle 20
                                    StartCircle
                                    [ onClick ScrollToBottom, alignLeft, alignBottom, moveUp 40 ]
                                    empty
                        , el None [ height <| px 40 ] empty
                        , screen <|
                            el None
                                [ alignBottom ]
                            <|
                                row None
                                    []
                                    [ Input.text None
                                        [ height <| px 40
                                        , width <| px <| (device.width |> toFloat |> flip (/) 4 |> (*) 3)
                                        , onPressEnter Send
                                        ]
                                        { onChange = InputChange
                                        , value = input
                                        , label = Input.hiddenLabel "input"
                                        , options = []
                                        }
                                    , button Button
                                        [ onClick Send
                                        , width <| px <| (device.width |> toFloat |> flip (/) 4)
                                        , height <| px 40
                                        ]
                                      <|
                                        text "send"
                                    ]
                        ]

                Start _ ->
                    wrapBody <|
                        circle (device.width |> toFloat |> flip (/) 3)
                            StartCircle
                            [ onClick Init, center, verticalCenter ]
                        <|
                            el None [ center, verticalCenter ] <|
                                text
                                    "Start"



-- HELPERS


isBottom : Decode.Value -> ( number, Bool )
isBottom =
    Decode.decodeValue
        (Decode.map3
            (\scrollHeight scrollTop clientHeight ->
                -- https://gist.github.com/paulirish/5d52fb081b3570c81e3a
                ( scrollTop, (scrollHeight - scrollTop) < (clientHeight + 100) )
            )
            (Decode.at [ "target", "scrollHeight" ] Decode.int)
            (Decode.at [ "target", "scrollTop" ] (Decode.map round Decode.float))
            (Decode.at [ "target", "clientHeight" ] Decode.int)
        )
        >> Result.withDefault ( 99, False )


scrollToBottom : Cmd Msg
scrollToBottom =
    toBottom "messages"
        |> Task.attempt CbScrollToBottom


onScroll : (Value -> msg) -> Attribute variation msg
onScroll msg =
    Decode.map msg Decode.value
        |> on "scroll"


onPressEnter : msg -> Attribute variation msg
onPressEnter msg =
    keyCode
        |> andThen
            (\int ->
                if int == 13 then
                    succeed msg
                else
                    fail "not enter"
            )
        |> on "keyup"


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

                    a ->
                        fail ("enum: \"" ++ a ++ "\" not recognised")
            )
