module Update exposing (update)

import Animation
import Dom.Scroll exposing (toBottom)
import Element
import Json exposing (decodeScrollEvent, decodeSocketText, encodeDataTransmit, encodePublicKey)
import Json.Decode
import Json.Encode
import Navigation exposing (newUrl)
import Ports
import Task
import Types exposing (ConnId(..), Model, Msg(..), SocketMessages(..), ScrollStatus(..), TypingStatus(..), Status(..))
import WebSocket


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartMsg ->
            model ! [ start model.wsApi ]

        CbScrollToBottom _ ->
            { model | arrow = False } ! []

        InputChange str ->
            case model.status of
                InChat ({ lastTyped, lastTypedPing } as args) ->
                    let
                        ( pinged, cmd ) =
                            if (lastTyped - lastTypedPing) > 4000 then
                                case model.status of
                                    InChat { connId } ->
                                        ( model.time
                                        , Json.Encode.string "TYPING"
                                            |> encodeDataTransmit connId
                                            |> WebSocket.send model.wsApi
                                        )

                                    _ ->
                                        ( lastTypedPing, Cmd.none )
                            else
                                ( lastTypedPing, Cmd.none )
                    in
                        { model
                            | status =
                                InChat
                                    { args
                                        | input = str
                                        , lastTyped = model.time
                                        , lastTypedPing = pinged
                                    }
                        }
                            ! [ cmd ]

                _ ->
                    model ! []

        Send ->
            case model.status of
                InChat ({ input, messages } as args) ->
                    if String.isEmpty input then
                        model ! []
                    else
                        { model
                            | status =
                                InChat
                                    { args
                                        | input = ""
                                        , messages = messages ++ [ { self = True, content = input } ]
                                    }
                        }
                            ! [ Ports.encrypt input, scrollToBottom ]

                _ ->
                    model ! []

        Resize size ->
            { model | device = Element.classifyDevice size } ! []

        CbEncrypt txt ->
            case model.status of
                InChat { connId } ->
                    let
                        messageTransfer =
                            [ ( "message", Json.Encode.string txt )
                            ]
                                |> Json.Encode.object
                                |> encodeDataTransmit connId
                                |> WebSocket.send model.wsApi
                    in
                        model
                            ! [ messageTransfer ]

                a ->
                    model ! [ log "cbEncrypt, oops" a ]

        CbDecrypt txt ->
            case model.status of
                InChat ({ messages } as args) ->
                    { model
                        | status =
                            InChat
                                { args
                                    | messages = messages ++ [ { self = False, content = txt } ]
                                    , typingStatus = NotTyping
                                }
                    }
                        ! [ scrollToBottom ]

                _ ->
                    model ! []

        Animate animMsg ->
            { model
                | keySpin = Animation.update animMsg model.keySpin
            }
                ! []

        Tick time ->
            { model | time = time } ! []

        ScrollToBottom ->
            model ! [ scrollToBottom ]

        Share url ->
            model ! [ Ports.share url ]

        PublicKeyLoaded () ->
            case model.status of
                AWaitingForBKey connId _ ->
                    { model
                        | status =
                            InChat
                                { connId = connId
                                , typingStatus = NotTyping
                                , messages = []
                                , lastTyped = 0
                                , lastTypedPing = 0
                                , isLive = True
                                , input = ""
                                }
                    }
                        ! [ newUrl "/" ]

                BWaitingForAKey connId ->
                    { model
                        | status =
                            InChat
                                { connId = connId
                                , typingStatus = NotTyping
                                , messages = []
                                , lastTyped = 0
                                , lastTypedPing = 0
                                , isLive = True
                                , input = ""
                                }
                    }
                        ! [ newUrl "/" ]

                _ ->
                    model ! []

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
            case Json.Decode.decodeString decodeSocketText str of
                Ok socketMsg ->
                    case socketMsg of
                        Waiting bId room ->
                            case model.status of
                                Start ->
                                    { model | status = AWaitingForBKey bId room } ! []

                                a ->
                                    model ! [ log "waiting, oops" a ]

                        ReceiveMessage txt ->
                            model ! [ Ports.decrypt txt ]

                        Typing ->
                            case model.status of
                                InChat args ->
                                    { model
                                        | status =
                                            InChat { args | typingStatus = IsTyping model.time }
                                    }
                                        ! []

                                _ ->
                                    model ! []

                        ConnectionDead ->
                            case model.status of
                                InChat args ->
                                    { model
                                        | status =
                                            InChat { args | isLive = False }
                                    }
                                        ! []

                                _ ->
                                    model ! []

                        Error err ->
                            model ! [ log "socket server error" err ]

                        RoomUnavailable ->
                            case model.status of
                                BJoining ->
                                    { model
                                        | status = Start
                                        , keySpin =
                                            Animation.interrupt
                                                [ Animation.set [ Animation.rotate (Animation.deg 0) ]
                                                ]
                                                model.keySpin
                                    }
                                        ! [ log "room unavailable" 0, newUrl "/" ]

                                a ->
                                    model ! [ log "room unavailable, oops" a ]

                        ReceiveAId connId ->
                            case model.status of
                                BJoining ->
                                    let
                                        keyTransfer =
                                            [ ( "key", encodePublicKey model.myPublicKey )
                                            ]
                                                |> Json.Encode.object
                                                |> encodeDataTransmit connId
                                                |> WebSocket.send model.wsApi
                                    in
                                        { model | status = BWaitingForAKey connId }
                                            ! [ keyTransfer ]

                                a ->
                                    model ! [ log "start, oops" a ]

                        Key theirPublicKey ->
                            case model.status of
                                AWaitingForBKey connId _ ->
                                    let
                                        keyTransfer =
                                            [ ( "key", encodePublicKey model.myPublicKey )
                                            ]
                                                |> Json.Encode.object
                                                |> encodeDataTransmit connId
                                                |> WebSocket.send model.wsApi
                                    in
                                        model
                                            ! [ keyTransfer
                                              , Ports.loadPublicKey <| encodePublicKey theirPublicKey
                                              ]

                                BWaitingForAKey _ ->
                                    let
                                        keySpin =
                                            Animation.interrupt
                                                [ Animation.set [ Animation.rotate (Animation.deg 0) ]
                                                ]
                                                model.keySpin
                                    in
                                        { model
                                            | keySpin = keySpin
                                        }
                                            ! [ Ports.loadPublicKey <| encodePublicKey theirPublicKey
                                              ]

                                a ->
                                    model ! [ log "key swap, oops" a ]

                Err err ->
                    model ! [ log "socket message error" err ]


start : String -> Cmd Msg
start wsUrl =
    Json.Encode.string "START"
        |> Json.Encode.encode 0
        |> WebSocket.send wsUrl


isBottom : Json.Decode.Value -> ( number, Bool )
isBottom =
    Json.Decode.decodeValue decodeScrollEvent
        >> Result.map
            (\{ scrollHeight, scrollTop, clientHeight } ->
                -- https://gist.github.com/paulirish/5d52fb081b3570c81e3a
                ( scrollTop, (scrollHeight - scrollTop) < (clientHeight + 100) )
            )
        >> Result.withDefault ( 99, False )


scrollToBottom : Cmd Msg
scrollToBottom =
    toBottom "messages"
        |> Task.attempt CbScrollToBottom


log : String -> a -> Cmd Msg
log tag a =
    let
        _ =
            Debug.log tag a
    in
        Cmd.none
