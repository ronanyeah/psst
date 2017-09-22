module Update exposing (update)

import Animation
import Dom.Scroll exposing (toBottom)
import Element
import Http
import Json exposing (decodeChatCreate, decodeScrollEvent, decodeSocketText, encodeDataTransmit, encodePublicKey)
import Json.Decode
import Json.Encode
import Navigation exposing (newUrl)
import Ports
import Task
import Types exposing (ConnId(..), Message(..), Model, Msg(..), ChatId(ChatId), SocketMessages(..), ScrollStatus(..), TypingStatus(..), Status(..))
import WebSocket


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CbCreateChat res ->
            case res of
                Ok { chatId, connId } ->
                    { model | status = AWaitingForBKey connId chatId }
                        ! [ let
                                (ChatId chatIdString) =
                                    chatId
                            in
                                Json.Encode.object [ ( "A_JOIN", Json.Encode.string chatIdString ) ]
                                    |> Json.Encode.encode 0
                                    |> WebSocket.send model.wsUrl
                          ]

                Err err ->
                    model ! [ log "chat create" err ]

        CbJoinChat res ->
            case res of
                Ok { aId, chatId } ->
                    { model | status = BWaitingForAKey aId }
                        ! [ let
                                (ChatId chatIdString) =
                                    chatId
                            in
                                Json.Encode.object [ ( "B_JOIN", Json.Encode.string chatIdString ) ]
                                    |> Json.Encode.encode 0
                                    |> WebSocket.send model.wsUrl
                          , [ ( "key", encodePublicKey model.myPublicKey )
                            ]
                                |> Json.Encode.object
                                |> encodeDataTransmit aId
                                |> WebSocket.send model.wsUrl
                          ]

                Err err ->
                    model ! [ log "chat join" err ]

        CbScrollToBottom _ ->
            { model | arrow = False } ! []

        CreateChat ->
            model ! [ createChat model.restUrl ]

        InputChange str ->
            case model.status of
                InChat ({ lastTyped, lastTypedPing } as args) ->
                    let
                        ( pinged, cmd ) =
                            case ( model.status, (lastTyped - lastTypedPing) > 4000 ) of
                                ( InChat { connId }, True ) ->
                                    ( model.time
                                    , Json.Encode.string "TYPING"
                                        |> encodeDataTransmit connId
                                        |> WebSocket.send model.wsUrl
                                    )

                                _ ->
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
                                        , messages = messages ++ [ Self input ]
                                        , lastTypedPing = 0
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
                                |> WebSocket.send model.wsUrl
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
                                    | messages = messages ++ [ Them txt ]
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

        ExitChat ->
            { model | status = Start } ! []

        PublicKeyLoaded () ->
            let
                startChat connId =
                    { model
                        | status =
                            InChat
                                { connId = connId
                                , typingStatus = NotTyping
                                , messages = [ ChatStart ]
                                , lastTyped = 0
                                , lastTypedPing = 0
                                , isLive = True
                                , input = ""
                                }
                    }
                        ! [ newUrl "/" ]
            in
                case model.status of
                    AWaitingForBKey connId _ ->
                        startChat connId

                    BWaitingForAKey connId ->
                        startChat connId

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
                                InChat ({ messages } as args) ->
                                    { model
                                        | status =
                                            InChat { args | isLive = False, messages = messages ++ [ ConnEnd ] }
                                    }
                                        ! []

                                _ ->
                                    model ! []

                        Error err ->
                            model ! [ log "socket server error" err ]

                        ChatUnavailable ->
                            { model
                                | status = Start
                                , keySpin =
                                    Animation.interrupt
                                        [ Animation.set [ Animation.rotate (Animation.deg 0) ]
                                        ]
                                        model.keySpin
                            }
                                ! [ log "chat unavailable" 0, newUrl "/" ]

                        Key theirPublicKey ->
                            case model.status of
                                AWaitingForBKey connId _ ->
                                    let
                                        keyTransfer =
                                            [ ( "key", encodePublicKey model.myPublicKey )
                                            ]
                                                |> Json.Encode.object
                                                |> encodeDataTransmit connId
                                                |> WebSocket.send model.wsUrl
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


createChat : String -> Cmd Msg
createChat restUrl =
    Http.get (restUrl ++ "/chat") decodeChatCreate
        |> Http.send CbCreateChat


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
