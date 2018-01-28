module Update exposing (update)

import Dom.Scroll exposing (toBottom)
import Http
import Json exposing (decodeChatCreate, decodeScrollEvent, decodeSocketText, encodeChatId, encodeDataTransmit, encodePublicKey)
import Json.Decode
import Json.Encode
import Navigation
import Ports
import Task
import Types exposing (ConnId(..), Device(..), Message(..), Model, Msg(..), ScrollStatus(..), SocketMessage(..), Status(..))
import Utils exposing (log)
import WebSocket


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CbCreateChat res ->
            case res of
                Ok { chatId, connId } ->
                    ( { model | status = AWaitingForBKey connId chatId }
                    , [ ( "A_JOIN", encodeChatId chatId ) ]
                        |> Json.Encode.object
                        |> Json.Encode.encode 0
                        |> WebSocket.send model.wsUrl
                    )

                Err err ->
                    ( model, log "chat create" err )

        CbJoinChat res ->
            case res of
                Ok { aId, chatId } ->
                    ( { model | status = BWaitingForAKey aId }
                    , Cmd.batch
                        [ [ ( "B_JOIN", encodeChatId chatId ) ]
                            |> Json.Encode.object
                            |> Json.Encode.encode 0
                            |> WebSocket.send model.wsUrl
                        , [ ( "key", encodePublicKey model.myPublicKey ) ]
                            |> Json.Encode.object
                            |> encodeDataTransmit aId
                            |> WebSocket.send model.wsUrl
                        ]
                    )

                Err err ->
                    ( model, log "chat join" err )

        CbScrollToBottom _ ->
            ( { model | arrow = False }, Cmd.none )

        CreateChat ->
            ( model, createChat model.restUrl )

        InputChange str ->
            case model.status of
                InChat ({ connId, lastTypedPing } as args) ->
                    let
                        shouldPing =
                            (model.time - lastTypedPing) > 4000
                    in
                    ( { model
                        | status =
                            InChat
                                { args
                                    | input = str
                                    , lastTypedPing =
                                        if shouldPing then
                                            model.time
                                        else
                                            lastTypedPing
                                }
                      }
                    , if shouldPing then
                        Json.Encode.string "TYPING"
                            |> encodeDataTransmit connId
                            |> WebSocket.send model.wsUrl
                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Send ->
            case model.status of
                InChat ({ input, messages } as args) ->
                    if String.isEmpty input then
                        ( model, Cmd.none )
                    else
                        ( { model
                            | status =
                                InChat
                                    { args
                                        | input = ""
                                        , messages = messages ++ [ Self input ]
                                        , lastTypedPing = 0
                                    }
                          }
                        , Cmd.batch [ Ports.encrypt input, scrollToBottom ]
                        )

                _ ->
                    ( model, Cmd.none )

        Resize size ->
            ( { model
                | device =
                    if size.width <= 600 then
                        Mobile
                    else
                        Desktop
              }
            , Cmd.none
            )

        CbEncrypt txt ->
            case model.status of
                InChat { connId } ->
                    ( model
                    , [ ( "message", Json.Encode.string txt ) ]
                        |> Json.Encode.object
                        |> encodeDataTransmit connId
                        |> WebSocket.send model.wsUrl
                    )

                a ->
                    ( model, log "cbEncrypt, oops" a )

        CbDecrypt txt ->
            case model.status of
                InChat ({ messages } as args) ->
                    ( { model
                        | status =
                            InChat
                                { args
                                    | messages = messages ++ [ Them txt ]
                                    , lastSeenTyping = 0
                                }
                      }
                    , scrollToBottom
                    )

                _ ->
                    ( model, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        ScrollToBottom ->
            ( model, scrollToBottom )

        Share url ->
            ( model, Ports.share url )

        ExitChat ->
            ( { model | status = Start }, Cmd.none )

        PublicKeyLoaded ->
            let
                startChat connId =
                    ( { model
                        | status =
                            InChat
                                { connId = connId
                                , lastSeenTyping = 0
                                , messages = [ ChatStart ]
                                , lastTypedPing = 0
                                , isLive = True
                                , input = ""
                                }
                      }
                    , Navigation.modifyUrl "/"
                    )
            in
            case model.status of
                AWaitingForBKey connId _ ->
                    startChat connId

                BWaitingForAKey connId ->
                    startChat connId

                _ ->
                    ( model, Cmd.none )

        DisplayScrollButton event ->
            case model.scroll of
                Static ->
                    let
                        ( h, arrow ) =
                            isBottom event
                    in
                    ( { model | arrow = not arrow, scroll = Moving model.time h }, Cmd.none )

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
                        ( { model | arrow = not arrow, scroll = scroll }, Cmd.none )
                    else
                        ( model, Cmd.none )

        CbWebsocketMessage str ->
            case Json.Decode.decodeString decodeSocketText str of
                Ok socketMsg ->
                    case socketMsg of
                        ReceiveMessage txt ->
                            ( model, Ports.decrypt txt )

                        Typing ->
                            case model.status of
                                InChat args ->
                                    ( { model
                                        | status =
                                            InChat { args | lastSeenTyping = model.time }
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        ConnectionDead ->
                            case model.status of
                                InChat ({ messages } as args) ->
                                    ( { model
                                        | status =
                                            InChat
                                                { args
                                                    | isLive = False
                                                    , messages = messages ++ [ ConnEnd ]
                                                }
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( { model
                                        | status = Start
                                      }
                                    , Cmd.batch
                                        [ log "!" "chat unavailable"
                                        , Navigation.modifyUrl "/"
                                        ]
                                    )

                        ChatUnavailable ->
                            ( { model
                                | status = Start
                              }
                            , Cmd.batch
                                [ log "!" "chat unavailable"
                                , Navigation.modifyUrl "/"
                                ]
                            )

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
                                    ( model
                                    , Cmd.batch
                                        [ keyTransfer
                                        , Ports.loadPublicKey <| encodePublicKey theirPublicKey
                                        ]
                                    )

                                BWaitingForAKey _ ->
                                    ( model
                                    , Ports.loadPublicKey <| encodePublicKey theirPublicKey
                                    )

                                a ->
                                    ( model, log "key swap, oops" a )

                Err err ->
                    ( model, log "socket message error" err )


createChat : String -> Cmd Msg
createChat restUrl =
    Http.get (restUrl ++ "/chat") decodeChatCreate
        |> Http.send CbCreateChat


isBottom : Json.Decode.Value -> ( Int, Bool )
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
