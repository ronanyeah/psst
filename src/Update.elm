module Update exposing (update)

import Browser.Dom
import Browser.Navigation
import Dict
import Json exposing (decodeScrollEvent, decodeSocketText, encodeDataTransmit, encodePublicKey)
import Json.Decode exposing (Value)
import Json.Encode
import Ports
import Task
import Time
import Types exposing (ConnId(..), Device(..), Message(..), Model, Msg(..), ScrollStatus(..), SocketMessage(..), Status(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CbScrollToBottom _ ->
            ( { model | arrow = False }, Cmd.none )

        CreateChat ->
            ( model
            , [ ( "create", Json.Encode.null ) ]
                |> Json.Encode.object
                |> Json.Encode.encode 0
                |> Ports.wsSend
            )

        InputChange str ->
            case model.status of
                InChat ({ connId, lastTypedPing } as args) ->
                    let
                        shouldPing =
                            (Time.posixToMillis model.time - Time.posixToMillis lastTypedPing) > 4000
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
                            |> Ports.wsSend

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
                                        , lastTypedPing = Time.millisToPosix 0
                                    }
                          }
                        , Cmd.batch
                            [ Ports.encrypt
                                { plaintext = input
                                , publicKey = args.partnerPublicKey
                                }
                            , scrollToBottom
                            ]
                        )

                _ ->
                    ( model, Cmd.none )

        CbEncrypt txt ->
            case model.status of
                InChat { connId } ->
                    ( model
                    , [ ( "message", Json.Encode.string txt ) ]
                        |> Json.Encode.object
                        |> encodeDataTransmit connId
                        |> Ports.wsSend
                    )

                a ->
                    ( model, Ports.log "unused encrypt, oops" )

        CbDecrypt txt ->
            case model.status of
                InChat ({ messages } as args) ->
                    ( { model
                        | status =
                            InChat
                                { args
                                    | messages = messages ++ [ Them txt ]
                                    , lastSeenTyping = Time.millisToPosix 0
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

        PublicKeyLoaded publicKey ->
            (case model.status of
                AWaitingForBKey connId ->
                    Just connId

                BWaitingForAKey connId ->
                    Just connId

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\connId ->
                        ( { model
                            | status =
                                InChat
                                    { connId = connId
                                    , lastSeenTyping = Time.millisToPosix 0
                                    , messages = [ ChatStart ]
                                    , lastTypedPing = Time.millisToPosix 0
                                    , isLive = True
                                    , input = ""
                                    , partnerPublicKey = publicKey
                                    }
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        DisplayScrollButton event ->
            case model.scroll of
                Static ->
                    let
                        ( h, arrow ) =
                            isBottom event
                    in
                    ( { model | arrow = not arrow, scroll = Moving model.time h }, Cmd.none )

                Moving pre h ->
                    if (Time.posixToMillis model.time - Time.posixToMillis pre) > 50 then
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
                        ChatCreated (ConnId assignedId) ->
                            ( { model
                                | status =
                                    AWaitingForBKey (ConnId assignedId)
                              }
                            , Cmd.none
                            )

                        ChatMatched { aId, bId } ->
                            ( { model
                                | status =
                                    BWaitingForAKey aId
                              }
                            , [ ( "key", encodePublicKey model.myPublicKey )
                              , ( "pairId"
                                , bId
                                    |> (\(ConnId connId) ->
                                            Json.Encode.string connId
                                       )
                                )
                              ]
                                |> Json.Encode.object
                                |> encodeDataTransmit aId
                                |> Ports.wsSend
                            )

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
                                    ( model
                                    , Ports.log "ConnectionDead"
                                    )

                        ChatUnavailable ->
                            ( { model
                                | status = Start
                              }
                            , Ports.log "chat unavailable"
                            )

                        Key theirPublicKey ->
                            case model.status of
                                BWaitingForAKey _ ->
                                    ( model
                                    , Ports.loadPublicKey <| encodePublicKey theirPublicKey
                                    )

                                a ->
                                    ( model, Ports.log "Key, oops" )

                        KeyAndConn theirPublicKey theirId ->
                            case model.status of
                                AWaitingForBKey _ ->
                                    let
                                        keyTransfer =
                                            [ ( "key", encodePublicKey model.myPublicKey )
                                            ]
                                                |> Json.Encode.object
                                                |> encodeDataTransmit theirId
                                                |> Ports.wsSend
                                    in
                                    ( { model
                                        | status = AWaitingForBKey theirId
                                      }
                                    , Cmd.batch
                                        [ keyTransfer
                                        , Ports.loadPublicKey <| encodePublicKey theirPublicKey
                                        ]
                                    )

                                a ->
                                    ( model, Ports.log "KeyAndConn, oops" )

                Err err ->
                    ( model, Ports.log <| "socket message error:\n" ++ Json.Decode.errorToString err )


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
    Browser.Dom.getViewportOf "messages"
        |> Task.andThen
            (\info ->
                Browser.Dom.setViewportOf "messages" 0 info.scene.height
            )
        |> Task.attempt CbScrollToBottom
