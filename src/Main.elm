module Main exposing (main)

import Animation
import Json exposing (decodePublicKey)
import Html
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import Ports
import Task
import Types exposing (Model, Msg(..), ScrollStatus(Static), Status(Joining, Start))
import Time
import Update exposing (update)
import View exposing (view)
import WebSocket
import Window


main : Program ( Value, Maybe String, String, String ) Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { wsApi, keySpin } =
    Sub.batch
        [ WebSocket.listen wsApi CbWebsocketMessage
        , Ports.cbEncrypt CbEncrypt
        , Ports.cbDecrypt CbDecrypt
        , Animation.subscription Animate [ keySpin ]
        , Time.every (100 * Time.millisecond) Tick
        ]



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
                            Json.Encode.object [ ( "roomId", Json.Encode.string roomId ) ]
                                |> Json.Encode.encode 0
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
