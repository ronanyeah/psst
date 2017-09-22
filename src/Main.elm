module Main exposing (main)

import Animation
import Json exposing (decodeChatJoin, decodePublicKey)
import Html
import Http
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import Ports
import Task
import Types exposing (Flags, Model, Msg(..), PublicKeyRecord, ScrollStatus(Static), Status(..))
import Time
import Update exposing (update)
import View exposing (view)
import WebSocket
import Window


main : Program ( Value, Flags ) Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { wsUrl, keySpin } =
    Sub.batch
        [ WebSocket.listen wsUrl CbWebsocketMessage
        , Ports.cbEncrypt CbEncrypt
        , Ports.cbDecrypt CbDecrypt
        , Ports.cbLoadPublicKey PublicKeyLoaded
        , Animation.subscription Animate [ keySpin ]
        , Time.every (100 * Time.millisecond) Tick
        ]



-- INIT


init : ( Value, Flags ) -> ( Model, Cmd Msg )
init ( jwk, flags ) =
    decodeValue decodePublicKey jwk
        |> Result.map (happyPath flags)
        |> Result.withDefault
            ({ emptyModel
                | status = ErrorView "Your browser is not equipped for this sweet PWA"
             }
                ! []
            )


happyPath : Flags -> PublicKeyRecord -> ( Model, Cmd Msg )
happyPath { maybeChatId, origin, wsUrl, shareEnabled, copyEnabled, restUrl } publicKey =
    let
        model =
            { emptyModel
                | shareEnabled = shareEnabled
                , copyEnabled = copyEnabled
                , wsUrl = wsUrl
                , restUrl = restUrl
                , origin = origin
                , myPublicKey = publicKey
            }
    in
        case maybeChatId of
            Just chatId ->
                { model
                    | status = BJoining
                    , keySpin =
                        model.keySpin
                            |> Animation.interrupt
                                [ Animation.loop
                                    [ Animation.to
                                        [ Animation.rotate (Animation.turn 1) ]
                                    ]
                                ]
                }
                    ! [ Http.get (restUrl ++ "/chat/" ++ chatId) decodeChatJoin
                            |> Http.send CbJoinChat
                      , Task.perform Resize Window.size
                      ]

            Nothing ->
                { model
                    | status = Start
                }
                    ! [ Task.perform Resize Window.size ]


emptyModel : Model
emptyModel =
    { status = ErrorView ""
    , origin = ""
    , wsUrl = ""
    , restUrl = ""
    , device =
        { width = 0
        , height = 0
        , phone = False
        , tablet = False
        , desktop = False
        , bigDesktop = False
        , portrait = False
        }
    , keySpin =
        Animation.style
            [ Animation.rotate <| Animation.deg 0
            ]
    , time = 0
    , arrow = False
    , scroll = Static
    , shareEnabled = False
    , copyEnabled = False
    , myPublicKey =
        { alg = ""
        , e = ""
        , ext = True
        , key_ops = []
        , kty = ""
        , n = ""
        }
    }
