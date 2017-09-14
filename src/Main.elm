module Main exposing (main)

import Animation
import Json exposing (decodeFlags)
import Html
import Json.Decode exposing (decodeValue, nullable)
import Json.Encode exposing (Value)
import Ports
import Task
import Types exposing (Flags, Model, Msg(..), ScrollStatus(Static), Status(..))
import Time
import Update exposing (update)
import View exposing (view)
import WebSocket
import Window


main : Program Value Model Msg
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
        , Ports.cbLoadPublicKey PublicKeyLoaded
        , Animation.subscription Animate [ keySpin ]
        , Time.every (100 * Time.millisecond) Tick
        ]



-- INIT


init : Value -> ( Model, Cmd Msg )
init =
    decodeValue (nullable decodeFlags)
        >> (\result ->
                case result of
                    Ok (Just data) ->
                        happyPath data

                    Ok Nothing ->
                        { emptyModel
                            | status = ErrorView "Your browser is not equipped for this sweet PWA"
                        }
                            ! []

                    Err err ->
                        { emptyModel | status = ErrorView err } ! []
           )


happyPath : Flags -> ( Model, Cmd Msg )
happyPath { maybeRoomId, publicKey, origin, wsUrl, shareEnabled, copyEnabled } =
    let
        ( status, animation, cmd ) =
            case maybeRoomId of
                Just roomId ->
                    ( BJoining
                    , animationInit
                        |> Animation.interrupt
                            [ Animation.loop
                                [ Animation.to
                                    [ Animation.rotate (Animation.turn 1) ]
                                ]
                            ]
                    , [ ( "roomId", Json.Encode.string roomId ) ]
                        |> Json.Encode.object
                        |> Json.Encode.encode 0
                        |> WebSocket.send wsUrl
                    )

                Nothing ->
                    ( Start
                    , animationInit
                    , Cmd.none
                    )
    in
        { emptyModel
            | status = status
            , keySpin = animation
            , shareEnabled = shareEnabled
            , copyEnabled = copyEnabled
            , wsApi = wsUrl
            , origin = origin
            , myPublicKey = publicKey
        }
            ! [ cmd
              , Task.perform Resize Window.size
              ]


animationInit : Animation.State
animationInit =
    Animation.style
        [ Animation.rotate <| Animation.deg 0
        ]


emptyModel : Model
emptyModel =
    { status = ErrorView ""
    , origin = ""
    , wsApi = ""
    , device =
        { width = 0
        , height = 0
        , phone = False
        , tablet = False
        , desktop = False
        , bigDesktop = False
        , portrait = False
        }
    , keySpin = animationInit
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
