module Main exposing (main)

import Html
import Json.Encode
import Ports
import Task
import Time
import Types exposing (ConnId(..), Device(..), Flags, Model, Msg(..), ScrollStatus(Static), Status(..))
import Update exposing (update)
import View exposing (view)
import WebSocket
import Window


main : Program (Maybe Flags) Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { wsUrl, status } =
    Sub.batch
        [ WebSocket.listen wsUrl CbWebsocketMessage
        , Ports.cbEncrypt CbEncrypt
        , Ports.cbDecrypt CbDecrypt
        , Ports.cbLoadPublicKey PublicKeyLoaded
        , case status of
            InChat _ ->
                Time.every (100 * Time.millisecond) Tick

            _ ->
                Sub.none
        ]



-- INIT


init : Maybe Flags -> ( Model, Cmd Msg )
init maybeFlags =
    case maybeFlags of
        Just { maybeChatId, origin, wsUrl, shareEnabled, copyEnabled, publicKey } ->
            let
                model =
                    { emptyModel
                        | shareEnabled = shareEnabled
                        , copyEnabled = copyEnabled
                        , wsUrl = wsUrl
                        , origin = origin
                        , myPublicKey = publicKey
                    }
            in
            case maybeChatId of
                Just chatId ->
                    ( { model
                        | status = BWaitingForAKey (ConnId chatId)
                      }
                    , Cmd.batch
                        [ Task.perform Resize Window.size
                        , [ ( "chatId", Json.Encode.string chatId ) ]
                            |> Json.Encode.object
                            |> Json.Encode.encode 0
                            |> WebSocket.send model.wsUrl
                        ]
                    )

                Nothing ->
                    ( { model
                        | status = Start
                      }
                    , Task.perform Resize Window.size
                    )

        Nothing ->
            ( { emptyModel
                | status = ErrorView "Your browser is not equipped for this sweet PWA"
              }
            , Cmd.none
            )


emptyModel : Model
emptyModel =
    { status = ErrorView ""
    , origin = ""
    , wsUrl = ""
    , device = Desktop
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
