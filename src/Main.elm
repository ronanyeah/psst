module Main exposing (main)

import Html
import Http
import Json exposing (decodeChatJoin, decodeFlags)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import Ports
import Task
import Time
import Types exposing (Device(..), Model, Msg(..), ScrollStatus(Static), Status(..))
import Update exposing (update)
import Utils exposing (log)
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
subscriptions { wsUrl } =
    Sub.batch
        [ WebSocket.listen wsUrl CbWebsocketMessage
        , Ports.cbEncrypt CbEncrypt
        , Ports.cbDecrypt CbDecrypt
        , Ports.cbLoadPublicKey (\_ -> PublicKeyLoaded)
        , Time.every (100 * Time.millisecond) Tick
        ]



-- INIT


init : Value -> ( Model, Cmd Msg )
init value =
    case decodeValue decodeFlags value of
        Ok maybeFlags ->
            case maybeFlags of
                Just { maybeChatId, origin, wsUrl, shareEnabled, copyEnabled, restUrl, publicKey } ->
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
                            ( { model
                                | status = BJoining
                              }
                            , Cmd.batch
                                [ Http.get (restUrl ++ "/chat/" ++ chatId) decodeChatJoin
                                    |> Http.send CbJoinChat
                                , Task.perform Resize Window.size
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

        Err err ->
            ( { emptyModel
                | status = ErrorView <| "Something horrible has occurred"
              }
            , log "!" err
            )


emptyModel : Model
emptyModel =
    { status = ErrorView ""
    , origin = ""
    , wsUrl = ""
    , restUrl = ""
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
