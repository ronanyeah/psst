module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html
import Json.Encode
import Ports
import Task
import Time
import Types exposing (ConnId(..), Device(..), Flags, Model, Msg(..), ScrollStatus(..), Status(..))
import Update exposing (update)
import View exposing (view)


main : Program (Maybe Flags) Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.cbEncrypt CbEncrypt
        , Ports.cbDecrypt CbDecrypt
        , Ports.cbLoadPublicKey PublicKeyLoaded
        , case model.status of
            InChat _ ->
                Time.every 100 Tick

            _ ->
                Sub.none
        , Ports.wsReceive CbWebsocketMessage
        ]



-- INIT


init : Maybe Flags -> ( Model, Cmd Msg )
init maybeFlags =
    case maybeFlags of
        Just { maybeChatId, origin, shareEnabled, copyEnabled, publicKey } ->
            let
                model =
                    { emptyModel
                        | shareEnabled = shareEnabled
                        , copyEnabled = copyEnabled
                        , origin = origin
                        , myPublicKey = publicKey
                    }
            in
            case maybeChatId of
                Just chatId ->
                    ( { model
                        | status = BWaitingForAKey (ConnId chatId)
                      }
                    , [ ( "chatId", Json.Encode.string chatId ) ]
                        |> Json.Encode.object
                        |> Json.Encode.encode 0
                        |> Ports.wsSend
                    )

                Nothing ->
                    ( { model
                        | status = Start
                      }
                    , Cmd.none
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
    , device = Desktop
    , time = Time.millisToPosix 0
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
