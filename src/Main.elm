port module Main exposing (main)

import Color exposing (black, grey)
import Element exposing (Attribute, circle, column, el, link, text, viewport)
import Element.Attributes exposing (center, target, verticalCenter)
import Element.Events exposing (keyCode, on, onClick)
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, list, string, bool, map6, map, decodeValue)
import Json.Encode exposing (Value)
import Style exposing (StyleSheet, style, stylesheet)
import Style.Color as Color
import Style.Border as Border
import WebSocket


main : Program ( Value, Maybe String ) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.listen "ws://psst-api.glitch.me" Echo



-- PORTS


port decrypt : String -> Cmd msg


port encrypt : String -> Cmd msg


port cbDecrypt : (String -> msg) -> Sub msg


port cbEncrypt : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { publicKey : PublicKey
    , room : Maybe String
    }


type alias PublicKey =
    { alg : String
    , e : String
    , ext : Bool
    , key_ops : List String
    , kty : String
    , n : String
    }


publicKeyDecoder : Decoder PublicKey
publicKeyDecoder =
    map6 PublicKey
        (field "alg" string)
        (field "e" string)
        (field "ext" bool)
        (field "key_ops" (list string))
        (field "kty" string)
        (field "n" string)



-- INIT


init : ( Value, Maybe String ) -> ( Model, Cmd Msg )
init ( jwk, maybeRoomId ) =
    let
        publicKey =
            jwk
                |> decodeValue publicKeyDecoder
                |> Result.withDefault
                    { alg = ""
                    , e = ""
                    , ext = True
                    , key_ops = []
                    , kty = ""
                    , n = ""
                    }

        cmd =
            case maybeRoomId of
                Just id ->
                    Http.get ("https://psst-api.glitch.me/room/" ++ id) (field "ready" bool)
                        |> Http.send RoomReady

                Nothing ->
                    Cmd.none
    in
        { publicKey = publicKey, room = maybeRoomId } ! [ cmd, WebSocket.send "ws://psst-api.glitch.me" "lol" ]



-- MESSAGES


type Msg
    = NewRoom (Result Http.Error String)
    | RoomReady (Result Http.Error Bool)
    | Click
    | Echo String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRoom res ->
            case res of
                Ok data ->
                    --let
                    --cmd =
                    --if data then
                    --in
                    { model | room = Just data } ! [ log "room" data ]

                Err err ->
                    model ! [ log "room err" err ]

        RoomReady res ->
            case res of
                Ok data ->
                    model ! [ log "room" data ]

                Err err ->
                    model ! [ log "room err" err ]

        Echo str ->
            model ! [ log "echo" str ]

        Click ->
            model ! [ getRoom ]



-- VIEW


type Styles
    = Button
    | None
    | StartCircle


styling : StyleSheet Styles variation
styling =
    stylesheet
        [ style None []
        , style StartCircle [ Color.background grey ]
        , style Button [ Border.dashed, Color.border black ]
        ]


view : Model -> Html Msg
view model =
    viewport styling <|
        column None
            [ center, verticalCenter ]
            [ case model.room of
                Just id ->
                    link ("http://localhost:8080/?room-id=" ++ id) <| el None [ target "_blank" ] <| text "Start!"

                Nothing ->
                    circle 50
                        StartCircle
                        [ onClick Click ]
                    <|
                        el None [ center, verticalCenter ] <|
                            text <|
                                "Start"
            ]


getRoom : Cmd Msg
getRoom =
    Http.get "https://psst-api.glitch.me/new" (field "room" string)
        |> Http.send NewRoom



-- HELPERS


onKeyDown : (Int -> msg) -> Attribute variation msg
onKeyDown tagger =
    on "keyup" (map tagger keyCode)



-- COMMANDS
--focusOn : String -> Cmd Msg
--focusOn =
--Dom.focus >> Task.attempt FocusCb


log : String -> a -> Cmd Msg
log tag a =
    let
        _ =
            Debug.log tag a
    in
        Cmd.none
