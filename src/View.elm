module View exposing (view)

import Color
import Element exposing (Attribute, Element, alignBottom, alignLeft, centerX, centerY, column, el, fill, height, htmlAttribute, inFront, layout, moveUp, none, padding, paragraph, px, scrollbars, shrink, spacing, text, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (keyCode, on)
import Json.Decode
import Json.Encode
import Style
import Time exposing (Time)
import Types exposing (ConnId(..), Message(..), Model, Msg(..), Status(..))
import Utils exposing (when)


id : String -> Attribute msg
id =
    Html.Attributes.id
        >> htmlAttribute


rotate : Attribute msg
rotate =
    Html.Attributes.style [ ( "animation", "rotation 2s infinite linear" ) ]
        |> Element.htmlAttribute


spinner : Element msg
spinner =
    el
        [ Font.size 30
        , rotate
        ]
    <|
        text "↻"


view : Model -> Html Msg
view { status, origin, time, arrow, shareEnabled, copyEnabled } =
    layout [ Bg.color Style.grn, Style.font ] <|
        case status of
            Start ->
                el
                    [ width <| px 100
                    , height <| px 100
                    , Bg.color Style.org
                    , Border.rounded 50
                    , centerY
                    , Element.pointer
                    , onClick CreateChat
                    , id "start-circle"
                    ]
                <|
                    el [ centerX, centerY ] <|
                        text
                            "Start"

            AWaitingForBKey (ConnId connId) ->
                let
                    chatlink =
                        origin ++ "#" ++ connId
                in
                column
                    [ spacing 20, height shrink, centerY ]
                    [ el
                        [ centerX, Bg.color Style.blu, Font.size 20, padding 10 ]
                      <|
                        text "Share this:"
                    , el
                        [ centerX, Bg.color Style.ylw, id "chat-link", padding 10 ]
                      <|
                        text chatlink
                    , when copyEnabled <|
                        button [ centerX ]
                            { onPress = Nothing
                            , label =
                                el
                                    [ id "copy-button"
                                    , Html.Attributes.attribute "data-clipboard-text" chatlink
                                        |> htmlAttribute
                                    , Border.dashed
                                    , Border.width 2
                                    , Border.color Color.black
                                    , Bg.color Style.org
                                    , padding 10
                                    ]
                                <|
                                    text "COPY"
                            }
                    , when shareEnabled <|
                        button [ centerX ]
                            { onPress = Just <| Share chatlink
                            , label =
                                el
                                    [ Border.dashed
                                    , Border.width 2
                                    , Border.color Color.black
                                    , Bg.color Style.org
                                    , padding 10
                                    ]
                                <|
                                    text
                                        "SHARE"
                            }
                    ]

            BJoining _ ->
                spinner

            BWaitingForAKey _ ->
                spinner

            InChat { lastSeenTyping, messages, isLive, input } ->
                column
                    [ inFront <| inputBox input isLive ]
                    [ column
                        [ spacing 7
                        , padding 7
                        , id "messages"
                        , scrollbars
                        , onScroll DisplayScrollButton
                        ]
                      <|
                        List.indexedMap msgCard messages
                    , viewTyping time lastSeenTyping
                    , when arrow <|
                        el
                            [ onClick ScrollToBottom, alignLeft, alignBottom, moveUp 40 ]
                            none
                    , el [ height <| px 40 ] none
                    ]

            ErrorView txt ->
                el [ width fill ] <| paragraph [] [ text txt ]


inputBox : String -> Bool -> Element Msg
inputBox input isLive =
    el
        [ alignBottom, centerX, padding 10 ]
    <|
        Input.text
            [ onPressEnter Send
            , id "message-input"
            , Border.dashed
            , Border.color Color.black
            , Border.width 2
            , padding 10
            ]
            { onChange =
                if isLive then
                    Just InputChange
                else
                    Nothing
            , text = input
            , label =
                Input.labelRight [] <|
                    button []
                        { onPress =
                            if isLive then
                                Just Send
                            else
                                Nothing
                        , label =
                            el
                                [ padding 5
                                , id <|
                                    if isLive then
                                        "send-message"
                                    else
                                        "conn-lost"
                                , Border.dashed
                                , Border.color Color.black
                                , Border.width 2
                                , Bg.color Style.org
                                , Font.size 30
                                ]
                            <|
                                if isLive then
                                    text "send"
                                else
                                    el
                                        [ Font.size 30
                                        ]
                                    <|
                                        text "🚫"
                        }
            , placeholder = Nothing
            }


viewTyping : Time -> Time -> Element msg
viewTyping currentTime lastSeenTyping =
    when ((currentTime - lastSeenTyping) < 5000) <|
        el
            [ id "typing"
            , Font.size 30
            , centerX
            , Font.color Color.red
            ]
        <|
            text "TYPING!"


msgCard : Int -> Message -> Element msg
msgCard i message =
    let
        attrs =
            [ padding 4
            , id <| "message-" ++ toString i
            ]
    in
    case message of
        Self content ->
            paragraph (Style.msgSelf ++ attrs) [ text content ]
                |> el [ centerX ]

        Them content ->
            paragraph (Style.msgThem ++ attrs) [ text content ]
                |> el [ centerX ]

        ChatStart ->
            paragraph (Style.msgSys ++ attrs) [ text "Ready to chat!" ]
                |> el [ centerX ]

        ConnEnd ->
            paragraph (Style.msgSys ++ attrs) [ text "Connection lost!" ]
                |> el [ centerX ]


onScroll : (Json.Encode.Value -> msg) -> Attribute msg
onScroll msg =
    Json.Decode.map msg Json.Decode.value
        |> on "scroll"
        |> htmlAttribute


onPressEnter : msg -> Attribute msg
onPressEnter msg =
    keyCode
        |> Json.Decode.andThen
            (\int ->
                if int == 13 then
                    Json.Decode.succeed msg
                else
                    Json.Decode.fail "not enter"
            )
        |> on "keyup"
        |> htmlAttribute
