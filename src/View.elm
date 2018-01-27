module View exposing (view)

import Color
import Element exposing (Attribute, Element, alignBottom, alignLeft, attribute, center, centerY, column, el, empty, fill, height, html, image, layout, moveUp, padding, paragraph, px, row, scrollbars, spacing, text, width)
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
import Types exposing (ChatId(..), Message(..), Model, Msg(..), Status(..))
import Utils exposing (when)


attr : String -> String -> Attribute msg
attr a b =
    Html.Attributes.attribute a b
        |> attribute


class : String -> Attribute msg
class =
    attr "class"


id : String -> Attribute msg
id =
    attr "id"


spinner : Element msg
spinner =
    el [] <|
        html <|
            Html.span [ Html.Attributes.attribute "class" "fas fa-sync fa-spin" ]
                []


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
                    ]
                <|
                    el [ center, centerY ] <|
                        text
                            "Start"

            AWaitingForBKey _ (ChatId chatId) ->
                let
                    chatlink =
                        origin ++ "#" ++ chatId
                in
                el [ center, centerY ] <|
                    column
                        [ spacing 20 ]
                        [ el
                            [ Bg.color Style.blu, Font.size 20, padding 10 ]
                          <|
                            text "Share this:"
                        , el
                            [ Bg.color Style.ylw, id "chat-link", padding 10 ]
                          <|
                            text chatlink
                        , when copyEnabled <|
                            button []
                                { onPress = Nothing
                                , label =
                                    el
                                        [ id "copy-button"
                                        , attr "data-clipboard-text" chatlink
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
                            button []
                                { onPress = Just <| Share chatlink
                                , label =
                                    el Style.button <|
                                        text "SHARE"
                                }
                        ]

            BJoining ->
                spinner

            BWaitingForAKey _ ->
                spinner

            InChat { lastSeenTyping, messages } ->
                column
                    []
                    [ column
                        [ spacing 7
                        , padding 7

                        --, id "messages"
                        , scrollbars
                        , onScroll DisplayScrollButton
                        ]
                      <|
                        List.map msgCard messages
                    , viewTyping time lastSeenTyping
                    , when arrow <|
                        --screen <|
                        --circle 20
                        el
                            [ onClick ScrollToBottom, alignLeft, alignBottom, moveUp 40 ]
                            empty
                    , el [ height <| px 40 ] empty
                    ]

            ErrorView txt ->
                text txt


inputBox : Element Msg
inputBox =
    el
        [ alignBottom, center ]
    <|
        row
            [ padding 2, spacing 2 ]
            [ Input.multiline
                (Style.messageBox
                    ++ [ height <| px 40

                       --, width <|
                       --px <|
                       --(viewWidth
                       --|> flip (/) 4
                       --|> (*) 3
                       --)
                       , onPressEnter Send
                       , class "message-input"
                       ]
                )
                { onChange =
                    -- if isLive then
                    if True then
                        Just InputChange
                    else
                        Nothing
                , text = "" -- input
                , label = Input.labelAbove [] empty
                , notice = Nothing
                , placeholder = Nothing
                }

            --, if isLive then
            , if True then
                button []
                    { onPress = Just Send
                    , label =
                        el
                            (Style.button
                                ++ [ --, width <| px <| (viewWidth |> flip (/) 4)
                                     height <| px 40
                                   , class "send-message"
                                   ]
                            )
                        <|
                            text "send"
                    }
              else
                button []
                    { onPress = Nothing
                    , label =
                        el
                            (Style.button
                                ++ [ -- width <| px <| (viewWidth |> flip (/) 4)
                                     height <| px 40
                                   , class "conn-lost"
                                   ]
                            )
                        <|
                            text "🚫"
                    }
            ]


viewTyping : Time -> Time -> Element msg
viewTyping currentTime lastSeenTyping =
    when ((currentTime - lastSeenTyping) < 5000) <|
        image
            [ class "typing" ]
            { src = "/typing.svg", description = "is-typing" }


msgCard : Message -> Element msg
msgCard message =
    case message of
        Self content ->
            paragraph (Style.msgSelf ++ [ padding 4 ]) [ text content ]

        Them content ->
            paragraph (Style.msgThem ++ [ class "message", padding 4 ]) [ text content ]

        ChatStart ->
            paragraph (Style.msgSys ++ [ padding 4, width fill ]) [ text "Ready to chat!" ]

        ConnEnd ->
            paragraph (Style.msgSys ++ [ padding 4 ]) [ text "Connection lost!" ]


onScroll : (Json.Encode.Value -> msg) -> Attribute msg
onScroll msg =
    Json.Decode.map msg Json.Decode.value
        |> on "scroll"
        |> attribute


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
        |> attribute
