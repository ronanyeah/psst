module View exposing (view)

import Animation
import Element exposing (Attribute, Element, button, circle, column, el, empty, image, paragraph, row, text, screen, viewport, when)
import Element.Attributes exposing (alignBottom, alignLeft, attribute, center, class, fill, height, id, padding, px, spacing, maxHeight, maxWidth, moveUp, verticalCenter, width, percent, vary, scrollbars)
import Element.Events exposing (on, onClick, keyCode)
import Element.Input as Input
import Json.Decode
import Json.Encode
import Html exposing (Html)
import Styling exposing (Styles(..), Variations(..), styling)
import Time exposing (Time)
import Types exposing (Message(..), Model, Msg(..), ChatId(..), Status(..))


view : Model -> Html Msg
view { status, device, keySpin, origin, time, arrow, shareEnabled, copyEnabled } =
    let
        viewWidth =
            clamp 0 425 device.width
                |> toFloat

        keySpinner =
            column None
                [ center, verticalCenter, height fill ]
                [ image None
                    (List.concat
                        [ Animation.render keySpin |> List.map Element.Attributes.toAttr
                        , [ width <| px <| (viewWidth |> flip (/) 3 |> min 100) ]
                        ]
                    )
                    { src = "/antenna.svg", caption = "key-spinner" }
                ]
    in
        viewport styling <|
            column Body
                [ height fill
                , width fill
                , center
                , verticalCenter
                ]
                [ case status of
                    Start ->
                        column None
                            [ center, verticalCenter ]
                            [ circle (viewWidth |> flip (/) 3)
                                StartCircle
                                [ class "start-circle", onClick CreateChat, center, verticalCenter ]
                              <|
                                el None [ center, verticalCenter ] <|
                                    text
                                        "Start"
                            ]

                    AWaitingForBKey _ (ChatId chatId) ->
                        let
                            chatlink =
                                origin ++ "#" ++ chatId
                        in
                            column None
                                [ center, verticalCenter, height fill, width fill ]
                                [ paragraph ShareThis
                                    [ padding 10 ]
                                    [ text "Share this:" ]
                                , paragraph Link [ class "chat-link", padding 10 ] [ text chatlink ]
                                , when copyEnabled <|
                                    button Button
                                        [ class "copy-button"
                                        , attribute "data-clipboard-text" chatlink
                                        ]
                                    <|
                                        text "COPY"
                                , when shareEnabled <|
                                    button Button [ onClick <| Share chatlink ] <|
                                        text "SHARE"
                                ]

                    BJoining ->
                        keySpinner

                    BWaitingForAKey _ ->
                        keySpinner

                    InChat { lastSeenTyping, messages, input, isLive } ->
                        column Body
                            [ width <| px viewWidth, height fill, center ]
                            [ column Body
                                [ spacing 7
                                , padding 7
                                , id "messages"
                                , scrollbars
                                , onScroll DisplayScrollButton
                                ]
                              <|
                                List.map msgCard messages
                            , viewTyping time lastSeenTyping
                            , when arrow <|
                                screen <|
                                    circle 20
                                        StartCircle
                                        [ onClick ScrollToBottom, alignLeft, alignBottom, moveUp 40 ]
                                        empty
                            , el None [ height <| px 40 ] empty
                            , screen <|
                                el None
                                    [ alignBottom, center ]
                                <|
                                    row None
                                        [ padding 2, spacing 2 ]
                                        [ Input.multiline MessageBox
                                            [ height <| px 40
                                            , width <|
                                                px <|
                                                    (viewWidth
                                                        |> flip (/) 4
                                                        |> (*) 3
                                                    )
                                            , onPressEnter Send
                                            , class "message-input"
                                            ]
                                            { onChange = InputChange
                                            , value = input
                                            , label = Input.hiddenLabel "input"
                                            , options =
                                                if isLive then
                                                    []
                                                else
                                                    [ Input.disabled ]
                                            }
                                        , if isLive then
                                            button Button
                                                [ onClick Send
                                                , width <| px <| (viewWidth |> flip (/) 4)
                                                , height <| px 40
                                                , class "send-message"
                                                ]
                                            <|
                                                text "send"
                                          else
                                            button Button
                                                [ width <| px <| (viewWidth |> flip (/) 4)
                                                , height <| px 40
                                                , class "conn-lost"
                                                ]
                                            <|
                                                text "🚫"
                                        ]
                            ]

                    ErrorView txt ->
                        text txt
                ]


viewTyping : Time -> Time -> Element Styles vars msg
viewTyping currentTime lastSeenTyping =
    when ((currentTime - lastSeenTyping) < 5000) <|
        image None
            [ class "typing" ]
            { src = "/typing.svg", caption = "is-typing" }


msgCard : Message -> Element Styles Variations msg
msgCard message =
    case message of
        Self content ->
            paragraph MsgSelf [ padding 4 ] [ text content ]

        Them content ->
            paragraph MsgThem [ class "message", padding 4 ] [ text content ]

        ChatStart ->
            paragraph MsgSys [ padding 4, width fill ] [ text "Ready to chat!" ]

        ConnEnd ->
            paragraph MsgSys [ padding 4 ] [ text "Connection lost!" ]


onScroll : (Json.Encode.Value -> msg) -> Attribute variation msg
onScroll msg =
    Json.Decode.map msg Json.Decode.value
        |> on "scroll"


onPressEnter : msg -> Attribute variation msg
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
