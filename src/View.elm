module View exposing (view)

import Animation
import Element exposing (Attribute, Element, button, circle, column, el, empty, image, paragraph, row, text, screen, viewport, when)
import Element.Attributes exposing (alignBottom, alignLeft, attribute, center, class, fill, height, id, padding, px, spacing, moveUp, verticalCenter, width, percent, vary, scrollbars)
import Element.Events exposing (on, onClick, keyCode)
import Element.Input as Input
import Json.Decode
import Json.Encode
import Html exposing (Html)
import Styling exposing (Styles(..), Variations(..), styling)
import Time exposing (Time)
import Types exposing (Message, Model, Msg(..), RoomId(..), Status(..), TypingStatus(..))


view : Model -> Html Msg
view { status, device, keySpin, location, time, arrow, shareEnabled, copyEnabled } =
    let
        keySpinner =
            column None
                [ center, verticalCenter, height fill ]
                [ image None
                    (List.concat
                        [ Animation.render keySpin |> List.map Element.Attributes.toAttr
                        , [ width <| px <| (device.width |> toFloat |> flip (/) 3) ]
                        ]
                    )
                    { src = "/antenna.svg", caption = "key-spinner" }
                ]
    in
        viewport styling <|
            column Body
                [ height fill, width fill ]
                [ case status of
                    AWaitingForBKey _ (RoomId roomId) ->
                        let
                            roomlink =
                                location ++ "#" ++ roomId
                        in
                            column None
                                [ center, verticalCenter, height fill, width fill, spacing 10 ]
                                [ paragraph ShareThis
                                    [ padding 10 ]
                                    [ text "Share this:" ]
                                , paragraph Link [ class "room-link", padding 10 ] [ text roomlink ]
                                , when copyEnabled <|
                                    button Button
                                        [ class "copy-button"
                                        , attribute "data-clipboard-text" roomlink
                                        ]
                                    <|
                                        text "COPY"
                                , when shareEnabled <|
                                    button Button [ onClick <| Share roomlink ] <|
                                        text "SHARE"
                                ]

                    BWaitingForAKey _ ->
                        keySpinner

                    BJoining ->
                        keySpinner

                    InChat { typingStatus, messages, input, isLive } ->
                        column Body
                            [ width <| percent 100, height <| percent 100 ]
                            [ column Body
                                [ spacing 7
                                , padding 7
                                , id "messages"
                                , scrollbars
                                , onScroll DisplayScrollButton
                                ]
                              <|
                                List.map msgCard messages
                            , viewTyping time typingStatus
                            , when (not isLive) <|
                                button Button
                                    [ onClick ExitChat
                                    , width <| px <| (device.width |> toFloat |> flip (/) 4)
                                    , height <| px 40
                                    ]
                                <|
                                    text "EXIT"
                            , when arrow <|
                                screen <|
                                    circle 20
                                        StartCircle
                                        [ onClick ScrollToBottom, alignLeft, alignBottom, moveUp 40 ]
                                        empty
                            , el None [ height <| px 40 ] empty
                            , when isLive <|
                                screen <|
                                    el None
                                        [ alignBottom ]
                                    <|
                                        row None
                                            []
                                            [ Input.text None
                                                [ height <| px 40
                                                , width <| px <| (device.width |> toFloat |> flip (/) 4 |> (*) 3)
                                                , onPressEnter Send
                                                , class "message-input"
                                                ]
                                                { onChange = InputChange
                                                , value = input
                                                , label = Input.hiddenLabel "input"
                                                , options = []
                                                }
                                            , button Button
                                                [ onClick Send
                                                , width <| px <| (device.width |> toFloat |> flip (/) 4)
                                                , height <| px 40
                                                , class "send-message"
                                                ]
                                              <|
                                                text "send"
                                            ]
                            , when (not isLive) <|
                                screen <|
                                    el DeadConn
                                        [ class "conn-lost", alignBottom, width fill, height <| px 40 ]
                                    <|
                                        el None
                                            [ center
                                            , verticalCenter
                                            ]
                                        <|
                                            text "LOST CONNECTION"
                            ]

                    Start ->
                        column None
                            [ center, verticalCenter, height fill ]
                            [ circle (device.width |> toFloat |> flip (/) 3)
                                StartCircle
                                [ class "start-circle", onClick StartMsg ]
                              <|
                                el None [ center, verticalCenter ] <|
                                    text
                                        "Start"
                            ]

                    ErrorView txt ->
                        text txt
                ]


viewTyping : Time -> TypingStatus -> Element Styles vars msg
viewTyping time status =
    case status of
        IsTyping typingAt ->
            when
                ((time - typingAt)
                    < 5000
                )
            <|
                image None
                    [ class "typing" ]
                    { src = "/typing.svg", caption = "is-typing" }

        NotTyping ->
            empty


msgCard : Message -> Element Styles Variations msg
msgCard { self, content } =
    paragraph MsgCard [ class "message", padding 4, vary Self self ] [ text content ]


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
