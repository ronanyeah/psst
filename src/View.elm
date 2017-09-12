module View exposing (view)

import Animation
import Element exposing (Attribute, Element, button, circle, column, el, empty, image, paragraph, row, text, screen, viewport, when)
import Element.Attributes exposing (alignBottom, alignLeft, attribute, center, class, height, id, padding, px, spacing, moveUp, verticalCenter, width, percent, vary, scrollbars)
import Element.Events exposing (on, onClick, keyCode)
import Element.Input as Input
import Json.Decode
import Json.Encode
import Html exposing (Html)
import Styling exposing (Styles(..), Variations(..), styling)
import Time exposing (Time)
import Types exposing (Message, Model, Msg(..), RoomId(..), Status(..), TypingStatus(..))


view : Model -> Html Msg
view { status, device, messages, input, keySpin, location, time, arrow } =
    let
        keySpinner =
            image None
                (List.concat
                    [ Animation.render keySpin |> List.map Element.Attributes.toAttr
                    , [ width <| px <| (device.width |> toFloat |> flip (/) 3) ]
                    ]
                )
                { src = "/car-key.svg", caption = "key-spinner" }

        wrapBody body =
            column Body
                [ center, verticalCenter, width <| percent 100, height <| percent 100 ]
                [ body ]
    in
        viewport styling <|
            case status of
                WaitingForBKey _ _ (RoomId roomId) ->
                    let
                        roomlink =
                            location ++ "#" ++ roomId
                    in
                        wrapBody <|
                            column None
                                [ center ]
                                [ paragraph ShareThis
                                    []
                                    [ text "Share this link with someone to begin chat:" ]
                                , paragraph Link [ padding 10 ] [ text roomlink ]
                                , button Button
                                    [ class "copy-button"
                                    , attribute "data-clipboard-text" roomlink
                                    ]
                                  <|
                                    text "COPY"
                                ]

                WaitingForAKey _ ->
                    wrapBody <| keySpinner

                Joining _ ->
                    wrapBody <| keySpinner

                Ready _ _ typingStatus ->
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
                        , when arrow <|
                            screen <|
                                circle 20
                                    StartCircle
                                    [ onClick ScrollToBottom, alignLeft, alignBottom, moveUp 40 ]
                                    empty
                        , el None [ height <| px 40 ] empty
                        , screen <|
                            el None
                                [ alignBottom ]
                            <|
                                row None
                                    []
                                    [ Input.text None
                                        [ height <| px 40
                                        , width <| px <| (device.width |> toFloat |> flip (/) 4 |> (*) 3)
                                        , onPressEnter Send
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
                                        ]
                                      <|
                                        text "send"
                                    ]
                        ]

                Start _ ->
                    wrapBody <|
                        circle (device.width |> toFloat |> flip (/) 3)
                            StartCircle
                            [ onClick Init, center, verticalCenter ]
                        <|
                            el None [ center, verticalCenter ] <|
                                text
                                    "Start"


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
                    []
                    { src = "/typing.svg", caption = "is-typing" }

        NotTyping ->
            empty


msgCard : Message -> Element Styles Variations msg
msgCard { self, content } =
    paragraph MsgCard [ padding 4, vary Self self ] [ text content ]


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
