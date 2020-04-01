module DesignSystem.Input exposing (ButtonState(..), button, buttonEnableAttributes, onEnter, primaryButton, submitButton, text)

import DesignSystem exposing (BorderRadius(..), Color(..), Padding(..))
import Element exposing (Attribute, Element)
import Element.Background
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode


buttonDisableAttributes : List (Element.Attribute msg)
buttonDisableAttributes =
    [ Element.Background.color (DesignSystem.color BootstrapDisabled)
    , Element.Font.color (DesignSystem.color BootstrapMutedText)
    , DesignSystem.borderRadius Rounded
    , DesignSystem.paddingXY P8 P3
    , Element.htmlAttribute (Html.Attributes.attribute "aria-disabled" "true")
    ]


buttonEnableAttributes : DesignSystem.Color -> List (Element.Attribute msg)
buttonEnableAttributes backgroundColor =
    [ Element.Background.color (DesignSystem.color backgroundColor)
    , DesignSystem.borderRadius Rounded
    , Element.Font.color (DesignSystem.color White)
    , DesignSystem.paddingXY P8 P3
    ]


type ButtonState onPressMsg
    = Enabled onPressMsg
    | EnabledWithoutMsg
    | FakeDisabled onPressMsg
    | Disabled


primaryButton : ButtonState msg -> Element msg -> Element msg
primaryButton buttonState label =
    case buttonState of
        Enabled onPressMsg ->
            button
                (buttonEnableAttributes Blue500Insurello)
                { onPress = onPressMsg
                , label = label
                }

        EnabledWithoutMsg ->
            Element.Input.button
                (buttonEnableAttributes Blue500Insurello)
                { onPress = Nothing
                , label = label
                }

        FakeDisabled onPressMsg ->
            button
                buttonDisableAttributes
                { onPress = onPressMsg
                , label = label
                }

        Disabled ->
            Element.Input.button
                buttonDisableAttributes
                { onPress = Nothing
                , label = label
                }


submitButton : ButtonState msg -> Element msg -> Element msg
submitButton buttonState label =
    case buttonState of
        Enabled onPressMsg ->
            button
                (buttonEnableAttributes Green500Gogo)
                { onPress = onPressMsg
                , label = label
                }

        EnabledWithoutMsg ->
            Element.Input.button
                (buttonEnableAttributes Green500Gogo)
                { onPress = Nothing
                , label = label
                }

        FakeDisabled onPressMsg ->
            button
                buttonDisableAttributes
                { onPress = onPressMsg
                , label = label
                }

        Disabled ->
            Element.Input.button
                buttonDisableAttributes
                { onPress = Nothing
                , label = label
                }


{-| Basic button with no built-in styling.
-}
button : List (Attribute msg) -> { onPress : msg, label : Element msg } -> Element msg
button attributes { onPress, label } =
    Element.Input.button
        attributes
        { onPress = Just onPress
        , label = label
        }


text :
    List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Element.Input.Placeholder msg)
        , label : Element.Input.Label msg
        }
    -> Element msg
text attrs config =
    Element.Input.text
        ([ Element.width Element.fill
         , DesignSystem.paddingXY P2 P2
         , DesignSystem.borderColor BootstrapInputBorder
         ]
            ++ attrs
        )
        config


{-| Useful if you want something other than a button to trigger when pressing the enter key.
-}
onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
