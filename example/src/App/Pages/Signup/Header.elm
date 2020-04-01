module App.Pages.Signup.Header exposing (dropdownOverlay, mobileView, view)

import DesignSystem exposing (Border(..), BorderRadius(..), Color(..), Font(..), FontSize(..), Length(..), Padding(..), Spacing(..))
import DesignSystem.Input
import Element exposing (Element)
import Element.Font


view : Bool -> Bool -> Element msg
view showLogin showSignup =
    Element.row
        [ Element.width Element.fill ]
        [ Element.image
            [ DesignSystem.padding P4 ]
            { src = "/assets/logo.svg", description = "Insurello logo" }
        , Element.row
            [ Element.alignRight
            , DesignSystem.spacing S8
            , DesignSystem.fontColor Blue500Insurello
            , DesignSystem.padding P4
            ]
            [ Element.link [] { url = "https://www.insurello.se/om-oss/", label = aboutUsText }
            , if showLogin then
                loginLink

              else
                Element.none
            , if showSignup then
                signupLink

              else
                Element.none
            ]
        ]


loginLink : Element msg
loginLink =
    Element.link [] { url = loginUrl, label = logInText }


signupLink : Element msg
signupLink =
    Element.link
        [ DesignSystem.borderRadius Rounded
        , DesignSystem.border B2
        , DesignSystem.borderColor Blue500Insurello
        , Element.alignBottom
        , Element.width Element.fill
        , DesignSystem.paddingEach P2 P8 P2 P8
        ]
        { url = signupUrl
        , label =
            Element.el
                (Element.centerX
                    :: DesignSystem.font Muli700Bold
                )
                (Element.text "Skapa Ärende")
        }


dropdownBottomButton : String -> String -> Element msg
dropdownBottomButton url text =
    Element.link
        [ DesignSystem.borderRadius Rounded
        , DesignSystem.border B2
        , DesignSystem.borderColor White
        , Element.alignBottom
        , Element.width Element.fill
        , DesignSystem.paddingEach P2 P2 P3 P2
        ]
        { url = url
        , label =
            Element.el
                (Element.centerX
                    :: DesignSystem.fontSize TextXl
                    :: DesignSystem.font Muli700Bold
                )
                (Element.text text)
        }


loginUrl : String
loginUrl =
    "https://app.insurello.se/cases/"


signupUrl : String
signupUrl =
    "https://app.insurello.se/"


dropdownOverlay : msg -> Bool -> Bool -> Element msg
dropdownOverlay toggleDropdown showLogin showSignup =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , DesignSystem.backgroundColor Blue400Sky
        , Element.inFront <|
            DesignSystem.Input.button [ Element.alignRight, DesignSystem.padding P8 ]
                { onPress = toggleDropdown
                , label = Element.image [] { src = "/assets/xButton.svg", description = "Stäng" }
                }
        , Element.Font.size 24
        , DesignSystem.fontColor White
        ]
        (Element.column
            [ DesignSystem.padding P8, Element.spacing 48, Element.width Element.fill, Element.height Element.fill ]
            [ Element.image
                [ DesignSystem.width L8 ]
                { src = "/assets/symbol.svg", description = "Insurello logo" }
            , Element.column
                [ DesignSystem.spacing S8 ]
                [ Element.link [] { url = "https://www.insurello.se/", label = Element.text "Hem" }
                , Element.link [] { url = "https://www.insurello.se/om-oss/", label = aboutUsText }
                ]
            , if showLogin then
                dropdownBottomButton loginUrl "Logga in"

              else if showSignup then
                dropdownBottomButton signupUrl "Skapa Ärende"

              else
                Element.none
            ]
        )


aboutUsText : Element msg
aboutUsText =
    Element.text "Om oss"


logInText : Element msg
logInText =
    Element.text "Logga in"


toggleDropdownButton : msg -> Element msg
toggleDropdownButton toggleDropdown =
    let
        line =
            Element.el
                [ Element.width Element.fill
                , Element.height <| Element.px 2
                , DesignSystem.backgroundColor Blue500Insurello
                ]
                Element.none
    in
    DesignSystem.Input.button
        [ Element.paddingXY 16 4, Element.alignRight, Element.height Element.fill ]
        { onPress = toggleDropdown
        , label =
            Element.column
                [ DesignSystem.spacing S2, Element.width <| Element.px 32, Element.centerY ]
                [ line, line, line ]
        }


mobileView : msg -> Element msg
mobileView toggleDropdown =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 64
        , Element.inFront <|
            Element.image
                [ Element.centerX
                , Element.centerY
                , Element.height <| Element.px 32
                ]
                { src = "/assets/logo.svg", description = "Insurello logo" }
        ]
        (toggleDropdownButton toggleDropdown)
