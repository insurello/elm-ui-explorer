module App.Pages.Signup.Testimonials exposing (desktopView, mobileView, trustpilot)

import DesignSystem exposing (Border(..), BorderRadius(..), Color(..), Font(..), FontSize(..), Padding(..), Spacing(..))
import DesignSystem.Input
import DesignSystem.Views
import Element exposing (Element)
import Element.Font
import Html
import Html.Attributes


desktopView : msg -> Element msg
desktopView getStartedMsg =
    Element.column
        [ Element.width Element.fill, Element.spacing 64 ]
        [ headerAndTrustpilot
        , Element.column
            [ Element.spacing 32, Element.width Element.fill ]
            [ Element.row
                [ Element.centerX, Element.width <| Element.maximum 1200 Element.fill ]
                [ DesignSystem.Views.horizontalFiller
                , sebastianReview review
                , DesignSystem.Views.horizontalFiller
                , rebeckaReview review
                , DesignSystem.Views.horizontalFiller
                ]
            , getStartedButton getStartedMsg
            ]
        ]


sebastianReview : (Review -> Element msg) -> Element msg
sebastianReview reviewView =
    reviewView
        { starIcon = "/elm-ui-explorer/assets/starFilled.svg"
        , personIcon = "/elm-ui-explorer/assets/sebastianCustomerAvatar.png"
        , text = "Det var oerhört smidigt, jag ansökte och skickade med de intygen som behövdes och resten fixade ni!"
        , name = "Sebastian"
        , injury = "Arbetsskada"
        , readMoreUrl = "https://www.insurello.se/testimonials/sebastian-30-000-kr/"
        , payment = "30 000"
        }


rebeckaReview : (Review -> Element msg) -> Element msg
rebeckaReview reviewView =
    reviewView
        { starIcon = "/elm-ui-explorer/assets/starFilled.svg"
        , personIcon = "/elm-ui-explorer/assets/rebeckaCustomerAvatar.png"
        , text = "Jag läste på om Insurello och fastnade direkt! Dom skötte verkligen allting åt mig"
        , name = "Rebecka"
        , injury = "Handsskada"
        , readMoreUrl = "https://www.insurello.se/testimonials/rebecka-18-058-kr/"
        , payment = "18 058"
        }


headerAndTrustpilot =
    Element.column
        [ Element.spacing 36, Element.width Element.fill ]
        [ header, Element.el [ Element.centerX ] trustpilot ]


mobileView : msg -> Element msg
mobileView getStartedMsg =
    Element.column
        [ Element.width Element.fill, Element.spacing 64 ]
        [ headerAndTrustpilot
        , Element.column
            [ Element.width Element.fill, Element.spacing 32 ]
            [ Element.column
                [ Element.centerX, Element.spacing 64 ]
                [ sebastianReview reviewMobile, rebeckaReview reviewMobile ]
            , getStartedButton getStartedMsg
            ]
        ]


triedAndTrue : Element msg
triedAndTrue =
    Element.paragraph
        (DesignSystem.fontColor Gold500
            :: Element.centerX
            :: Element.spacing 4
            :: Element.Font.center
            :: DesignSystem.font Muli700Bold
        )
        [ Element.text (String.toUpper "Omtyckt och bevisad") ]


checkOutWhatCustomerAreSaying : Element msg
checkOutWhatCustomerAreSaying =
    Element.paragraph
        (Element.Font.center
            :: Element.width (Element.maximum 400 Element.fill)
            :: Element.centerX
            :: DesignSystem.fontSize Text3Xl
            :: DesignSystem.spacing S2
            :: DesignSystem.font Merriweather700Bold
        )
        [ Element.text "Det här säger våra kunder om oss" ]


header : Element msg
header =
    Element.column
        [ Element.width Element.fill, Element.spacing 22 ]
        [ triedAndTrue, checkOutWhatCustomerAreSaying ]


getStartedButton : msg -> Element msg
getStartedButton onPress =
    DesignSystem.Input.button
        (DesignSystem.border B2
            :: DesignSystem.borderRadius Rounded
            :: DesignSystem.borderColor Blue500Insurello
            :: DesignSystem.paddingEach P3 P8 P3 P8
            :: Element.centerX
            :: DesignSystem.fontColor Blue500Insurello
            :: Element.mouseOver
                [ DesignSystem.fontColor Blue600Pacific
                , DesignSystem.borderColor Blue600Pacific
                ]
            :: DesignSystem.font Muli700Bold
        )
        { onPress = onPress, label = Element.text "Kom igång" }


trustpilot : Element msg
trustpilot =
    Element.el
        [ Element.height <| Element.px 30, Element.clip ]
        (Element.html <|
            Html.iframe
                [ Html.Attributes.srcdoc
                    """
<head>
    <script type="text/javascript" src="//widget.trustpilot.com/bootstrap/v5/tp.widget.bootstrap.min.js" async></script>
</head>
<div class="result--stars">
    <div id="trustpilot-rating" class="trustpilot-widget rating" data-locale="sv-SE" data-template-id="5419b732fbfb950b10de65e5" data-businessunit-id="5c13cb62e4be9d0001d057a5" data-style-height="24px" data-style-width="100%" data-theme="light">
        <a href="https://se.trustpilot.com/review/www.insurello.se" target="_blank" rel="noopener">Trustpilot</a>
    </div>
</div>"""
                , Html.Attributes.style "border-width" "0"
                ]
                []
        )


fiveStars : String -> Element msg
fiveStars starIcon =
    Element.image [] { src = starIcon, description = "Stjärna" }
        |> List.repeat 5
        |> Element.row [ Element.spacing 16, Element.centerX ]


type alias Review =
    { starIcon : String
    , personIcon : String
    , text : String
    , name : String
    , injury : String
    , readMoreUrl : String
    , payment : String
    }


review : Review -> Element msg
review { starIcon, personIcon, text, name, injury, readMoreUrl, payment } =
    let
        nameInjuryPayment =
            Element.column
                [ Element.width Element.fill, Element.spacing 8 ]
                [ Element.el (Element.centerX :: Element.spacing 2 :: DesignSystem.font Muli700Bold) (Element.text name)
                , Element.el [ Element.centerX ] (Element.text injury)
                , payment ++ " kr" |> Element.text |> Element.el [ Element.centerX ]
                ]

        starsAndDescription =
            Element.column
                [ Element.spacing 14 ]
                [ fiveStars starIcon
                , Element.paragraph
                    (Element.Font.italic
                        :: Element.spacing 12
                        :: Element.Font.center
                        :: Element.width Element.fill
                        :: DesignSystem.font Merriweather400Regular
                    )
                    [ Element.text (text ++ "...")
                    , Element.newTabLink
                        [ DesignSystem.fontColor Blue500Insurello ]
                        { url = readMoreUrl, label = readMore }
                    , Element.text "."
                    ]
                ]
    in
    Element.column
        [ Element.width <| Element.px 489, Element.spacing 20 ]
        [ starsAndDescription
        , Element.column
            [ Element.spacing 10, Element.width Element.fill ]
            [ Element.image
                [ Element.centerX, Element.width <| Element.px 85 ]
                { src = personIcon, description = name }
            , nameInjuryPayment
            ]
        ]


reviewMobile : Review -> Element msg
reviewMobile { starIcon, personIcon, text, name, injury, readMoreUrl, payment } =
    let
        nameInjuryPayment =
            Element.column
                [ Element.width Element.fill, Element.spacing 8 ]
                [ Element.el (Element.centerX :: Element.spacing 2 :: DesignSystem.font Muli700Bold) (Element.text name)
                , Element.el [ Element.centerX ] (Element.text injury)
                , payment ++ " kr" |> Element.text |> Element.el [ Element.centerX ]
                ]

        starsAndDescription =
            Element.column
                [ Element.spacing 18 ]
                [ fiveStars starIcon
                , Element.paragraph
                    (Element.Font.italic
                        :: Element.spacing 12
                        :: Element.Font.center
                        :: Element.width Element.fill
                        :: DesignSystem.font Merriweather400Regular
                    )
                    [ Element.text (text ++ "...")
                    , Element.newTabLink
                        [ DesignSystem.fontColor Blue500Insurello ]
                        { url = readMoreUrl, label = readMore }
                    , Element.text "."
                    ]
                ]
    in
    Element.column
        [ Element.width <| Element.maximum 343 Element.fill, Element.spacing 20 ]
        [ starsAndDescription
        , Element.column
            [ Element.spacing 10, Element.width Element.fill ]
            [ Element.image
                [ Element.centerX, Element.width <| Element.px 104 ]
                { src = personIcon, description = name }
            , nameInjuryPayment
            ]
        ]


readMore =
    -- Non breaking space to keep the link together
    Element.text "Läs\u{00A0}mer"
