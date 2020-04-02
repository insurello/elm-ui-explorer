module App.Pages.Signup.Process exposing (BackgroundType(..), desktopView, mobileView)

import DesignSystem exposing (Color(..), Font(..), FontSize(..), Padding(..), Spacing(..))
import Element exposing (Element)
import Element.Background
import Element.Font
import Html.Attributes


theProcessTitle : Element msg
theProcessTitle =
    Element.el
        (DesignSystem.fontColor Gold500
            :: Element.centerX
            :: DesignSystem.font Muli700Bold
        )
        (Element.text (String.toUpper "Processen"))


curiousAboutNextSteps : Element msg
curiousAboutNextSteps =
    Element.paragraph
        (DesignSystem.fontSize Text3Xl
            :: Element.centerX
            :: DesignSystem.font Merriweather700Bold
        )
        [ Element.text "Nyfiken på nästa steg?" ]


theProcessHeader : Element msg
theProcessHeader =
    Element.column
        [ Element.Font.center, Element.spacing 20, Element.centerX ]
        [ Element.column
            [ Element.spacing 22 ]
            [ theProcessTitle
            , curiousAboutNextSteps
            ]
        , Element.paragraph
            (Element.width (Element.px 360)
                :: Element.centerX
                :: DesignSystem.spacing S2
                :: DesignSystem.font Muli400Regular
            )
            [ fourSimpleSteps ]
        ]


theProcessHeaderMobile : Element msg
theProcessHeaderMobile =
    Element.column
        [ Element.Font.center, Element.spacing 20, Element.centerX ]
        [ Element.column
            [ Element.spacing 22, Element.centerX ]
            [ theProcessTitle
            , Element.el [ Element.width <| Element.maximum 300 Element.shrink, Element.centerX ] curiousAboutNextSteps
            ]
        , Element.paragraph
            (Element.width (Element.maximum 360 Element.fill)
                :: Element.centerX
                :: DesignSystem.spacing S2
                :: DesignSystem.font Muli400Regular
            )
            [ fourSimpleSteps ]
        ]


fourSimpleSteps =
    Element.text "Processen består av fyra enkla steg. När vi har all information kan du luta dig tillbaka och göra annat."


type BackgroundType
    = WhiteBackground
    | BlueBackground


youStartSteps : List ( String, String )
youStartSteps =
    [ ( "/elm-ui-explorer/assets/on_blue_background/bankid", "Logga in via BankID" )
    , ( "/elm-ui-explorer/assets/on_blue_background/claimForm", "Fyll i formuläret" )
    , ( "/elm-ui-explorer/assets/on_blue_background/powerOfAttorney", "Signera fullmakten digitalt" )
    , ( "/elm-ui-explorer/assets/on_blue_background/myPages", "Svara på följdfrågor" )
    ]


weFinishSteps : List ( String, String, String )
weFinishSteps =
    [ ( "/elm-ui-explorer/assets/on_blue_background/caseReview.svg"
      , "Först till kvarn"
      , "Vi har normalt en handläggningstid på ca 5-6 veckor på grund av stor efterfrågan. När vi sätter igång kommer vi att utvärdera ditt ärende direkt och eventuellt hitta ersättning."
      )
    , ( "/elm-ui-explorer/assets/on_blue_background/waitTime.svg"
      , "Vi sköter dialogen med försäkringsbolaget"
      , "Våra handläggare tar hand om allt pappersarbete och för din talan för att hjälpa dig till rättvis ersättning. Handläggningstiden beror på vilka försäkringar du täcks av och varierar mellan försäkringsbolag, från några veckor till flera månader."
      )
    , ( "/elm-ui-explorer/assets/on_blue_background/paid.svg"
      , "Pengar på kontot!"
      , "Vi uppdaterar dig kontinuerligt i hanteringen av ärendet och meddelar dig snabbt vid beslut. Om du har rätt till ersättning skickar vi pengarna direkt in på ditt konto."
      )
    ]


theProcessTimeline : Element msg
theProcessTimeline =
    let
        verboseStep title body =
            Element.column
                [ Element.width <| Element.px 424, Element.spacing 6 ]
                [ Element.paragraph (DesignSystem.spacing S2 :: DesignSystem.font Muli700Bold) [ Element.text title ]
                , Element.paragraph [ DesignSystem.spacing S2 ] [ Element.text body ]
                ]

        stepView icon text =
            Element.row [ Element.spacing 25 ] [ Element.image [] { src = icon, description = "" }, text ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 35
        ]
        [ Element.el
            (Element.Font.size 24 :: DesignSystem.font Muli700Bold)
            weStartText
        , Element.column
            [ Element.spacing 32 ]
            [ youStartSteps
                |> List.map (\( icon, text ) -> stepView icon (Element.text text))
                |> Element.column [ Element.spacing 64, Element.behindContent dottedVerticalLine ]
            , Element.column
                [ Element.spacing 28 ]
                [ Element.image
                    [ Element.moveRight ((iconWidth - downArrowWidth) / 2) ]
                    { src = "/elm-ui-explorer/assets/downArrow.svg", description = "" }
                , Element.el
                    (Element.Font.size 24 :: DesignSystem.font Muli700Bold)
                    weFinishText
                ]
            , weFinishSteps
                |> List.map (\( icon, title, body ) -> stepView icon (verboseStep title body))
                |> Element.column
                    [ Element.spacing 64
                    , Element.behindContent
                        (Element.el
                            -- Padding is needed to prevent the dotted line from starting above the icon
                            [ Element.paddingXY 0 10, Element.height Element.fill ]
                            dottedVerticalLine
                        )
                    ]
            ]
        ]


theProcessTimelineMobile : Element msg
theProcessTimelineMobile =
    let
        verboseStep title body =
            Element.column
                [ DesignSystem.spacing S2
                , Element.Font.center
                , Element.width <| Element.maximum 400 Element.shrink
                ]
                [ Element.paragraph
                    (DesignSystem.spacing S2 :: DesignSystem.font Merriweather700Bold)
                    [ Element.text title ]
                , Element.paragraph
                    [ DesignSystem.spacing S2 ]
                    [ Element.text body ]
                ]

        stepView icon text =
            Element.column
                [ Element.spacing 12, Element.centerX ]
                [ Element.image [ Element.centerX ] { src = icon, description = "" }, text ]

        arrow =
            Element.image [ Element.centerX ] { src = "/elm-ui-explorer/assets/downArrow.svg", description = "" }
    in
    Element.column
        [ Element.spacing 20
        , Element.width Element.fill
        ]
        [ Element.el
            (Element.centerX
                :: Element.Font.size 24
                :: DesignSystem.font Muli700Bold
            )
            weStartText
        , youStartSteps
            |> List.map (\( icon, text ) -> stepView icon (Element.text text))
            |> List.intersperse arrow
            |> Element.column [ Element.spacing 18, Element.width Element.fill ]
        , Element.column
            [ Element.spacing 18, Element.width Element.fill ]
            [ arrow
            , Element.el
                (Element.centerX :: Element.Font.size 24 :: DesignSystem.font Muli700Bold)
                weFinishText
            ]
        , weFinishSteps
            |> List.map (\( icon, title, body ) -> stepView icon (verboseStep title body))
            |> List.intersperse arrow
            |> Element.column [ Element.spacing 18, Element.width Element.fill ]
        ]


weStartText =
    Element.text "Du börjar"


weFinishText =
    Element.text "Vi sköter resten"


iconWidth =
    96


downArrowWidth =
    20


dottedLineWidth =
    6


dottedVerticalLine : Element msg
dottedVerticalLine =
    Element.el
        [ Element.width <| Element.px dottedLineWidth
        , Element.height Element.fill
        , Element.moveRight ((iconWidth - dottedLineWidth) / 2)
        , Element.Background.tiled "/elm-ui-explorer/assets/on_blue_background/dottedLine.svg"
        ]
        Element.none


desktopView : Element msg
desktopView =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 29
        ]
        [ theProcessHeader
        , Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.image
                    [ Element.centerX, Element.moveDown 130 ]
                    { src = "/elm-ui-explorer/assets/on_blue_background/illustrationWomanApp.svg", description = "" }
                )
            , Element.el
                [ Element.width Element.fill
                , Element.moveLeft (iconWidth / 2)
                ]
                theProcessTimeline
            ]
        ]


mobileView : BackgroundType -> Element msg
mobileView backgroundType =
    Element.column
        [ Element.width Element.fill ]
        [ mobileIllustration backgroundType
        , Element.column
            [ Element.width Element.fill
            , DesignSystem.paddingEach P8 P2 P16 P2
            , Element.spacing 29
            , case backgroundType of
                WhiteBackground ->
                    Element.Background.color (DesignSystem.color White)

                BlueBackground ->
                    Element.Background.color (DesignSystem.color Blue100Cascade)
            ]
            [ theProcessHeaderMobile
            , theProcessTimelineMobile
            ]
        ]


mobileIllustration : BackgroundType -> Element msg
mobileIllustration backgroundType =
    Element.column
        [ Element.inFront <|
            Element.image
                [ Element.centerX, Element.width <| Element.px 223 ]
                { src = "/elm-ui-explorer/assets/on_white_background/illustrationWomanApp.svg", description = "" }
        , Element.width Element.fill
        , Element.height <| Element.px 224
        ]
        [ Element.el
            [ Element.height Element.fill ]
            Element.none
        , Element.image
            [ Element.width Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "pointer-events" "none"
            , Element.padding 0
            ]
            { src = "/elm-ui-explorer/assets/on_blue_background/backgroundWaveCascadeBlue.svg", description = "" }
        , Element.el
            [ Element.height Element.fill
            , Element.width Element.fill
            , case backgroundType of
                WhiteBackground ->
                    Element.Background.color (DesignSystem.color White)

                BlueBackground ->
                    Element.Background.color (DesignSystem.color Blue100Cascade)
            ]
            Element.none
        ]
