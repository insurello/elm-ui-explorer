module App.Pages.Signup.Footer exposing (view, viewWithoutPress)

import DesignSystem exposing (Color(..), Font(..), FontSize(..), Padding(..), Spacing(..))
import DesignSystem.Views
import Element exposing (Element)
import Pixels
import Quantity
import Shared.Types.WindowSize exposing (WindowSize)


view : WindowSize -> Int -> Element msg
view windowSize copyrightYear =
    if windowSize.width |> Quantity.lessThan (Pixels.pixels 1000) then
        mobileView windowSize copyrightYear

    else
        desktopView copyrightYear


viewWithoutPress : WindowSize -> Int -> Element msg
viewWithoutPress windowSize copyrightYear =
    Element.el
        (Element.width Element.fill
            :: DesignSystem.fontColor White
            :: DesignSystem.fontSize TextSm
            :: DesignSystem.font Muli400Regular
        )
    <|
        if windowSize.width |> Quantity.lessThan (Pixels.pixels 1000) then
            bottomPartMobile windowSize copyrightYear

        else
            bottomPart copyrightYear


desktopView : Int -> Element msg
desktopView copyrightYear =
    Element.column
        (Element.width Element.fill
            :: DesignSystem.fontColor White
            :: DesignSystem.fontSize TextSm
            :: DesignSystem.font Muli400Regular
        )
        [ topPart, bottomPart copyrightYear ]


mobileView : WindowSize -> Int -> Element msg
mobileView windowSize copyrightYear =
    Element.column
        (Element.width Element.fill
            :: DesignSystem.fontColor White
            :: DesignSystem.fontSize TextSm
            :: DesignSystem.font Muli400Regular
        )
        [ topPartMobile, bottomPartMobile windowSize copyrightYear ]


newsIcons : List { icon : String, description : String, smallHeight : Int }
newsIcons =
    [ { icon = "/assets/dagensNyheter.svg", description = "Dagens Nyheter", smallHeight = 12 }
    , { icon = "/assets/didigtal.svg", description = "DiDigital", smallHeight = 12 }
    , { icon = "/assets/expressen.svg", description = "Expressen", smallHeight = 12 }
    , { icon = "/assets/sakOchLiv.svg", description = "Sak&Liv", smallHeight = 30 }
    , { icon = "/assets/breakit.svg", description = "Breakit", smallHeight = 12 }
    ]


topPart : Element msg
topPart =
    Element.el
        [ Element.width Element.fill, DesignSystem.backgroundColor Blue700Midnight ]
        (newsIcons
            |> List.map (\{ icon, description } -> Element.image [] { src = icon, description = description })
            |> List.intersperse DesignSystem.Views.horizontalFiller
            |> (\list -> DesignSystem.Views.horizontalFiller :: list ++ [ DesignSystem.Views.horizontalFiller ])
            |> Element.row
                [ Element.centerX
                , Element.centerY
                , Element.width <| Element.maximum 1200 Element.fill
                , DesignSystem.spacing S4
                , Element.height <| Element.px 96
                ]
        )


topPartMobile : Element msg
topPartMobile =
    newsIcons
        |> List.map
            (\{ icon, description, smallHeight } ->
                Element.image
                    [ Element.centerX, Element.height <| Element.px smallHeight ]
                    { src = icon, description = description }
                    |> Element.el [ Element.width Element.fill ]
            )
        |> Element.wrappedRow
            [ Element.width Element.fill
            , DesignSystem.paddingXY P2 P4
            , DesignSystem.spacing S4
            , DesignSystem.backgroundColor Blue700Midnight
            ]


bottomPart : Int -> Element msg
bottomPart copyrightYear =
    Element.el
        [ Element.width Element.fill
        , DesignSystem.backgroundColor Blue800Abyss
        ]
        (Element.el
            [ Element.width Element.fill
            , Element.height <| Element.px 234
            , Element.centerX
            , Element.inFront <| Element.el [ Element.moveDown 154 ] horizontalSplitter
            ]
            (Element.row
                [ Element.spacing 37, Element.centerX, Element.centerY, Element.width <| Element.px 807 ]
                [ logo
                , Element.column [ Element.spacing 72 ]
                    [ linkBlock
                    , legal
                    ]
                , Element.column
                    [ Element.height Element.fill, Element.alignRight ]
                    [ socialMedia 17
                    , Element.el [ Element.alignRight, Element.alignBottom ] (copyright copyrightYear)
                    ]
                ]
            )
        )


logo =
    Element.image
        [ Element.alignTop, Element.moveUp 8 ]
        { src = "/assets/symbol.svg", description = "Insurello logo" }


logoMini =
    Element.image
        [ Element.alignTop, Element.height <| Element.px 43 ]
        { src = "/assets/symbol.svg", description = "Insurello logo" }


horizontalSplitter : Element msg
horizontalSplitter =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 2
        , DesignSystem.backgroundColor Gray500
        ]
        Element.none


bottomPartMobile : WindowSize -> Int -> Element msg
bottomPartMobile windowSize copyrightYear =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 286
        , DesignSystem.backgroundColor Blue800Abyss
        , DesignSystem.padding P4
        ]
        (Element.column
            [ Element.alignBottom, Element.width Element.fill ]
            [ Element.column
                [ Element.centerX
                , Element.onLeft <|
                    if windowSize.width |> Quantity.lessThan (Pixels.pixels 355) then
                        Element.none

                    else
                        Element.el [] logoMini
                ]
                [ Element.el [ Element.moveRight 36 ] linkBlockMobile
                , Element.el [ DesignSystem.paddingY P8, Element.centerX ] <| socialMedia 24
                ]
            , horizontalSplitter
            , Element.el [ Element.centerX, DesignSystem.paddingTop P4 ] (copyright copyrightYear)
            ]
        )


linkBlockMobile : Element msg
linkBlockMobile =
    Element.row
        []
        [ links0Mobile |> toLink |> Element.column [ DesignSystem.spacing S2, Element.width <| Element.px 90 ]
        , links1Mobile |> toLink |> Element.column [ DesignSystem.spacing S2 ]
        ]


linkBlock : Element msg
linkBlock =
    Element.row
        []
        [ toLink links0 |> Element.column [ column0Width, DesignSystem.spacing S2 ]
        , toLink links1 |> Element.column [ column1Width, DesignSystem.spacing S2 ]
        , toLink links2 |> Element.column [ column2Width, DesignSystem.spacing S2 ]
        ]


legal : Element msg
legal =
    Element.row
        [ DesignSystem.fontSize TextXs, DesignSystem.fontColor Gray500 ]
        [ Element.link [ column0Width ] { url = "https://www.insurello.se/cookies/", label = Element.text "Cookies" }
        , Element.link [ column1Width ] { url = "https://www.insurello.se/dataskydd/", label = Element.text "Dataskydd" }
        , Element.link [ column2Width ] { url = "https://www.insurello.se/villkor/", label = Element.text "Användarvillkor" }
        ]


column0Width : Element.Attribute msg
column0Width =
    Element.width <| Element.px 100


column1Width : Element.Attribute msg
column1Width =
    Element.width <| Element.px 100


column2Width : Element.Attribute msg
column2Width =
    Element.width Element.fill


links0Mobile : List ( String, String )
links0Mobile =
    links0 ++ List.take 2 links1


links1Mobile : List ( String, String )
links1Mobile =
    List.drop 2 links1 ++ links2 ++ [ ( "Juridiskt", "https://www.insurello.se/cookies/" ) ]


links0 : List ( String, String )
links0 =
    [ ( "Logga In", "https://app.insurello.se/cases/" )
    , ( "Om oss", "https://www.insurello.se/om-oss/" )
    , ( "Blogg", "https://www.insurello.se/blogg/" )
    ]


links1 : List ( String, String )
links1 =
    [ ( "Hjälp", "https://insurello.zendesk.com/" )
    , ( "Press", "https://www.insurello.se/press/" )
    , ( "Karriärer", "https://jobb.insurello.se/" )
    ]


links2 : List ( String, String )
links2 =
    [ ( "Försäkringsskolan", "https://www.insurello.se/forsakringsskolan/" )
    , ( "Kundhistorier", "https://www.insurello.se/testimonials/" )
    , ( "Kan jag få pengar?", "https://www.insurello.se/kan-jag-fa-pengar/" )
    ]


toLink : List ( String, String ) -> List (Element msg)
toLink =
    List.map (\( text, url ) -> Element.link [ Element.alignLeft ] { url = url, label = Element.text text })


socialMedia : Int -> Element msg
socialMedia height =
    [ ( "/assets/facebook.svg", "https://www.facebook.com/Insurello/", "Facebook" )
    , ( "/assets/instagram.svg", "https://www.instagram.com/insurello_sverige/", "Instagram" )
    , ( "/assets/youtube.svg", "https://www.youtube.com/channel/UCVOfYjqU-XbKmMsq3Ic3XaA", "YouTube" )
    , ( "/assets/linkedin.svg", "https://www.linkedin.com/company/insurello/", "linkedIn" )
    , ( "/assets/twitterCopy.svg", "https://twitter.com/insurello?lang=en", "Twitter" )
    ]
        |> List.map
            (\( src, url, description ) ->
                Element.newTabLink
                    []
                    { url = url
                    , label =
                        Element.image
                            [ Element.height <| Element.px height ]
                            { src = src, description = description }
                    }
            )
        |> Element.row [ DesignSystem.spacing S8 ]


copyright : Int -> Element msg
copyright year =
    Element.el
        [ DesignSystem.fontColor Blue500Insurello
        , DesignSystem.fontSize TextXs
        ]
        (Element.text ("© " ++ String.fromInt year ++ " Insurello AB"))
