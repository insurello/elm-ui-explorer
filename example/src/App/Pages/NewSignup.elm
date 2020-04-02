module App.Pages.NewSignup exposing (Model, Msg(..), MsgConfig, bankIdCard, init, update, view)

import App.ApiRequest as ApiRequest
import App.Pages.Signup.BankId as BankId exposing (Layout(..))
import App.Pages.Signup.Footer as Footer
import App.Pages.Signup.Header as Header
import App.Pages.Signup.Process as Process
import App.Pages.Signup.Testimonials as Testimonials
import App.UserAgent exposing (UserAgent)
import Browser.Dom
import DesignSystem exposing (Border(..), BorderRadius(..), Color(..), Font(..), FontSize(..), Length(..), Padding(..), Spacing(..))
import DesignSystem.Card as Card
import DesignSystem.Views
import Ease
import Element exposing (Element)
import Element.Background
import Element.Font
import Html.Attributes
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Shared.Types.WindowSize exposing (WindowSize)
import SmoothScroll
import Task
import Time
import Url exposing (Url)


type Msg
    = BankIdMsg BankId.Msg
    | ToggleDropdown
    | PressedGetStarted
    | SmoothScrollEnded (Result Browser.Dom.Error ())


type alias Model =
    { dropdownOpen : Bool
    , bankId : BankId.State
    }


init : Time.Posix -> Model
init time =
    { dropdownOpen = False
    , bankId = BankId.init time
    }


type alias MsgConfig msg =
    { tagger : Msg -> msg
    , onAuthenticationCompleted : Result ApiRequest.Error () -> msg
    }


update : MsgConfig msg -> UserAgent -> Url -> Msg -> Model -> ( Model, Cmd msg )
update msgConfig userAgent currentUrl msg model =
    case msg of
        ToggleDropdown ->
            ( { model | dropdownOpen = not model.dropdownOpen }, Cmd.none )

        BankIdMsg bankIdMsg ->
            let
                ( bankIdModel, bankIdCmd ) =
                    BankId.update (BankIdMsg >> msgConfig.tagger) userAgent currentUrl msgConfig.onAuthenticationCompleted bankIdMsg model.bankId
            in
            ( { model | bankId = bankIdModel }, bankIdCmd )

        PressedGetStarted ->
            ( model
            , Browser.Dom.getElement bottomBankIdCardId
                |> Task.andThen
                    (\bankId ->
                        SmoothScroll.scrollTo
                            (SmoothScroll.createConfig Ease.inOutCubic 500)
                            (bankId.element.y + bankId.element.height / 2 - bankId.viewport.height / 2)
                    )
                |> Task.attempt (SmoothScrollEnded >> msgConfig.tagger)
            )

        SmoothScrollEnded _ ->
            ( model, Cmd.none )


view : WindowSize -> Model -> Element Msg
view windowSize model =
    Element.el
        (Element.width Element.fill
            :: Element.height Element.fill
            :: DesignSystem.fontColor Gray800TextBlack
            :: DesignSystem.fontSize TextBase
            :: DesignSystem.font Muli400Regular
        )
        (if windowSize.width |> Quantity.lessThan (Pixels.pixels 1000) then
            mobileView windowSize model

         else
            desktopView windowSize model
        )


desktopView : WindowSize -> Model -> Element Msg
desktopView windowSize model =
    Element.column
        [ Element.width Element.fill ]
        [ Header.view True False
        , Element.el
            [ Element.width Element.fill
            , DesignSystem.backgroundColor Blue100Cascade
            ]
            (signup model.bankId |> Element.map BankIdMsg)
        , Element.el
            [ Element.width Element.fill
            , Element.paddingEach { left = 0, right = 0, top = 64, bottom = 0 }
            ]
            (Testimonials.desktopView PressedGetStarted)
        , Element.el
            [ Element.width Element.fill
            , Element.height <| Element.px 140
            , Element.Background.image "/elm-ui-explorer/assets/backgroundWaveCascadeBlue.svg"
            ]
            Element.none
        , Element.el
            [ Element.width Element.fill
            , DesignSystem.backgroundColor Blue100Cascade
            , DesignSystem.paddingEach P0 P0 P8 P0
            ]
            Process.desktopView
        , Element.el
            [ Element.htmlAttribute <| Html.Attributes.id bottomBankIdCardId
            , DesignSystem.backgroundColor Blue100Cascade
            , Element.width Element.fill
            , DesignSystem.paddingXY P2 P12
            ]
            (bankIdCard model)
        , Footer.view windowSize (Time.toYear Time.utc model.bankId.now)
        ]


mobileView : WindowSize -> Model -> Element Msg
mobileView windowSize model =
    if model.dropdownOpen then
        Header.dropdownOverlay ToggleDropdown True False

    else
        Element.column
            [ Element.width Element.fill ]
            [ Header.mobileView ToggleDropdown
            , Element.el
                [ Element.width Element.fill
                , Element.Background.color <| DesignSystem.color Blue100Cascade
                , DesignSystem.paddingXY P2 P8
                ]
                (signupMobile model.bankId |> Element.map BankIdMsg)
            , Element.el
                [ DesignSystem.paddingEach P8 P2 P0 P2
                , Element.width Element.fill
                ]
                (Testimonials.mobileView PressedGetStarted)
            , Element.el [ Element.height <| Element.px 32 ] Element.none
            , Process.mobileView Process.BlueBackground
            , Element.el
                [ Element.htmlAttribute <| Html.Attributes.id bottomBankIdCardId
                , DesignSystem.backgroundColor Blue100Cascade
                , Element.width Element.fill
                , DesignSystem.paddingXY P2 P12
                ]
                (bankIdCardMobile model)
            , Footer.view windowSize (Time.toYear Time.utc model.bankId.now)
            ]


signupContent : Element msg
signupContent =
    Element.column
        [ Element.spacing 48 ]
        [ signInToBegin
        , theFirstStep
        ]


signupMobile : BankId.State -> Element BankId.Msg
signupMobile state =
    Element.column
        [ Element.width Element.fill, Element.spacing 32 ]
        [ signInToBeginMobile
        , BankId.view MobileLayout BankId.defaultViewConfig state
        , theFirstStepMobile
        ]


signup : BankId.State -> Element BankId.Msg
signup state =
    Element.row
        [ Element.paddingXY 0 64, Element.width Element.fill ]
        [ Element.el [ Element.width Element.fill ] Element.none
        , signupContent
        , Element.el [ Element.width <| Element.maximum 76 Element.fill ] Element.none
        , Element.el
            [ Element.width <| Element.px 489 ]
            (BankId.view DesktopLayout BankId.defaultViewConfig state)
        , Element.el [ Element.width Element.fill ] Element.none
        ]


signInToBeginMobile : Element msg
signInToBeginMobile =
    Element.paragraph
        (DesignSystem.fontSize Text3Xl
            :: Element.centerX
            :: Element.Font.center
            :: Element.width (Element.px 300)
            :: DesignSystem.font Merriweather700Bold
        )
        [ titleText ]


signInToBegin : Element msg
signInToBegin =
    Element.paragraph
        (Element.spacing 16
            :: Element.Font.size 50
            :: Element.width (Element.px 429)
            :: DesignSystem.font Merriweather400Regular
        )
        [ titleText ]


titleText =
    Element.text "Logga in för att komma igång"


theFirstStepText : Element msg
theFirstStepText =
    Element.text "Efter du har loggat in behöver du skapa ett ärende genom att fylla i ett formulär. Det tar ca 15 min. När vi fått information om skadan tar vi över och driver ärendet för att maximera din ersättning. Ofta hittar vi pengar du inte visste du har rätt till."


claimCount : Element msg
claimCount =
    Element.text "Insurello har hanterat fler än 60 000 ärenden!"


theFirstStep : Element msg
theFirstStep =
    Element.column
        []
        [ Element.paragraph
            [ DesignSystem.spacing S2
            , Element.width <| Element.px 388
            ]
            [ theFirstStepText ]
        , horizontalSplitter
        , Element.el (DesignSystem.font Muli700Bold) claimCount
        ]


theFirstStepMobile : Element msg
theFirstStepMobile =
    Element.column
        [ Element.width Element.fill ]
        [ Element.paragraph
            [ DesignSystem.spacing S2
            , Element.width <| Element.maximum 343 Element.fill
            , Element.centerX
            , Element.Font.center
            ]
            [ theFirstStepText ]
        , horizontalSplitterMobile
        , Element.paragraph (Element.centerX :: Element.Font.center :: DesignSystem.font Muli700Bold) [ claimCount ]
        ]


horizontalSplitter : Element msg
horizontalSplitter =
    Element.el
        [ Element.Background.color (DesignSystem.color Gold500)
        , Element.height <| Element.px 2
        , Element.width <| Element.px 287
        ]
        Element.none
        |> Element.el [ Element.paddingEach { left = 0, right = 0, top = 20, bottom = 16 } ]


horizontalSplitterMobile : Element msg
horizontalSplitterMobile =
    Element.el
        [ Element.Background.color (DesignSystem.color Gold500)
        , Element.height <| Element.px 2
        , Element.width <| Element.px 104
        ]
        Element.none
        |> Element.el [ Element.paddingEach { left = 0, right = 0, top = 32, bottom = 16 }, Element.centerX ]


readyToStartText : Element msg
readyToStartText =
    Element.el (DesignSystem.fontColor Gold500 :: DesignSystem.font Muli700Bold) (Element.text "REDO ATT BÖRJA?")


bankIdCard : Model -> Element Msg
bankIdCard model =
    Element.el
        (Card.cardAttributes
            ++ [ Element.centerX
               , Element.width (Element.maximum 1100 Element.fill)
               , Element.height (Element.px 487)
               , DesignSystem.borderRadius NotRounded
               ]
        )
        (Element.row
            [ Element.width Element.fill
            , Element.centerY
            ]
            [ DesignSystem.Views.horizontalFiller
            , Element.column [ DesignSystem.spacing S4 ]
                [ readyToStartText
                , Element.column [ DesignSystem.spacing S8 ]
                    [ Element.paragraph
                        (DesignSystem.spacing S2
                            :: DesignSystem.fontSize Text3Xl
                            :: Element.width (Element.px 260)
                            :: DesignSystem.font Merriweather700Bold
                        )
                        [ titleText ]
                    , Element.column
                        [ DesignSystem.spacing S4 ]
                        [ Element.paragraph
                            [ DesignSystem.spacing S2
                            , Element.width <| Element.px 388
                            ]
                            [ theFirstStepText ]
                        , horizontalSplitter
                        , Element.el [ Element.moveLeft 20, Element.moveUp 8 ] Testimonials.trustpilot
                        ]
                    ]
                ]
            , DesignSystem.Views.horizontalFiller
            , DesignSystem.Views.horizontalFiller
            , BankId.view DesktopLayout BankId.defaultViewConfig model.bankId
                |> Element.el [ Element.alignTop ]
                |> Element.map BankIdMsg
            , DesignSystem.Views.horizontalFiller
            ]
        )


bottomBankIdCardId =
    "bottom-bank-id-card"


bankIdCardMobile : Model -> Element Msg
bankIdCardMobile model =
    Element.column
        (Card.cardAttributes
            ++ [ Element.centerX
               , Element.width (Element.maximum 343 Element.fill)
               , DesignSystem.borderRadius NotRounded
               , DesignSystem.spacing S8
               , DesignSystem.paddingXY P0 P8
               ]
        )
        [ Element.column
            [ DesignSystem.spacing S4, Element.centerX ]
            [ Element.el [ Element.centerX ] readyToStartText
            , Element.paragraph
                (DesignSystem.spacing S2
                    :: DesignSystem.fontSize Text3Xl
                    :: Element.Font.center
                    :: Element.centerX
                    :: Element.width (Element.px 260)
                    :: DesignSystem.font Merriweather700Bold
                )
                [ titleText ]
            ]
        , BankId.viewContent BankId.defaultViewConfig model.bankId
            |> Maybe.withDefault Element.none
            |> Element.el [ Element.centerX ]
            |> Element.map BankIdMsg
        , Element.el [ Element.centerX ] Testimonials.trustpilot
        ]
