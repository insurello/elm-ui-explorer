module App.Pages.NewLogin exposing (Model, Msg(..), MsgConfig, init, update, view)

import App.ApiRequest as ApiRequest
import App.Pages.Signup.BankId as BankId
import App.Pages.Signup.Footer as Footer
import App.Pages.Signup.Header as Header
import App.UserAgent exposing (UserAgent)
import DesignSystem exposing (Color(..), Font(..), FontSize(..), Padding(..), Spacing(..))
import Element exposing (Element)
import Element.Font
import Html.Attributes
import Shared.Types.WindowSize exposing (WindowSize)
import Time
import Url exposing (Url)


type Msg
    = BankIdMsg BankId.Msg
    | ToggleDropdown


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


view : WindowSize -> Model -> Element Msg
view windowSize model =
    Element.el
        (Element.width Element.fill
            :: Element.height Element.fill
            :: DesignSystem.fontColor Gray800TextBlack
            :: DesignSystem.fontSize TextBase
            :: DesignSystem.font Muli400Regular
        )
        (if windowSize.width < 1000 then
            mobileView windowSize model

         else
            desktopView windowSize model
        )


desktopView : WindowSize -> Model -> Element Msg
desktopView windowSize model =
    Element.column
        [ Element.width Element.fill, Element.height Element.fill ]
        [ Header.view False True
        , content model.bankId
        , Footer.viewWithoutPress windowSize (Time.toYear Time.utc model.bankId.now)
        ]


bankIdViewConfig : BankId.ViewConfig
bankIdViewConfig =
    { showLoginPageLink = False, submitButtonText = "Logga in på Mina sidor" }


content : BankId.State -> Element Msg
content bankId =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , DesignSystem.backgroundColor Blue100Cascade
        ]
        [ Element.row
            [ DesignSystem.paddingY P16
            , Element.width Element.fill
            ]
            [ Element.el [ Element.width (Element.fillPortion 1) ] Element.none
            , Element.column
                [ Element.width (Element.px 400)
                , Element.spacing 40
                ]
                [ Element.paragraph
                    (DesignSystem.fontSize Text6Xl
                        :: Element.spacing 16
                        :: DesignSystem.font Merriweather400Regular
                    )
                    [ titleText ]
                , Element.paragraph [ DesignSystem.spacing S2 ] [ descriptionText ]
                ]
            , Element.el [ Element.width <| Element.maximum 100 (Element.fillPortion 1) ] Element.none
            , BankId.view BankId.DesktopLayout bankIdViewConfig bankId
                |> Element.map BankIdMsg
                |> Element.el [ Element.alignRight ]
            , Element.el [ Element.width (Element.fillPortion 1) ] Element.none
            ]
        , desktopBackground
        ]


titleText : Element msg
titleText =
    Element.text "Följ dina ärenden"


descriptionText : Element msg
descriptionText =
    Element.text "Logga in med ditt BankID för att följa vad som händer i dina ärenden, svara på eventuella följdfrågor, eller skapa nya ärenden."


mobileView : WindowSize -> Model -> Element Msg
mobileView windowSize model =
    if model.dropdownOpen then
        Header.dropdownOverlay ToggleDropdown False True

    else
        Element.column
            [ Element.width Element.fill, Element.height Element.fill ]
            [ Header.mobileView ToggleDropdown
            , mobileContent windowSize model.bankId
            , Footer.viewWithoutPress windowSize (Time.toYear Time.utc model.bankId.now)
            ]


mobileContent : WindowSize -> BankId.State -> Element Msg
mobileContent windowSize bankId =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , DesignSystem.backgroundColor Blue100Cascade
        , DesignSystem.paddingTop P8
        ]
        [ Element.column
            [ Element.width Element.fill, DesignSystem.spacing S8, DesignSystem.paddingX P2 ]
            [ Element.paragraph
                (Element.centerX
                    :: Element.Font.center
                    :: DesignSystem.fontSize Text3Xl
                    :: DesignSystem.font Merriweather700Bold
                )
                [ titleText ]
            , BankId.view BankId.MobileLayout bankIdViewConfig bankId
                |> Element.map BankIdMsg
                |> Element.el [ Element.width Element.fill ]
            , Element.paragraph
                [ Element.centerX
                , Element.Font.center
                , DesignSystem.spacing S2
                , Element.width <| Element.maximum 343 Element.fill
                ]
                [ descriptionText ]
            ]
        , if windowSize.width < 600 then
            Element.image
                [ Element.htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                , Element.width Element.fill
                , Element.alignBottom
                ]
                { src = "/elm-ui-explorer/assets/illustrationMonsterBikeAccidentMobile.svg"
                , description = ""
                }

          else
            desktopBackground
        ]


desktopBackground : Element msg
desktopBackground =
    Element.el
        [ Element.clip
        , Element.behindContent <|
            Element.image
                [ Element.centerX
                , Element.width <| Element.px 1440
                , Element.htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                ]
                { src = "/elm-ui-explorer/assets/illustrationMonsterBikeAccident.svg", description = "" }
        , Element.width <| Element.fill
        , Element.height <| Element.px 300
        , Element.alignBottom
        ]
        Element.none
