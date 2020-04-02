module App.Pages.Signup.BankId exposing
    ( Layout(..)
    , Msg
    , State
    , ViewConfig
    , createFormMsgForTesting
    , defaultViewConfig
    , init
    , personalNumberPath
    , update
    , view
    , viewContent
    )

import App.ApiRequest as ApiRequest exposing (BadStatusPayload(..), Error(..))
import App.BankID
import App.BankID.GrantStatus
import App.UserAgent exposing (UserAgent)
import DesignSystem exposing (Color(..), Font(..), FontSize(..), Padding(..), Spacing(..))
import DesignSystem.Card as Card
import DesignSystem.Input
import Element exposing (Element)
import Element.Font
import Element.Input
import Form exposing (Form)
import Form.Field
import Form.Validate
import Html
import Html.Attributes
import Html.Insurello.Form.Error
import Html.Insurello.Form.Validation
import Shared.SwedishPersonalNumber exposing (PersonalNumber)
import Time exposing (Posix)
import Url exposing (Url)


type alias State =
    { now : Posix
    , form : PersonalNumberForm
    , bankID : App.BankID.Model
    }


type alias PersonalNumberForm =
    Form Html.Insurello.Form.Error.ValidationError PersonalNumber


type Msg
    = FormMsg Form.Msg
    | BankIDMsg App.BankID.Msg


createFormMsgForTesting : Form.Msg -> Msg
createFormMsgForTesting =
    FormMsg


personalNumberPath : String
personalNumberPath =
    "personal-number"


init : Time.Posix -> State
init now =
    { now = now
    , form = Form.initial [] (minAge18 now)
    , bankID = App.BankID.init
    }


update : (Msg -> msg) -> UserAgent -> Url -> (Result ApiRequest.Error () -> msg) -> Msg -> State -> ( State, Cmd msg )
update tagger userAgent currentUrl onAuthenticationCompleted msg state =
    case msg of
        FormMsg formMsg ->
            let
                validation =
                    minAge18 state.now

                form =
                    Form.update validation formMsg state.form
            in
            case formMsg of
                Form.Submit ->
                    case Form.getOutput form of
                        Just personalNumber ->
                            let
                                ( nextBankID, cmd ) =
                                    App.BankID.start userAgent currentUrl personalNumber
                            in
                            ( { state | form = form, bankID = nextBankID }
                            , Cmd.map (tagger << BankIDMsg) cmd
                            )

                        Nothing ->
                            ( { state | form = form }
                            , Cmd.none
                            )

                _ ->
                    ( { state | form = form }
                    , Cmd.none
                    )

        BankIDMsg bankIDMsg ->
            let
                ( nextBankID, cmd ) =
                    App.BankID.update bankIDMsg state.bankID
            in
            case nextBankID of
                App.BankID.Complete ->
                    ( state
                    , Cmd.none
                    )

                _ ->
                    ( { state | bankID = nextBankID }
                    , Cmd.map (tagger << BankIDMsg) cmd
                    )


minAge18 : Time.Posix -> Form.Validate.Validation Html.Insurello.Form.Error.ValidationError PersonalNumber
minAge18 now =
    Form.Validate.field personalNumberPath
        (Html.Insurello.Form.Validation.personalNumber
            |> Form.Validate.andThen (Html.Insurello.Form.Validation.minAge 18 now)
        )


baseCard =
    Element.el (Element.width Element.fill :: Element.centerY :: DesignSystem.font Muli400Regular)
        >> Element.el
            (Card.cardAttributes
                ++ [ Element.width <| Element.px 483
                   , Element.height <| Element.px 268
                   ]
            )


baseCardMobile =
    Element.el (Element.centerX :: Element.centerY :: DesignSystem.font Muli400Regular)
        >> Element.el
            (Card.cardAttributes
                ++ [ Element.width <| Element.maximum 343 Element.fill
                   , Element.height <| Element.minimum 216 Element.fill
                   , Element.centerX
                   ]
            )


viewContent : ViewConfig -> State -> Maybe (Element Msg)
viewContent { showLoginPageLink, submitButtonText } state =
    let
        cancelButton =
            Element.Input.button
                [ DesignSystem.fontColor Blue500Insurello
                , Element.centerX
                , Element.mouseOver [ DesignSystem.fontColor Blue600Pacific ]
                ]
                { onPress = BankIDMsg App.BankID.Cancel |> Just, label = Element.text "Avbryt" }

        tryAgainButton =
            Element.Input.button
                [ DesignSystem.fontColor Blue500Insurello
                , Element.centerX
                , Element.mouseOver [ DesignSystem.fontColor Blue600Pacific ]
                ]
                { onPress = BankIDMsg App.BankID.Retry |> Just, label = Element.text "Försök igen" }

        pendingCard : Maybe App.BankID.GrantStatus.Session -> Element Msg
        pendingCard maybeSession =
            Element.column
                [ Element.width Element.fill, DesignSystem.spacing S4 ]
                [ (case maybeSession of
                    Just session ->
                        [ session.description
                            |> Maybe.withDefault "Väntar på svar från BankID"
                            |> Element.text
                        ]

                    Nothing ->
                        [ Element.text "Väntar på svar från BankID" ]
                  )
                    |> Element.paragraph
                        (DesignSystem.paddingX P8
                            :: Element.centerX
                            :: Element.Font.center
                            :: Element.Font.size 18
                            :: DesignSystem.font Muli700Bold
                        )
                , Html.span [ Html.Attributes.class "spinner" ] [] |> Element.html |> Element.el [ Element.centerX ]
                , maybeSession
                    |> Maybe.map (always cancelButton)
                    |> Maybe.withDefault Element.none
                ]

        errorCard error =
            Element.column
                [ DesignSystem.spacing S4, Element.centerX ]
                [ Element.paragraph
                    (DesignSystem.paddingX P8
                        :: Element.centerX
                        :: Element.Font.center
                        :: DesignSystem.fontSize TextXl
                        :: DesignSystem.font Muli700Bold
                    )
                    [ Element.text
                        (case error of
                            BadStatus _ (StringPayload errorMessage) ->
                                errorMessage

                            BadStatus _ (JsonPayload errorPayload) ->
                                errorPayload.errorDescription

                            BadUrl url ->
                                "Felaktig adress: " ++ url

                            Timeout ->
                                "Förfrågan tog för lång tid"

                            NetworkError ->
                                "Nätverksfel"

                            BadBody decodeErrorMessage ->
                                decodeErrorMessage
                        )
                    ]
                , Element.row [ DesignSystem.spacing S8, Element.centerX ] [ cancelButton, tryAgainButton ]
                ]

        signupCard =
            Element.column
                [ Element.spacing 32, Element.width Element.fill ]
                [ Element.column
                    [ Element.spacing 16, Element.width <| Element.maximum 383 Element.fill, Element.centerX ]
                    [ personNumberField
                    , DesignSystem.Input.button
                        (DesignSystem.Input.buttonEnableAttributes Blue500Insurello
                            ++ [ Element.width Element.fill
                               , Element.height (Element.px 55)
                               , DesignSystem.padding P0
                               ]
                        )
                        { onPress = FormMsg Form.Submit
                        , label =
                            Element.el
                                (Element.centerX :: DesignSystem.font Muli700Bold)
                                (Element.text submitButtonText)
                        }
                    ]
                , footerView
                ]

        personNumberField =
            let
                maybeError =
                    Form.getFieldAsString personalNumberPath state.form |> .liveError

                borderColor =
                    case maybeError of
                        Just _ ->
                            [ DesignSystem.borderColor DesignSystem.Red500Alert ]

                        Nothing ->
                            []
            in
            Element.column
                [ Element.width Element.fill ]
                [ Element.Input.text
                    (Element.width Element.fill
                        :: Element.height (Element.px 55)
                        :: Element.paddingEach { left = 72, top = 18, bottom = 18, right = 12 }
                        :: Element.inFront
                            (Element.image
                                [ Element.width <| Element.px 39
                                , Element.moveRight 14
                                , Element.centerY
                                ]
                                { src = "/assets/bankIdLogo.svg", description = "" }
                            )
                        :: DesignSystem.fontColor Gray800TextBlack
                        :: DesignSystem.Input.onEnter (FormMsg Form.Submit)
                        :: DesignSystem.font Muli400Regular
                        ++ borderColor
                    )
                    { onChange = Form.Field.String >> Form.Input personalNumberPath Form.Text >> FormMsg
                    , text = Form.getFieldAsString personalNumberPath state.form |> .value |> Maybe.withDefault ""
                    , placeholder =
                        Element.text "ååååmmddxxxx"
                            |> Element.Input.placeholder [ Element.height Element.fill, DesignSystem.fontColor Gray500 ]
                            |> Just
                    , label =
                        case maybeError of
                            Just _ ->
                                Element.Input.labelAbove
                                    [ DesignSystem.fontColor Red500Alert ]
                                    (Element.text "Ditt personnummer")

                            Nothing ->
                                Element.Input.labelHidden "Ditt personnummer"
                    }
                , case maybeError of
                    Just error ->
                        Html.Insurello.Form.Error.errorString error
                            |> Element.text
                            |> Element.el
                                [ DesignSystem.fontColor Red500Alert
                                , DesignSystem.fontSize TextSm
                                , DesignSystem.paddingTop P2
                                ]

                    Nothing ->
                        Element.none
                ]

        footerView =
            Element.wrappedRow
                [ DesignSystem.spacing S2
                , DesignSystem.fontSize TextXs
                , DesignSystem.fontColor Gray600Dark
                , Element.centerX
                , DesignSystem.paddingBottom P3
                ]
                [ if showLoginPageLink then
                    Element.link [] { url = "https://www.insurello.se/cases", label = Element.text "Har du en anmälan?" }

                  else
                    Element.none
                , Element.link [] { url = "https://www.insurello.se/villkor", label = Element.text "Användarvillkor" }
                , Element.link [] { url = "https://www.insurello.se/dataskydd", label = Element.text "Dataskydd" }
                ]
    in
    case state.bankID of
        App.BankID.Start _ ->
            Just signupCard

        App.BankID.Pending _ session ->
            Just <| pendingCard session

        App.BankID.Failed _ err ->
            Just <| errorCard err

        App.BankID.Complete ->
            Nothing


type Layout
    = MobileLayout
    | DesktopLayout


type alias ViewConfig =
    { showLoginPageLink : Bool
    , submitButtonText : String
    }


defaultViewConfig : ViewConfig
defaultViewConfig =
    { showLoginPageLink = True, submitButtonText = "Anmäl din skada med BankID" }


view : Layout -> ViewConfig -> State -> Element Msg
view layout config state =
    let
        baseCard_ =
            case layout of
                MobileLayout ->
                    baseCardMobile

                DesktopLayout ->
                    baseCard
    in
    viewContent config state |> Maybe.map baseCard_ |> Maybe.withDefault Element.none
