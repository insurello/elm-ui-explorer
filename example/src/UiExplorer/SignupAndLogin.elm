module UiExplorer.SignupAndLogin exposing (Msg, loginView, signupView, update)

import App.ApiRequest as ApiRequest
import App.Pages.NewLogin as NewLogin
import App.Pages.NewSignup as NewSignup
import Element exposing (Element)
import Shared.Types.WindowSize exposing (WindowSize)
import Url exposing (Protocol(..), Url)


signupView : WindowSize -> NewSignup.Model -> Element.Element Msg
signupView windowSize model =
    NewSignup.view windowSize model |> Element.map SignupMsg


loginView : WindowSize -> NewSignup.Model -> Element Msg
loginView windowSize model =
    NewLogin.view windowSize model |> Element.map LoginMsg


type Msg
    = SignupMsg NewSignup.Msg
    | LoginMsg NewLogin.Msg
    | AuthenticationCompleted (Result ApiRequest.Error ())


url : Url
url =
    { protocol = Http
    , host = ""
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


update : Msg -> NewSignup.Model -> ( NewSignup.Model, Cmd Msg )
update msg model =
    case msg of
        SignupMsg signupMsg ->
            NewSignup.update
                { tagger = SignupMsg, onAuthenticationCompleted = AuthenticationCompleted }
                { os = { name = "" }, browser = { name = "" }, engine = { name = "" } }
                url
                signupMsg
                model

        LoginMsg loginMsg ->
            NewLogin.update
                { tagger = LoginMsg, onAuthenticationCompleted = AuthenticationCompleted }
                { os = { name = "" }, browser = { name = "" }, engine = { name = "" } }
                url
                loginMsg
                model

        AuthenticationCompleted _ ->
            ( model, Cmd.none )
