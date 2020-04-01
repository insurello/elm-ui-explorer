module Main exposing (..)

import App.Pages.NewLogin as NewLogin
import App.Pages.NewSignup as NewSignup
import App.Pages.Signup.Footer as Footer
import DesignSystem exposing (Color(..), Font(..), FontSize(..), Padding(..), Spacing(..))
import Element
import Json.Decode
import Pixels
import Time
import UiExplorer
import UiExplorer.BankId
import UiExplorer.DesignSystemInput as DesignSystemInput
import UiExplorer.SignupAndLogin as SignupAndLogin


nextPageWithWidth :
    String
    -> UiExplorer.Page model msg flags
    -> Int
    -> UiExplorer.PageBuilder modelPrevious msgPrevious flags
    -> UiExplorer.PageBuilder ( modelPrevious, model ) (UiExplorer.PageMsg msgPrevious msg) flags
nextPageWithWidth id page pageWidth =
    let
        idSuffix =
            id ++ " (width " ++ String.fromInt pageWidth ++ ")"
    in
    UiExplorer.nextPage
        idSuffix
        { page
            | view =
                \pageSize model ->
                    Element.el
                        [ Element.width <| Element.px pageWidth
                        , Element.height Element.fill
                        , Element.scrollbars
                        ]
                        (page.view
                            { pageSize | width = Pixels.pixels pageWidth }
                            model
                        )
        }


newSignupPage =
    { init = always ( NewSignup.init (Time.millisToPosix 1585124286000), Cmd.none )
    , update = SignupAndLogin.update
    , view = SignupAndLogin.signupView
    , subscriptions = \_ -> Sub.none
    }


newSignup : String
newSignup =
    "Signup"


newLoginPage =
    { init = always ( NewLogin.init (Time.millisToPosix 1585124286000), Cmd.none )
    , update = SignupAndLogin.update
    , view = SignupAndLogin.loginView
    , subscriptions = \_ -> Sub.none
    }


newLogin : String
newLogin =
    "Login"


footerView pageSize () =
    Element.column
        [ DesignSystem.spacing S8, Element.width Element.fill, DesignSystem.paddingXY P0 P2 ]
        [ Element.column
            [ DesignSystem.spacing S1, Element.width Element.fill ]
            [ Element.text "With press logos", Footer.view pageSize 2020 ]
        , Element.column
            [ DesignSystem.spacing S1, Element.width Element.fill ]
            [ Element.text "Without press logos", Footer.viewWithoutPress pageSize 2020 ]
        ]


footer : String
footer =
    "Footer"


pages =
    UiExplorer.firstPage newSignup newSignupPage
        |> nextPageWithWidth newSignup newSignupPage 320
        |> nextPageWithWidth newSignup newSignupPage 1000
        |> UiExplorer.nextPage newLogin newLoginPage
        |> nextPageWithWidth newLogin newLoginPage 320
        |> nextPageWithWidth newLogin newLoginPage 1000
        |> UiExplorer.nextPage footer (UiExplorer.static footerView)
        |> nextPageWithWidth footer (UiExplorer.static footerView) 320
        |> nextPageWithWidth footer (UiExplorer.static footerView) 1000
        |> UiExplorer.nextPage "BankID" (UiExplorer.static UiExplorer.BankId.view)
        |> nextPageWithWidth "BankID" (UiExplorer.static UiExplorer.BankId.view) 320
        |> UiExplorer.nextPage "DesignSystem.Input" (UiExplorer.static (\_ _ -> DesignSystemInput.view))


main =
    UiExplorer.application
        { flagsDecoder = Json.Decode.succeed ()
        , layoutOptions = []
        , layoutAttributes =
            DesignSystem.fontSize TextBase
                :: DesignSystem.fontColor Gray800TextBlack
                :: DesignSystem.font Roboto400Regular
        }
        pages
