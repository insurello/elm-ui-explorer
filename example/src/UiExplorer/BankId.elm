module UiExplorer.BankId exposing (..)

import App.ApiRequest
import App.BankID
import App.BankID.Grant as Grant
import App.Pages.Signup.BankId as BankId exposing (Layout(..))
import DesignSystem exposing (Padding(..), Spacing(..))
import Element exposing (Element)
import Form
import Form.Field
import Pixels
import Quantity
import Shared.SwedishPersonalNumber as PersonalNumber
import Shared.Types.WindowSize exposing (WindowSize)
import Time
import Url


init =
    BankId.init (Time.millisToPosix 0)


update : BankId.Msg -> BankId.State -> BankId.State
update msg state =
    BankId.update
        (always ())
        { os = { name = "" }, browser = { name = "" }, engine = { name = "" } }
        { protocol = Url.Https
        , host = "app.insurello.se"
        , port_ = Nothing
        , path = ""
        , query = Nothing
        , fragment = Nothing
        }
        (always ())
        msg
        state
        |> Tuple.first


grant =
    Grant.fromPersonalNumber PersonalNumber.personalNumber191212121212


view : WindowSize -> () -> Element BankId.Msg
view windowSize () =
    let
        layout =
            if windowSize.width |> Quantity.lessThan (Pixels.pixels 500) then
                MobileLayout

            else
                DesktopLayout
    in
    [ BankId.view layout BankId.defaultViewConfig init
    , BankId.view layout { showLoginPageLink = False, submitButtonText = "Logga in på Mina sidor" } init
    , BankId.view
        layout
        BankId.defaultViewConfig
        (init
            |> update (Form.Input BankId.personalNumberPath Form.Text (Form.Field.String "200011119999") |> BankId.createFormMsgForTesting)
        )
    , BankId.view
        layout
        BankId.defaultViewConfig
        (init
            |> update (Form.Input BankId.personalNumberPath Form.Text (Form.Field.String "abc") |> BankId.createFormMsgForTesting)
            |> update (Form.Submit |> BankId.createFormMsgForTesting)
        )
    , BankId.view layout BankId.defaultViewConfig { init | bankID = App.BankID.Pending grant Nothing }
    , BankId.view
        layout
        BankId.defaultViewConfig
        { init
            | bankID =
                App.BankID.Pending
                    grant
                    (Just
                        { description = Just "Skriv in din säkerhetskod i BankID-appen och välj Legitimera eller Skriv under."
                        }
                    )
        }
    , BankId.view
        layout
        BankId.defaultViewConfig
        { init | bankID = App.BankID.Failed grant (App.ApiRequest.BadUrl "https://example.com/") }
    , BankId.view
        layout
        BankId.defaultViewConfig
        { init
            | bankID =
                App.BankID.Failed grant
                    (App.ApiRequest.BadBody
                        "BankID-appen svarar inte. Kontrollera att den är startad och att du har internetanslutning. Om du inte har något giltigt BankID kan du hämta ett hos bin Bank. Försök sedan igen."
                    )
        }
    , BankId.view layout BankId.defaultViewConfig { init | bankID = App.BankID.Complete }
    ]
        |> List.map (Element.el [ Element.width Element.fill ])
        |> Element.column
            [ DesignSystem.spacing S4
            ]
