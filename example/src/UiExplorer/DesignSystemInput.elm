module UiExplorer.DesignSystemInput exposing (..)

import DesignSystem exposing (Color(..), FontSize(..), Padding(..), Spacing(..))
import DesignSystem.Input exposing (ButtonState(..))
import Element


buttonState : List ( String, ButtonState () )
buttonState =
    [ ( "Enabled", Enabled () )
    , ( "EnabledWithoutMsg", EnabledWithoutMsg )
    , ( "Disabled", Disabled )
    , ( "FakeDisabled", FakeDisabled () )
    ]


view : Element.Element ()
view =
    Element.column
        [ DesignSystem.spacing S8, DesignSystem.padding P8 ]
        [ buttonState
            |> List.map (\( name, buttonState_ ) -> DesignSystem.Input.primaryButton buttonState_ (Element.text name))
            |> Element.row [ DesignSystem.spacing S4, Element.above <| Element.el [ Element.moveUp 4, DesignSystem.fontSize TextSm ] (Element.text "primaryButton") ]
        , buttonState
            |> List.map (\( name, buttonState_ ) -> DesignSystem.Input.submitButton buttonState_ (Element.text name))
            |> Element.row [ DesignSystem.spacing S4, Element.above <| Element.el [ Element.moveUp 4, DesignSystem.fontSize TextSm ] (Element.text "submitButton") ]
        ]
