module DesignSystem.Card exposing (cardAttributes)

import DesignSystem exposing (Border(..), BorderRadius(..), Color(..), Font(..), FontSize(..), Padding(..))
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font


cardAttributes : List (Attribute msg)
cardAttributes =
    [ DesignSystem.paddingTop P5
    , DesignSystem.borderRadius Rounded
    , DesignSystem.borderColor BootstrapCardBorder
    , Element.Border.shadow { offset = ( 0, 5 ), size = 2, blur = 14, color = Element.rgba 0 0 0 0.1 }
    , DesignSystem.border B1
    , Element.Background.color <| DesignSystem.color BootstrapWhite
    , Element.width Element.fill
    , DesignSystem.fontSize TextBase
    , Element.Font.color <| DesignSystem.color DesignSystem.Gray800TextBlack
    ]
