module DesignSystem.Views exposing (horizontalFiller)

import Element exposing (Element)


horizontalFiller : Element msg
horizontalFiller =
    Element.el [ Element.width Element.fill ] Element.none
