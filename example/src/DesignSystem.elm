module DesignSystem exposing
    ( Border(..)
    , BorderRadius(..)
    , Color(..)
    , Font(..)
    , FontSize(..)
    , Length(..)
    , Padding(..)
    , Spacing(..)
    , backgroundColor
    , border
    , borderBottom
    , borderColor
    , borderRadius
    , color
    , font
    , fontColor
    , fontSize
    , height
    , intFromLength
    , padding
    , paddingBottom
    , paddingEach
    , paddingLeft
    , paddingRight
    , paddingTop
    , paddingX
    , paddingXY
    , paddingY
    , spacing
    , width
    )

import Element exposing (Color, Element)
import Element.Background
import Element.Border
import Element.Font


type Font
    = Roboto400Regular
    | Roboto500Medium
    | Merriweather400Regular
    | Merriweather700Bold
    | Muli400Regular
    | Muli700Bold


font : Font -> List (Element.Attribute msg)
font typefaceAndWeight =
    let
        roboto =
            Element.Font.family
                [ Element.Font.typeface "Roboto"
                , Element.Font.sansSerif
                ]

        merriweather =
            Element.Font.family
                [ Element.Font.typeface "Merriweather"
                , Element.Font.serif
                ]

        muli =
            Element.Font.family
                [ Element.Font.typeface "Muli"
                , Element.Font.sansSerif
                ]
    in
    case typefaceAndWeight of
        Roboto400Regular ->
            [ roboto, Element.Font.regular ]

        Roboto500Medium ->
            [ roboto, Element.Font.medium ]

        Merriweather400Regular ->
            [ merriweather, Element.Font.regular ]

        Merriweather700Bold ->
            [ merriweather, Element.Font.bold ]

        Muli400Regular ->
            [ muli, Element.Font.regular ]

        Muli700Bold ->
            [ muli, Element.Font.bold ]


type FontSize
    = TextXs
    | TextSm
    | TextBase
    | TextXl
    | Text2Xl
    | Text3Xl
    | Text6Xl


fontSize : FontSize -> Element.Attribute msg
fontSize size =
    Element.Font.size (intFromFontSize size)


intFromFontSize : FontSize -> Int
intFromFontSize size =
    case size of
        TextXs ->
            12

        TextSm ->
            14

        TextBase ->
            16

        TextXl ->
            20

        Text2Xl ->
            28

        Text3Xl ->
            32

        Text6Xl ->
            64


type Color
    = BootstrapMutedText
    | BootstrapBorder
    | BootstrapDisabled
    | BootstrapInputBorder
    | BootstrapAlertWarning
    | BootstrapCardBorder
    | BootstrapGrey
    | BootstrapWhite
    | Boxshadow
    | Outline
    | Red300AlertSuperLight
    | Red400AlertLight
    | Red500Alert
    | Red600AlertDark
    | Green300GogoSuperLight
    | Green400GogoLight
    | Green500Gogo
    | Blue100Cascade
    | Blue200Mist
    | Blue300Arctic
    | Blue400Sky
    | Blue500Insurello
    | Blue600Pacific
    | Blue700Midnight
    | Blue800Abyss
    | Saphire300SuperLight
    | Saphire400Light
    | Saphire500
    | Saphire600Dark
    | Gold300SuperLight
    | Gold400Light
    | Gold500
    | Gold600Dark
    | White
    | Gray200White
    | Gray300SuperLight
    | Gray400Light
    | Gray500
    | Gray600Dark
    | Gray700SuperDark
    | Gray800TextBlack
    | Gray900InsurelloBlack
    | Black


color : Color -> Element.Color
color theColor =
    case theColor of
        BootstrapMutedText ->
            Element.rgb255 108 117 125

        BootstrapBorder ->
            Element.rgb255 222 226 230

        BootstrapDisabled ->
            Element.rgb255 233 236 239

        BootstrapInputBorder ->
            Element.rgb255 206 212 218

        BootstrapAlertWarning ->
            Element.rgb255 243 234 215

        BootstrapCardBorder ->
            Element.rgba255 0 0 0 0.125

        BootstrapGrey ->
            Element.rgb255 248 249 250

        BootstrapWhite ->
            Element.rgb255 255 255 255

        Boxshadow ->
            Element.rgba255 0 0 0 0.1

        Outline ->
            Element.rgb255 77 144 254

        Red300AlertSuperLight ->
            Element.rgb255 250 237 236

        Red400AlertLight ->
            Element.rgb255 230 165 160

        Red500Alert ->
            Element.rgb255 206 75 66

        Red600AlertDark ->
            Element.rgb255 166 22 17

        Green300GogoSuperLight ->
            Element.rgb255 235 244 243

        Green400GogoLight ->
            Element.rgb255 155 203 195

        Green500Gogo ->
            Element.rgb255 56 151 135

        Blue100Cascade ->
            Element.rgb255 234 241 243

        Blue200Mist ->
            Element.rgb255 211 228 237

        Blue300Arctic ->
            Element.rgb255 176 208 225

        Blue400Sky ->
            Element.rgb255 126 178 209

        Blue500Insurello ->
            Element.rgb255 59 139 186

        Blue600Pacific ->
            Element.rgb255 27 92 130

        Blue700Midnight ->
            Element.rgb255 15 64 93

        Blue800Abyss ->
            Element.rgb255 27 41 60

        Saphire300SuperLight ->
            Element.rgb255 236 239 244

        Saphire400Light ->
            Element.rgb255 162 176 204

        Saphire500 ->
            Element.rgb255 69 98 154

        Saphire600Dark ->
            Element.rgb255 35 58 114

        Gold300SuperLight ->
            Element.rgb255 249 244 235

        Gold400Light ->
            Element.rgb255 226 201 156

        Gold500 ->
            Element.rgb255 197 148 57

        Gold600Dark ->
            Element.rgb255 161 100 23

        White ->
            Element.rgb255 255 255 255

        Gray200White ->
            Element.rgb255 246 249 253

        Gray300SuperLight ->
            Element.rgb255 228 234 241

        Gray400Light ->
            Element.rgb255 206 215 225

        Gray500 ->
            Element.rgb255 170 182 196

        Gray600Dark ->
            Element.rgb255 135 148 162

        Gray700SuperDark ->
            Element.rgb255 105 115 130

        Gray800TextBlack ->
            Element.rgb255 56 60 67

        Gray900InsurelloBlack ->
            Element.rgb255 35 40 40

        Black ->
            Element.rgb255 0 0 0


fontColor : Color -> Element.Attr decorative msg
fontColor =
    color >> Element.Font.color


backgroundColor : Color -> Element.Attr decorative msg
backgroundColor =
    color >> Element.Background.color


type Padding
    = P0
    | P1
    | P2
    | P3
    | P4
    | P5
    | P8
    | P12
    | P16


type Spacing
    = S1
    | S2
    | S4
    | S8


intFromPadding : Padding -> Int
intFromPadding p =
    case p of
        P0 ->
            0

        P1 ->
            4

        P2 ->
            8

        P3 ->
            12

        P4 ->
            16

        P5 ->
            20

        P8 ->
            32

        P12 ->
            48

        P16 ->
            64


intFromSpacing : Spacing -> Int
intFromSpacing s =
    case s of
        S1 ->
            4

        S2 ->
            8

        S4 ->
            16

        S8 ->
            32


spacing : Spacing -> Element.Attribute msg
spacing s =
    Element.spacing (intFromSpacing s)


padding : Padding -> Element.Attribute msg
padding p =
    Element.padding (intFromPadding p)


paddingEach : Padding -> Padding -> Padding -> Padding -> Element.Attribute msg
paddingEach top right bottom left =
    Element.paddingEach
        { top = intFromPadding top
        , right = intFromPadding right
        , bottom = intFromPadding bottom
        , left = intFromPadding left
        }


paddingXY : Padding -> Padding -> Element.Attribute msg
paddingXY px py =
    Element.paddingXY (intFromPadding px) (intFromPadding py)


paddingX : Padding -> Element.Attribute msg
paddingX px =
    Element.paddingXY (intFromPadding px) 0


paddingY : Padding -> Element.Attribute msg
paddingY py =
    Element.paddingXY 0 (intFromPadding py)


paddingTop : Padding -> Element.Attribute msg
paddingTop pt =
    Element.paddingEach
        { top = intFromPadding pt
        , right = 0
        , bottom = 0
        , left = 0
        }


paddingRight : Padding -> Element.Attribute msg
paddingRight pr =
    Element.paddingEach
        { top = 0
        , right = intFromPadding pr
        , bottom = 0
        , left = 0
        }


paddingBottom : Padding -> Element.Attribute msg
paddingBottom pb =
    Element.paddingEach
        { top = 0
        , right = 0
        , bottom = intFromPadding pb
        , left = 0
        }


paddingLeft : Padding -> Element.Attribute msg
paddingLeft pl =
    Element.paddingEach
        { top = 0
        , right = 0
        , bottom = 0
        , left = intFromPadding pl
        }


type Length
    = L4
    | L6
    | L8
    | L14
    | L28
    | L58
    | L63
    | L80
    | L96
    | L112
    | BigDesktopBreakpoint
    | TabletBreakpoint


intFromLength : Length -> Int
intFromLength theLength =
    case theLength of
        L4 ->
            16

        L6 ->
            24

        L8 ->
            32

        L14 ->
            56

        L28 ->
            112

        L58 ->
            232

        L63 ->
            252

        L80 ->
            320

        L96 ->
            384

        L112 ->
            448

        TabletBreakpoint ->
            1200

        BigDesktopBreakpoint ->
            1800


height : Length -> Element.Attribute msg
height theHeight =
    theHeight
        |> intFromLength
        |> Element.px
        |> Element.height


width : Length -> Element.Attribute msg
width theWidth =
    theWidth
        |> intFromLength
        |> Element.px
        |> Element.width


type Border
    = B0
    | B1
    | B2


border : Border -> Element.Attribute msg
border theBorder =
    Element.Border.width (intFromBorder theBorder)


borderBottom : Border -> Element.Attribute msg
borderBottom theBorder =
    Element.Border.widthEach
        { top = 0
        , right = 0
        , bottom = intFromBorder theBorder
        , left = 0
        }


intFromBorder : Border -> Int
intFromBorder theBorder =
    case theBorder of
        B0 ->
            0

        B1 ->
            1

        B2 ->
            2


type BorderRadius
    = Rounded
    | NotRounded


intFromBorderRadius : BorderRadius -> Int
intFromBorderRadius br =
    case br of
        NotRounded ->
            0

        Rounded ->
            4


borderRadius : BorderRadius -> Element.Attribute msg
borderRadius br =
    Element.Border.rounded (intFromBorderRadius br)


borderColor : Color -> Element.Attr decorative msg
borderColor =
    color >> Element.Border.color
