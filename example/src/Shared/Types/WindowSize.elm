module Shared.Types.WindowSize exposing (..)

import Json.Decode as Decode
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)


type alias WindowSize =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


pixels : number -> number -> { width : Quantity number Pixels, height : Quantity number Pixels }
pixels width height =
    { width = Pixels.pixels width, height = Pixels.pixels height }


inPixels : WindowSize -> { width : Int, height : Int }
inPixels { width, height } =
    { width = Pixels.inPixels width, height = Pixels.inPixels height }


decode : Decode.Decoder WindowSize
decode =
    Decode.map2 pixels
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
