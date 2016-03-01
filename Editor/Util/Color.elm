module Editor.Util.Color (..) where

import Color.Gradient exposing (gradient)
import Color.Convert exposing (hexToColor, colorToCssRgb)
import Color.Interpolate exposing (..)


toGradient : List String -> List String
toGradient l =
    (List.map hexToColor l
        |> List.filterMap identity
    )
        |> (flip (gradient LAB)) 20
        |> List.map colorToCssRgb
