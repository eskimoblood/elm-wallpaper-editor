module Editor.Util.Color (..) where

import Color.Gradient exposing (gradient)
import Color.Convert exposing (hexToColor, colorToCssRgb)


toGradient : List String -> List String
toGradient l = Debug.log "colors"
    (List.map hexToColor l
        |> List.filterMap identity)
        |> (flip gradient) 20
        |> List.map colorToCssRgb
