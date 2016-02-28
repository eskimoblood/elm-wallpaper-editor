module Editor.Ui.Header (..) where

import Svg exposing (..)
import Svg.Attributes exposing (..)


header : String -> Svg
header t =
    svg
        [ height "50" ]
        [ Svg.text'
            [ fill "white"
            , y "40"
            ]
            [ text t ]
        ]
