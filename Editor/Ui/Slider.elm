module Editor.Ui.Slider (..) where

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)
import Editor.Action exposing (..)


type alias SliderSettings =
    { min : String
    , max : String
    , value : String
    , address : Signal.Address Action
    , createAction : String -> Action
    }


slider : SliderSettings -> Html
slider { value, min, max, address, createAction } =
    div
        []
        [ input
            [ on "input" targetValue (\str -> Signal.message address (createAction str))
            , Attr.type' "range"
            , Attr.value value
            , Attr.max max
            , Attr.min min
            ]
            []
        , span
            []
            [ text value ]
        ]
