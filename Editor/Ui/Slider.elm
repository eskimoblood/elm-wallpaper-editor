module Editor.Ui.Slider where


import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)
import Editor.Action exposing (..)


type alias SliderSettings = {
  min: String,
  max: String,
  address: Signal.Address Action,
  createAction: (String -> Action)
}


slider : SliderSettings -> Html
slider {min, max, address, createAction} =
  input [
    on "input" targetValue (\str -> Signal.message address (createAction str)),
    Attr.type' "range",
    Attr.max max,
    Attr.min min
  ][]
