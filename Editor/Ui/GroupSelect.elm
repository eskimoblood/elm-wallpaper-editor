module Editor.Ui.GroupSelect where


import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)
import Editor.Action exposing(..)


groupTypes : List String
groupTypes = ["P1", "P2", "Pm", "Pg", "Cm", "P2mm", "P2mg", "C2mm", "P4", "P4mm", "P4mg", "P3","P3m1", "P31m" , "P6"]


createOption : String -> Html
createOption v = option[Attr.value v][text v]


groupSelect : Signal.Address Action -> Html
groupSelect address =
    select [
      on "change" targetValue (\str -> Signal.message address (Group str))
    ](List.map createOption groupTypes)
