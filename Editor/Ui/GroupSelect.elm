module Editor.Ui.GroupSelect where


import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)
import Editor.Action exposing(..)


groupTypes : List String
groupTypes = ["P1", "P2"]


createOption : String -> Html
createOption v = option[Attr.value v][text v]


groupSelect : Signal.Address Action -> Html
groupSelect address =
    select [
      on "change" targetValue (\str -> Signal.message address Group str)
    ](List.map createOption groupTypes)
