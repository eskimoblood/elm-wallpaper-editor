module Editor.Ui.ColorFinder where


import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)
import Editor.Action exposing (..)
import Editor.Model exposing (..)


colorItem : String -> Html
colorItem color =
  span [Attr.style [("backgroundColor", "#" ++ color)]] []


palette : Signal.Address Action -> List String -> Html
palette address colors =
  li
   [ Attr.class "preview"
   , on "click" targetValue (\str -> Signal.message address  (SelectPalette colors))]
   (List.map colorItem colors)


colorList : Bool -> List (List String) -> Signal.Address Action -> Html
colorList isLoading palettes address =
  if isLoading then
    ul [Attr.class "loader"] [li [][]]
  else
    ul [] (List.map (palette address) palettes)


colorFinder : Signal.Address Action -> ColorState -> Html
colorFinder address model =
  div
    [ Attr.class "palette"]
    [ div
      [Attr.class "column"]
      [input
          [ on "input" targetValue (\str -> Signal.message address  (StartColorSearch str))
          , Attr.type' "text"
          , Attr.value model.colorSearch
         ]
         []
      , colorList model.loading model.palettes address
      ]
    ]
