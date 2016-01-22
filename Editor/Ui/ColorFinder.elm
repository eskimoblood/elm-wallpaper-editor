module Editor.Ui.ColorFinder where


import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue, onBlur, onFocus)
import Editor.Action exposing (..)
import Editor.Model exposing (..)


colorItem : String -> Html
colorItem color =
  span [Attr.style [("backgroundColor", "#" ++ color)]] []


palette : Signal.Address Action -> List String -> Html
palette address colors =
  li
   [ Attr.class "preview"
   , Html.Events.onClick address  (SelectPalette colors)]
   (List.map colorItem colors)


colorList : Bool -> Bool -> List (List String) -> Signal.Address Action -> Html
colorList isShown isLoading palettes address =
  if isLoading then
    ul [Attr.class "loader"] [li [][]]
  else if isShown && ((List.length palettes) == 0) then
      span [] []
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
          , onBlur address (TogglePallete False)
          , onFocus address (TogglePallete True)
          , Attr.type' "text"
          , Attr.value model.colorSearch
         ]
         []
      , colorList model.paletteOpen model.loading model.palettes address
      ]
    ]
