module Editor.Ui.PatternStage where

import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Editor.Types exposing (Tile, Point, MultiLine, Bezier)
import Editor.Util.Svg exposing (lineToAttribute, renderTiles, pointToString)
import String


renderLine : (Float, MultiLine) -> Svg
renderLine (noise, line) =
   Svg.line (List.foldl lineToAttribute [] line) []



renderPath :  Bezier -> Svg
renderPath {p1, p2, c1, c2, opacity, color}  =
 let
   path = String.join " " ["M", (pointToString p1), "C", (pointToString c1), (pointToString c2), (pointToString p2)]

 in
  Svg.path
     [ d path
     , stroke color
     , Attr.opacity (toString opacity)
     , fill "none"
     , class "tile"
     ]
     []

renderTile : List Bezier -> Svg
renderTile  tile =
    Svg.g [] (List.map renderPath tile)


renderColorizedNoisyTiles : List (List Bezier) -> Svg
renderColorizedNoisyTiles tiles =
    Svg.g
      []
      (List.map renderTile tiles)


stage : List (List Bezier) -> Svg
stage  model =
    svg
      [ version "1.1", x "0", y "0"]
      [renderColorizedNoisyTiles model]
