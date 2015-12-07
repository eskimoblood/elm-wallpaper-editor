module Editor.Util.Svg where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Editor.Types exposing (Point, MultiLine, Tile)

lineToAttribute : Point -> List Svg.Attribute -> List Svg.Attribute
lineToAttribute {x, y} attributes =
  if List.length attributes == 0  then
    List.append [x1 (toString x), y1 (toString y)] attributes
  else
    List.append [x2 (toString x), y2 (toString y)] attributes


renderLine : MultiLine -> Svg
renderLine line =
   Svg.line (List.foldl lineToAttribute [] line) []


renderTile : Tile ->  List Svg
renderTile tile =
  List.map renderLine tile
