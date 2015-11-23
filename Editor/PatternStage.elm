module Editor.PatternStage where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import WallpaperGroup.Pattern exposing (pattern)
import WallpaperGroup.Group exposing (..)


type alias Point = {x:Float, y:Float}
type alias Line = List Point
type alias Tile = List Line

lineToAttribute : Point -> List Svg.Attribute -> List Svg.Attribute
lineToAttribute {x, y} attributes =
  if List.length attributes == 1  then
    List.append [x1 (toString x), y1 (toString y)] attributes
  else
    List.append [x2 (toString x), y2 (toString y)] attributes


renderLine : Line -> Svg
renderLine line =
   Svg.line (List.foldl lineToAttribute [stroke "black"] line) []

renderTile : Tile -> Svg
renderTile tile = Svg.g [] (List.map renderLine tile)

stage : Group -> Int -> Int -> Tile -> Svg
stage  group columns rows tile =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ] (
    List.map renderTile (WallpaperGroup.Pattern.pattern group columns rows tile))
