module Editor.Ui.PatternStage where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Editor.Types exposing (Tile, Point)
import Editor.Model exposing (PatternState)
import Editor.Util.Svg exposing (renderTiles)
import Editor.Util.TileSize exposing (..)


scalePoint : {a | width: Float, height: Float, groupType: String} -> Point -> Point
scalePoint {width, height, groupType} p =
  { x= p.x / (getTileSize groupType) * width
  , y= p.y / (getTileSize groupType) * height
  }

stage : PatternState -> Svg
stage  model =
  let
    tile = List.map (List.map (scalePoint model)) model.tile
  in
    svg
      [ version "1.1", x "0", y "0"]
      [(renderTiles model.group model.columns model.rows tile)]
