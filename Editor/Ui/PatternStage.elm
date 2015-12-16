module Editor.Ui.PatternStage where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Editor.Types exposing (Tile, Point)
import Editor.Model exposing (PatternState)
import Editor.Util.Svg exposing (renderTiles)


scalePoint : {a | width: Float, height: Float} -> Point -> Point
scalePoint {width, height} p =
  { x= p.x / 100 * width
  , y= p.y / 100 * height
  }

stage : PatternState -> Svg
stage  model =
  let
    tile = List.map (List.map (scalePoint model)) model.tile
  in
    svg
      [ version "1.1", x "0", y "0"]
      [(renderTiles model.group model.columns model.rows tile)]
