module Editor.Ui.PatternStage where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import WallpaperGroup.Pattern as Pattern
import WallpaperGroup.Group exposing (..)

import Editor.Types exposing (Tile, Point)
import Editor.Model exposing (..)
import Debug exposing (log)
import Editor.Util.Svg exposing (renderTile)


scalePoint : {a | width: Float, height: Float} -> Point -> Point
scalePoint {width, height} p=
  {x= p.x / 100 * width, y= p.y / 100 * height}

renderTiles :  Tile -> Svg
renderTiles  tile =
  Svg.g [stroke "grey"] (renderTile tile)

stage : Model -> Svg
stage  model =
  let
    tile = List.map (List.map (scalePoint model)) model.tile
  in
    svg
      [ version "1.1", x "0", y "0" ]
      [
        Svg.g
          []
          (List.map renderTiles (Pattern.pattern model.group model.columns model.rows tile))
      ]