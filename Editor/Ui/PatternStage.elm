module Editor.Ui.PatternStage where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Editor.Types exposing (Tile, Point, MultiLine)
import Editor.Model exposing (Model)
import Editor.Util.Svg exposing (lineToAttribute, renderTiles)
import Editor.Model exposing (Model)
import Editor.Util.TileSize exposing (..)
import WallpaperGroup.Pattern as Pattern
import Editor.Util.Noise exposing (noise3d)


scalePoint : {a | width: Float, height: Float, groupType: String} -> Point -> Point
scalePoint {width, height, groupType} p =
  { x= p.x / (getTileSize groupType) * width
  , y= p.y / (getTileSize groupType) * height
  }

renderLine : (Float, MultiLine) -> Svg
renderLine (noise, line) =
   Svg.line (List.foldl lineToAttribute [] line) []


renderTile : List (Float, MultiLine) -> Svg
renderTile  tile =
  let
    lines = List.filter (\ (s,_) -> s > 0.6) tile
  in
    Svg.g [stroke "grey"] (List.map renderLine lines)

getTileLength : List Tile -> Int
getTileLength tiles =
    List.length (Maybe.withDefault [] (List.head tiles))

renderColorizedNoisyTiles : Model -> Svg
renderColorizedNoisyTiles model =
  let
    patternState = model.patternState
    group = Debug.log "" patternState.group
    columns = patternState.columns
    rows = patternState.rows
    tile = List.map (List.map (scalePoint patternState)) patternState.tile
    groups = Debug.log "groups" (Pattern.pattern group columns rows tile)
    maxZ = getTileLength groups
    noise = Debug.log "noise "(fst (noise3d model maxZ))
    noisyGroups = (List.map2 (List.map2 (,)) noise groups)
  in
    Svg.g
      []
      (List.map renderTile noisyGroups)


stage : Model -> Svg
stage  model =
    svg
      [ version "1.1", x "0", y "0"]
      [renderColorizedNoisyTiles model]
