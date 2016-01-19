module Editor.Ui.PatternStage where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Editor.Types exposing (Tile, Point, MultiLine)
import Editor.Model exposing (Model)
import Editor.Util.Svg exposing (lineToAttribute, renderTiles, pointToString)
import Editor.Model exposing (Model)
import Editor.Util.TileSize exposing (..)
import WallpaperGroup.Pattern as Pattern
import Editor.Util.Noise exposing (noise3d)
import String

scalePoint : {a | width: Float, height: Float, groupType: String} -> Point -> Point
scalePoint {width, height, groupType} p =
  { x= p.x / (getTileSize groupType) * width
  , y= p.y / (getTileSize groupType) * height
  }

renderLine : (Float, MultiLine) -> Svg
renderLine (noise, line) =
   Svg.line (List.foldl lineToAttribute [] line) []


renderPath :  (Float, MultiLine) -> Svg
renderPath (noise, line)  =
 let
   p1 = Maybe.withDefault {x=0,y=0} (List.head line)
   p2 = Maybe.withDefault {x=0,y=0} (List.reverse line |> List.head)
   c1 = randomPoint p1 (noise* -1)
   c2 = randomPoint p2 noise
   path = String.join " " [  "M", (pointToString p1), "C", (pointToString c1), (pointToString c2), (pointToString p2)]

 in
  Svg.path
     [ d path
     , fill "none"
     , class "tile"
     ]
     []

randomPoint: Point -> Float -> Point
randomPoint p noise =
  let
    angle = noise * pi * 4
    rad = noise * 4
  in
    {x= p.x + (cos angle) * rad, y= p.y + (sin angle) * rad}

renderTile : List (Float, MultiLine) -> Svg
renderTile  tile =
  let
    lines = List.filter (\ (s,_) -> s > 0) tile
  in
    Svg.g [stroke "grey"] (List.map renderPath lines)

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
    groups = Pattern.pattern group rows columns tile
    maxZ = getTileLength groups
    noise = fst (noise3d model maxZ)
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
