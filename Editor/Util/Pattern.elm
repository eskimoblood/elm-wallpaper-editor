module Editor.Util.Pattern where

import Editor.Util.Noise exposing (noise3d)
import Editor.Util.TileSize exposing (..)
import Editor.Types exposing (Tile, Point, MultiLine, Bezier)
import Editor.Model exposing (Model)
import WallpaperGroup.Pattern as Pattern


scalePoint : {a | width: Float, height: Float, groupType: String} -> Point -> Point
scalePoint {width, height, groupType} p =
  { x= p.x / (getTileSize groupType) * width
  , y= p.y / (getTileSize groupType) * height
  }


getTileLength : List Tile -> Int
getTileLength tiles =
    List.length (Maybe.withDefault [] (List.head tiles))


randomPoint: Point -> Float -> Int -> Point
randomPoint p noise noiseDesctruction =
  let
    angle = noise * pi * 4
    rad = noise * (toFloat noiseDesctruction)
  in
    {x= p.x + (cos angle) * rad, y= p.y + (sin angle) * rad}


calcPath : Int -> (Float, MultiLine) -> Bezier
calcPath noiseDesctruction (noise, line)  =
  let
    p1 = Maybe.withDefault {x=0,y=0} (List.head line)
    p2 = Maybe.withDefault {x=0,y=0} (List.reverse line |> List.head)
    c1 = randomPoint p1 -noise noiseDesctruction
    c2 = randomPoint p2 noise noiseDesctruction
  in
    {p1= p1, p2=p2, c1=c1, c2=c2}

calcTile : Int -> List (Float, MultiLine) -> List Bezier
calcTile noiseDesctruction tile =
  let
    lines = List.filter (\ (s,_) -> s > 0) tile
  in
    List.map (calcPath noiseDesctruction) lines


updatePatternInModel : Model -> Model
updatePatternInModel  model =
  let
    patternState = model.patternState
    group = patternState.group
    columns = patternState.columns
    rows = patternState.rows
    tile = List.map (List.map (scalePoint patternState)) patternState.tile
    groups = Pattern.pattern group rows columns tile
    maxZ = getTileLength groups
    (noise, seed) =  (noise3d model maxZ)
    noisyGroups = (List.map2 (List.map2 (,)) noise groups)
    noiseDesctruction = patternState.noiseDesctruction
    pattern = List.map (calcTile noiseDesctruction) noisyGroups
  in
    {model | patternState = {patternState | pattern = pattern}, seed = seed}
