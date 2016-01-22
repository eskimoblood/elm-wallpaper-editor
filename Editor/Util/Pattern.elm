module Editor.Util.Pattern where

import Editor.Util.Noise exposing (noise3d)
import Editor.Util.TileSize exposing (..)
import Editor.Types exposing (Tile, Point, MultiLine, Bezier)
import Editor.Model exposing (Model)
import WallpaperGroup.Pattern as Pattern
import Array


getColor : List String -> Float -> String
getColor colors noise =
  let
    color = Array.fromList colors
      |> Array.get (floor ((toFloat (List.length colors) * (abs noise))))
  in
    case color of
      Just cl -> "#" ++ cl
      Nothing -> "grey"


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


calcPath : Int -> List String -> (Float, MultiLine) -> Bezier
calcPath noiseDesctruction colors (noise, line)  =
  let
    p1 = Maybe.withDefault {x=0,y=0} (List.head line)
    p2 = Maybe.withDefault {x=0,y=0} (List.reverse line |> List.head)
    c1 = randomPoint p1 -noise noiseDesctruction
    c2 = randomPoint p2 noise noiseDesctruction
    color = getColor colors noise
  in
    {p1= p1, p2=p2, c1=c1, c2=c2, color= color, opacity= 1 - (abs noise)}

calcTile : Int -> List String -> List (Float, MultiLine) -> List Bezier
calcTile noiseDesctruction colors tile =
  let
    lines = List.filter (\ (s,_) -> (abs s) > 0.2) tile
  in
    List.map (calcPath noiseDesctruction colors) lines


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
    colors = model.colorState.selectedPalette
    pattern = List.map (calcTile noiseDesctruction colors) noisyGroups

  in
    {model | patternState = {patternState | pattern = pattern}, seed = seed}
