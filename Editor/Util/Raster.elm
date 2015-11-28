module Editor.Util.Raster where


import Editor.Types exposing (..)
import WallpaperGroup.Geom.BoundingBox exposing (..)


type alias SplitLines = (Line, Line)


line : Point -> Point -> Line
line p1 p2 = {start=p1, end=p2}


toLines : SplitLines -> Float ->  List Line
toLines lines steps=
  let
    l1 = splitLine steps (fst lines)
    l2 = splitLine steps (snd lines)
  in
    List.map2 line l1 l2

calcStep : Point -> Point -> Float -> Point
calcStep start step i = {x= start.x + step.x * i, y= start.y + step.y * i}

splitLine : Float -> Line -> List Point
splitLine steps {start, end} =
  if steps == 0 then
    [start]
  else
    let
      step = {x= (end.x - start.x) / (steps), y= (end.y - start.y) / (steps)}
    in
      List.map (calcStep start step)  [0..steps + 1]


rectRaster : Point -> Point -> Point -> Point ->  Float -> List Point
rectRaster  p1 p2 p3 p4  steps =
  toLines (line p1 p2, line p4 p3) steps
    |> List.map (splitLine steps)
    |> List.concat


triangleRaster : Point -> Point -> Point ->  Float -> List Point
triangleRaster  p1 p2 p3  steps =
  toLines (line p1 p2, line p1 p3) steps
    |> List.map2 splitLine [0..steps]
    |> List.concat



rasterCoords : Float -> BoundingBox -> List Point
rasterCoords steps bounding =
  case bounding of
    Triangle p1 p2 p3 ->
      triangleRaster p1 p2 p3 steps
    Rect p1 p2 p3 p4 ->
      rectRaster p1 p2 p3 p4 steps
