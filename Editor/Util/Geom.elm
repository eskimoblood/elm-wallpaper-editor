module Editor.Util.Geom where

import Editor.Types exposing (..)
import Random as Random

distance : Point -> Point -> Float
distance p1 p2 =
  sqrt (((p1.x - p2.x) ^ 2) + ((p1.y - p2.y) ^ 2))


findShortest : Point -> Point -> {p: Point, d: Float} -> {p: Point, d: Float}
findShortest p1 p2 r =
  let
    dist = distance p1 p2
  in
    if dist < r.d then
      {p= p2, d= dist}
    else
      r


snap : List Point -> Point -> Point
snap points p =
  .p (List.foldl (findShortest p) {d= Random.maxInt |> toFloat, p= p} points)


multiLineToTuple : MultiLine -> (Point, Point)
multiLineToTuple line =
  let
    p1 = List.head line |> Maybe.withDefault {x=0, y=0}
    p2 = List.drop 1 line |> List.head |> Maybe.withDefault p1
  in
    (p1, p2)


coords : Float -> Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float)
coords param x1 x2 y1 y2 c d =
  if param < 0 || (x1 == x2 && y1 == y2) then
    (x1, y1)

  else if param > 1 then
    (x2, y2)

  else
    (x1 + param * c, y1 + param * d)

lineIsNearPoint : Point -> Float -> MultiLine -> Bool
lineIsNearPoint p distance line' =
  let
    line = multiLineToTuple line'
    p1 = fst line
    p2 = snd line
    x1 = p1.x
    y1 = p1.y
    x2 = p2.x
    y2 = p2.y

    a = p.x - x1
    b = p.y - y1
    c = x2 - x1
    d = y2 - y1

    dot = a * c + b * d
    len_sq = c * c + d * d
    param = dot / len_sq

    cords = coords param x1 x2 y1 y2 c d

    dx = p.x - (fst cords)
    dy = p.y - (snd cords)
    actualDistance = sqrt dx * dx + dy * dy
  in
    actualDistance > distance
