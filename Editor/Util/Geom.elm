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
