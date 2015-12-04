module Editor.Util.Geom where

import Editor.Types exposing (..)


distance : Point -> Point -> Float
distance p1 p2 =
  sqrt (p1.x - p2.x) ^ 2  + (p1.y - p2.y) ^ 2


find : Point -> Point -> {p: Point, d: Float} -> {p: Point, d: Float}
find p1 p2 r=
  let
    dist = distance p1 p2
  in
    if dist < r.d then
      {p= p2, d= dist}
    else
      r

snap : List Point -> Point -> Point
snap points p =
  .p (List.foldl (find p) {d= 10000000, p= p} points)
