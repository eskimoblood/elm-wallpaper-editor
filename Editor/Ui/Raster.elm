module Editor.Ui.Raster where

import Editor.Util.Raster exposing (rasterCoords)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Editor.Types exposing (..)
import WallpaperGroup.Geom.BoundingBox exposing (..)

renderPoint : Point -> Svg
renderPoint p =
  Svg.circle [cx (toString p.x), cy (toString p.y), r "1"][]


raster : BoundingBox -> Float -> Svg
raster bounding steps =
  let
    points = rasterCoords steps bounding
  in
    svg [version "1.1", x "0", y "0"]
    (List.map renderPoint points)
