module Editor.Model where

import  WallpaperGroup.Group exposing (..)
import  WallpaperGroup.Geom.BoundingBox exposing (..)
import Editor.Types exposing (..)

type alias Model = {
  columns: Int,
  rows: Int,
  width: Float,
  height: Float,
  group: Group,
  groupType: String,
  boundingBox: BoundingBox,
  rasterSize: Float,
  tile: Tile,
  lineStart: Point,
  lineEnd: Point,
  isDrawing: Bool
}
