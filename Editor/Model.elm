module Editor.Model where

import  WallpaperGroup.Group exposing (..)
import  WallpaperGroup.Geom.BoundingBox exposing (..)
import Editor.Types exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import WallpaperGroup.Pattern as Pattern

type alias Model = {
  columns: Int,
  rows: Int,
  width: Float,
  height: Float,
  group: Group,
  groupType: String,
  boundingBox: BoundingBox,
  rasterCoords: List Point,
  rasterSize: Float,
  tile: Tile,
  lineStart: Point,
  lineEnd: Point,
  isDrawing: Bool,
  seed: Int
}

initialModel = {
      lineStart={x=0, y=0},
      lineEnd={x=0, y=0},
      isDrawing=False,
      columns= 10,
      rows= 10,
      width= 40,
      height =40,
      groupType= "P1",
      rasterSize= 4,
      boundingBox = Pattern.bounding (P1 20 20),
      rasterCoords = rasterCoords 4 (Pattern.bounding (P1 100 100)),
      tile= [],
      group= P1 40 40,
      seed = 0
    }
