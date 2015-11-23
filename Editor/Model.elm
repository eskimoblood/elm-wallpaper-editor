module Editor.Model where

import  WallpaperGroup.Group exposing (..)

type alias Point = {x:Float, y:Float}
type alias Line = List Point
type alias Tile = List Line

type alias Model = {
  columns: Int,
  rows: Int,
  width: Float,
  height: Float,
  group: Group,
  tile: Tile
}
