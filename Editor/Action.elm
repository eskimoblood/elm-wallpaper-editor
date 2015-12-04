module Editor.Action where

import Editor.Model exposing (Model)
import Editor.Types exposing (..)
import Editor.Util.Geom exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import WallpaperGroup.Group exposing(..)
import WallpaperGroup.Pattern as Pattern



type Action
  = Columns Int
  | Rows Int
  | Width Float
  | Height Float
  | Group String
  | RasterSize Float
  | LineStart Point
  | LineMove Point
  | LineEnd Point
  | ClearTiles

getGroup : String -> Float -> Float -> Group
getGroup groupType height width =
  if groupType == "P1" then
    P1 width height

  else if groupType == "P2" then
    P2 width height

  else if groupType == "Pm" then
    Pm width height

  else if groupType == "Pg" then
    Pg width height

  else if groupType == "Cm" then
    Cm width height

  else if groupType == "P2mm" then
    P2mm width height

  else if groupType == "P2mg" then
    P2mg width height

  else if groupType == "C2mm" then
    C2mm width height

  else if groupType == "P4" then
    P4 width height

  else if groupType == "P4mm" then
    P4mm width height

  else if groupType == "P4mg" then
    P4mg width height

  else if groupType == "P3" then
    P3 width

  else if groupType == "P3m1" then
    P3m1 width

  else if groupType == "P31m" then
    P31m width

  else if groupType == "P6" then
    P6 width

  else
    P1 width height

scalePoint : Model -> Point -> Point
scalePoint model p=
  {x= p.x / 100 * model.width, y= p.y / 100 * model.height}


update : Action -> Model -> Model
update action model =
  case action of
    Rows value ->
      {model | rows = value}

    Columns value ->
      {model | columns = value}

    Width value ->
      {model |
        width = value,
        group = getGroup model.groupType model.height value
      }

    Height value ->
      {model |
        height = value,
        group = getGroup model.groupType value model.width
      }

    Group value ->
      let
        boundingBox  = Pattern.bounding (getGroup value 100 100)
      in
        {model |
          group = getGroup value model.height model.width,
          groupType = value,
          boundingBox = Pattern.bounding (getGroup value model.height model.width),
          rasterCoords = rasterCoords model.rasterSize (Pattern.bounding (getGroup value 100 100))
        }

    RasterSize value ->
      {model |
        rasterSize = value,
        rasterCoords = rasterCoords value model.boundingBox
      }

    LineStart mousePosition ->
      {model |
        lineStart = snap model.rasterCoords mousePosition,
        lineEnd = snap model.rasterCoords mousePosition,
        isDrawing = True
      }

    LineMove mousePosition ->
      if model.isDrawing then
        {model |
          lineEnd = snap model.rasterCoords mousePosition
        }
      else
        model

    LineEnd mousePosition ->
      {model |
        tile = [scalePoint model model.lineStart, scalePoint model model.lineEnd] :: model.tile,
        isDrawing = False
      }

    ClearTiles ->
      {model |
        tile = []
      }
