module Editor.Action where

import Editor.Model exposing (Model)
import WallpaperGroup.Group exposing(..)
import WallpaperGroup.Pattern as Pattern

type Action
  = Columns Int
  | Rows Int
  | Width Float
  | Height Float
  | Group String
  | RasterSize Float
  | LineStart (Float, Float)
  | LineMove (Float, Float)
  | LineEnd (Float, Float)

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
      {model |
        group = getGroup value model.height model.width,
        groupType = value,
        boundingBox = Pattern.bounding (getGroup value 100 100)
      }

    RasterSize value ->
      {model |
        rasterSize = value
      }

    LineStart mousePosition ->
      {model |
        lineStart = {x= fst mousePosition, y= snd mousePosition},
        isDrawing = True
      }

    LineMove mousePosition ->
      {model |
        lineEnd = {x= fst mousePosition, y= snd mousePosition}
      }

    LineEnd mousePosition ->
      {model |
        tile = [model.lineStart, model.lineEnd] :: model.tile,
        isDrawing = False
      }
