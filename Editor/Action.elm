module Editor.Action where

import Editor.Model exposing (Model)
import Editor.Types exposing (..)
import Editor.Util.Geom exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import WallpaperGroup.Group exposing(..)
import WallpaperGroup.Pattern as Pattern
import Random
import Array
import Date
import Debug exposing (log)

type Action
  = Columns Int
  | Rows Int
  | Group String
  | RasterSize Float
  | LineStart Point
  | LineMove Point
  | LineEnd Point
  | ClearTiles
  | Random

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

getRandom : Int -> Int -> Int -> Int
getRandom seed min max =
  let
    initialSeed = Random.initialSeed seed
    generator = Random.int min max
  in
    fst (Random.generate generator initialSeed)

getRandomCoord : Model -> Int -> MultiLine
getRandomCoord model i =
  let
    seed = getRandom (model.seed + i) Random.minInt Random.maxInt
    points = model.rasterCoords
    i1 = getRandom (seed + i) 0 (List.length points - 1)
    seed' = getRandom seed Random.minInt Random.maxInt
    i2 = getRandom (seed') 0 (List.length points - 1)
    getValue item =
    case item of
      Just i
        -> i
      Nothing
        -> {x=0, y=0}
  in
    [points
    |> Array.fromList
    |> Array.get i1
    |> getValue,
    points
    |> Array.fromList
    |> Array.get i2
    |> getValue
    ]


update : Action -> Model -> Model
update action model =
  case action of
    Rows value ->
      {model | rows = value}

    Columns value ->
      {model | columns = value}

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
        rasterCoords = rasterCoords value (Pattern.bounding (getGroup model.groupType 100 100)),
        tile = []
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
        tile = [model.lineStart, model.lineEnd] :: model.tile,
        isDrawing = False
      }

    ClearTiles ->
      {model |
        tile = []
      }

    Random ->
      let
        i = getRandom model.seed 3 10
        l = List.map (getRandomCoord model) [1..i]
      in
        {model |
          tile = l,
          seed = getRandom model.seed Random.minInt Random.maxInt
        }
