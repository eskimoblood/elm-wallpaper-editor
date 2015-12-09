module Editor.Action where

import Editor.Model exposing (..)
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
  | Undo
  | Redo

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

getRandomCoord : List Point -> Int-> Int -> MultiLine
getRandomCoord points seed i =
  let
    seed = getRandom (seed + i) Random.minInt Random.maxInt
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
    [ points
      |> Array.fromList
      |> Array.get i1
      |> getValue
    , points
      |> Array.fromList
      |> Array.get i2
      |> getValue
    ]

addHistory : Model -> Model
addHistory model =
  let
    actualState = model.patternState
    history = model.history
  in
     {model | history = actualState :: history}


undo : Model -> Model
undo model =
  let
    lastState = List.head model.history
    newHistory = List.tail model.history |> (Maybe.withDefault [])
  in
    case lastState of

      Just state ->
        {model | history = newHistory, patternState = state}

      Nothing ->
        model


update : Action -> Model -> Model
update action model =
  let
      patternState = model.patternState
      drawingState = model.drawingState
  in
    case action of
      Rows value ->
        let
          model = addHistory model
        in
          {model | patternState = {patternState | rows = value}}

      Columns value ->
        let
          model = addHistory model
        in
          {model | patternState = {patternState | columns = value}}

      Group value ->
        let
          model = addHistory model
          boundingBox  = Pattern.bounding (getGroup value 100 100)
        in
          { model |
            patternState =
              { patternState |
                group = getGroup value patternState.height patternState.width
              , groupType = value
              , boundingBox = Pattern.bounding (getGroup value patternState.height patternState.width)
              },
            drawingState =
              {drawingState |
                  rasterCoords = rasterCoords patternState.rasterSize (Pattern.bounding (getGroup value 100 100))
              }
          }

      RasterSize value ->
        let
          model = addHistory model
        in
          {model | patternState =
            { patternState |
              rasterSize = value
              ,tile = []
            },
            drawingState = {
               drawingState |
                 rasterCoords = rasterCoords value (Pattern.bounding (getGroup patternState.groupType 100 100))
             }
           }

      LineStart mousePosition ->
        {model | drawingState =
          {drawingState |
            lineStart = snap drawingState.rasterCoords mousePosition,
            lineEnd = snap drawingState.rasterCoords mousePosition,
            isDrawing = True
        }}

      LineMove mousePosition ->
        if drawingState.isDrawing then
          { model |
            drawingState = { drawingState |
              lineEnd = snap drawingState.rasterCoords mousePosition }
          }
        else
          model

      LineEnd mousePosition ->
        let
          model = addHistory model
        in
          { model |
            drawingState = { drawingState | isDrawing = False },
            patternState = { patternState | tile = [drawingState.lineStart, drawingState.lineEnd] :: patternState.tile}
          }

      ClearTiles ->
        let
          model = addHistory model
        in
          { model |
            patternState = {patternState | tile = [] }
          }

      Random ->
        let
          model = addHistory model
          i = getRandom model.seed 3 10
          l = List.map (getRandomCoord drawingState.rasterCoords model.seed) [1..i]
        in
          {model |
            patternState = {patternState | tile = l},
            seed = getRandom model.seed Random.minInt Random.maxInt
          }
      Undo ->
        undo model

      Redo ->
        model
