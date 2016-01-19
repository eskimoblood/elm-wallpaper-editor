module Editor.Action where

import Editor.Model exposing (..)
import Editor.Types exposing (..)
import Editor.Util.Geom exposing (..)
import Editor.Util.TileSize exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import Editor.Util.Noise exposing (noise3d)
import WallpaperGroup.Group exposing(..)
import Effects exposing (Effects , none)
import WallpaperGroup.Pattern as Pattern
import Random
import Array
import Effects exposing (..)
import Editor.Signals exposing (requestPalette)
import Task

import Debug

type Action
  = NoOp
  | Columns Int
  | Rows Int
  | Group String
  | RasterSize Float
  | LineStart Point
  | LineMove Point
  | LineEnd Point
  | DeleteLine Point
  | ClearTiles
  | Random
  | Undo
  | Redo
  | StartColorSearch String
  | NewColors (Result String (List (List String)))
  | SelectPalette (List String)

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

getRandom : Random.Seed -> Int -> Int -> (Int, Random.Seed)
getRandom seed min max =
  let
    generator = Random.int min max
  in
    Random.generate generator seed

getRandomCoord : List Point -> Random.Seed-> Int -> MultiLine
getRandomCoord points seed i =
  let
    seed' = Random.initialSeed (i + (fst (getRandom seed Random.minInt Random.maxInt )))
    (i1, seed'') = getRandom seed' 0 (List.length points - 1)
    (i2, _) = getRandom seed'' 0 (List.length points - 1)
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
    undoStack = model.undoStack
  in
    { model
    | undoStack = actualState :: undoStack
    , redoStack = []
    }


undo : Model -> Model
undo model =
  let
    lastState = List.head model.undoStack
    actualState = model.patternState
    undoStack = model.undoStack
    redoStack = model.redoStack
  in
    case lastState of

      Just state ->
        { model
        | undoStack = Maybe.withDefault [] (List.tail undoStack)
        , patternState = state
        , redoStack = actualState :: redoStack
        }

      Nothing ->
        model

redo : Model -> Model
redo model =
  let
    undoStack = model.undoStack
    actualState = model.patternState
    lastState = List.head model.redoStack
    newHistory = List.tail model.redoStack |> (Maybe.withDefault [])
  in
    case lastState of

      Just state ->
        { model
        | redoStack = newHistory
        , patternState = state
        , undoStack = actualState :: undoStack
        }

      Nothing ->
        model


update : Action -> Model -> (Model, Effects Action)
update action model =
  let
      patternState = model.patternState
      drawingState = model.drawingState
      colorState = model.colorState
  in
    case action of
      NoOp ->
        (model, Effects.none)
      Rows value ->
        let
          model = addHistory model
        in
          ({model | patternState = {patternState | rows = value}}, Effects.none)

      Columns value ->
        let
          model = addHistory model
        in
        (  {model | patternState = {patternState | columns = value}}, Effects.none)

      Group value ->
        let
          model = addHistory model
          previewGroupSize = getTileSize value
          previewGroup = getGroup value previewGroupSize previewGroupSize
          boundingBox  = Pattern.bounding (previewGroup)
        in
          ({ model |
            patternState =
              { patternState |
                group = getGroup value patternState.height patternState.width
              , previewGroup = previewGroup
              , groupType = value
              , boundingBox = boundingBox
              },
            drawingState =
              {drawingState |
                  rasterCoords = rasterCoords patternState.rasterSize (Pattern.bounding (getGroup value previewGroupSize previewGroupSize))
              }
          } , Effects.none)
      RasterSize value ->
        let
          model = addHistory model
          previewGroupSize = getTileSize model.patternState.groupType
        in
          ({model | patternState =
            { patternState |
              rasterSize = value
              ,tile = []
            },
            drawingState = {
               drawingState |
                 rasterCoords = rasterCoords value (Pattern.bounding (getGroup patternState.groupType previewGroupSize previewGroupSize))
             }
           }, Effects.none)

      LineStart mousePosition ->
        ({model | drawingState =
          {drawingState |
            lineStart = snap drawingState.rasterCoords mousePosition,
            lineEnd = snap drawingState.rasterCoords mousePosition,
            isDrawing = True
          }
        }, Effects.none)

      LineMove mousePosition ->
        if drawingState.isDrawing then
          ({ model |
            drawingState = { drawingState |
              lineEnd = snap drawingState.rasterCoords mousePosition }
          }, Effects.none)
        else
          (model, Effects.none)

      LineEnd mousePosition ->
        if drawingState.isDrawing then
          let
            model = addHistory model
            tile = [drawingState.lineStart, drawingState.lineEnd] :: patternState.tile

            (noise, seed) = noise3d model (List.length tile)
            e = Debug.log "noise" noise
          in
            ({ model |
              drawingState = { drawingState | isDrawing = False },
              patternState = { patternState | tile = tile , noise = noise},
              seed = seed
            }, Effects.none)
        else
          (model, Effects.none)

      DeleteLine mousePosition ->
        let
          model = addHistory model
          tile = List.filter (lineIsNearPoint mousePosition 5) patternState.tile
          (noise, seed) = noise3d model (List.length tile)
        in
          ({ model |
            patternState = {patternState | tile = tile, noise = noise },
            seed = seed
          }, Effects.none)

      ClearTiles ->
        let
          model = addHistory model
        in
          ({ model |
            patternState = {patternState | tile = [], noise = [] }

          }, Effects.none)

      Random ->
        let
          model = addHistory model
          (i, seed') = getRandom model.seed 3 10
          tile = List.map (getRandomCoord drawingState.rasterCoords seed') [1..i]
          (noise, seed) = noise3d model (List.length tile)
        in
          ({model |
            patternState = {patternState | tile = tile, noise = noise},
            seed = seed'
          }, Effects.none)

      Undo ->
        ((undo model), Effects.none)

      Redo ->
        ((redo model), Effects.none)

      StartColorSearch str ->
        let
          model = addHistory model
          sendTask =
          Signal.send requestPalette.address str
            `Task.andThen` (\_ -> Task.succeed NoOp)
        in
          ({ model |
            colorState = { colorState | colorSearch = str, loading = True }
          }, sendTask |> Effects.task)

      NewColors colors ->
        case colors of
          Ok c ->
              ({ model |
              colorState = {colorState | palettes = c, loading = False }

          }, Effects.none)
          Err  e->
            ({ model |
              colorState = {colorState | palettes = [], loading = False }

          }, Effects.none)

      SelectPalette palette ->
        let
          model = addHistory model
        in
          ({ model |
            colorState = {colorState | selectedPalette = palette }
          }, Effects.none)
