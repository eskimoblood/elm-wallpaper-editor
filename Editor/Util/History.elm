module Editor.Util.History (..) where

import Editor.Model exposing (Model)
import Editor.Util.Raster exposing (rasterCoords)
import WallpaperGroup.Pattern as Pattern
import Editor.Util.Groups exposing (getGroup)
import Editor.Util.TileSize exposing (..)


addHistory : Model -> Model
addHistory model =
    let
        actualState = model.patternState

        undoStack = List.take 100 model.undoStack
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
                let
                    drawingState = model.drawingState

                    previewGroupSize = getPreviewTileSize state.groupType
                in
                    { model
                        | undoStack = Maybe.withDefault [] (List.tail undoStack)
                        , patternState = state
                        , redoStack = actualState :: redoStack
                        , drawingState =
                            { drawingState
                                | rasterCoords = rasterCoords state.rasterSize (Pattern.bounding (getGroup state.groupType previewGroupSize previewGroupSize))
                            }
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
              let
                  drawingState = model.drawingState

                  previewGroupSize = getPreviewTileSize state.groupType
              in
                { model
                    | redoStack = newHistory
                    , patternState = state
                    , undoStack = actualState :: undoStack
                    , drawingState =
                        { drawingState
                            | rasterCoords = rasterCoords state.rasterSize (Pattern.bounding (getGroup state.groupType previewGroupSize previewGroupSize))
                        }
                }

            Nothing ->
                model
