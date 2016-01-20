module Editor.Util.History where

import Editor.Model exposing (Model)

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
