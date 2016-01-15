module Editor.View where

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)

import Editor.Ui.Slider exposing (slider)
import Editor.Ui.GroupSelect exposing (groupSelect)
import Editor.Ui.Raster exposing (raster)
import Editor.Ui.ColorFinder exposing (colorFinder)

import Editor.Util.Convert as Convert
import Editor.Ui.PatternStage exposing (stage)
import Editor.Model exposing (Model)
import Editor.Action exposing (..)


view : Signal.Address Action -> Model -> Html
view address model =
  let
    patternState = model.patternState
    drawingState = model.drawingState
    undoDisabled = List.length (model.undoStack) == 0
    redoDisabled = List.length (model.redoStack) == 0
  in
    div
      [Attr.class "row", Attr.id "String"
      ]
      [ div
          [Attr.class "sidebar"
          ]
          [ slider { value = (toString patternState.rasterSize)
                   , min= "1"
                   , max= "20"
                   , address= address, createAction= \str -> RasterSize(Convert.toFloat str)
                   }
          , slider { value=( toString patternState.columns)
                   , min= "1"
                   , max= "20"
                   , address= address
                   , createAction= \str -> Columns(Convert.toInt str)
                   }
          , slider { value = (toString patternState.rows)
                   , min= "1"
                   , max= "20"
                   , address= address
                   , createAction= \str -> Rows(Convert.toInt str)
                   }
          , groupSelect patternState.groupType address
          , raster drawingState patternState.tile patternState.previewGroup patternState.boundingBox address
          , button
              [ on "click" targetValue (\_ -> Signal.message address ClearTiles)
              ]
              [ Html.text "Clear"
              ]
          , button
              [ on "click" targetValue (\_ -> Signal.message address Random)
              ]
              [Html.text "Random"
              ]
          , button
              [ on "click" targetValue (\_ -> Signal.message address Undo)
              , Attr.disabled undoDisabled
              ]
              [Html.text "Undo"
              ]
          , button
              [ on "click" targetValue (\_ -> Signal.message address Redo)
              , Attr.disabled redoDisabled
              ]
              [Html.text "Redo"
              ]
          , colorFinder address model.colorState
          ]
      , div
          [ Attr.class "main lalasd"
          ]
          [stage patternState
          ]
      ]
