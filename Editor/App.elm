module Editor.App where

import WallpaperGroup.Group exposing (..)
import WallpaperGroup.Pattern as Pattern

import Graphics.Element exposing (show)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)
import StartApp.Simple exposing (start)

import Editor.PatternStage exposing (stage)
import String
import Editor.Action exposing(..)
import Editor.Model exposing (Model)

import Editor.Ui.Slider exposing (slider)
import Editor.Ui.GroupSelect exposing (groupSelect)
import Editor.Ui.Raster exposing (raster)
import Editor.Util.Raster exposing (rasterCoords)


import Editor.Types exposing (..)



contentToValue : String -> Int
contentToValue str =
    let
      result = String.toInt str
    in
      case result of
        Ok v -> v
        Err e -> 0

contentToValue2 : String -> Float
contentToValue2 str =
    let
      result = String.toFloat str
    in
      case result of
        Ok v -> v
        Err e -> 0



view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [
      slider {min= "1", max= "10", address= address, createAction= \str -> Columns(contentToValue str)},
      slider {min= "1", max= "10", address= address, createAction= \str -> Rows(contentToValue str)},
      slider {min= "10", max= "100", address= address, createAction= \str -> Width(contentToValue2 str)},
      slider {min= "10", max= "100", address= address, createAction= \str -> Height(contentToValue2 str)},
      groupSelect address,
      raster model address,
      button [
        on "click" targetValue (\_ -> Signal.message address ClearTiles)
      ][Html.text "Clear"],
      stage model.group model.columns model.rows model.tile
    ]

m = {
      lineStart={x=0, y=0},
      lineEnd={x=0, y=0},
      isDrawing=False,
      columns= 10,
      rows= 10,
      width= 20,
      height =20,
      groupType= "P1",
      rasterSize= 4,
      boundingBox = Pattern.bounding (P1 20 20),
      rasterCoords = rasterCoords 4 (Pattern.bounding (P1 100 100)),
      tile= [],
      group= P1 40 40
    }

main =
  start
    { model = m
    , update = update
    , view = view
    }
