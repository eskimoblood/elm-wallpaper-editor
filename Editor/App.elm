module Editor.App where

import WallpaperGroup.Group exposing (..)

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


type alias Point = {x:Float, y:Float}
type alias Line = List Point
type alias Tile = List Line


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
      stage model.group model.columns model.rows model.tile
    ]

m = {
  columns= 10,
  rows= 10,
  width= 20,
  height =20,
  groupType= "P1",
  tile= [
    [{x=0, y=0}, {x=20, y=10}],
    [{x=30, y=30}, {x=20, y=10}],
    [{x=0, y=0}, {x=10, y=20}]
    ],
    group= P1 40 40
    }

main =
  start
    { model = m
    , update = update
    , view = view
    }
