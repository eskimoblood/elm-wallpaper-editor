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
    [input [
      on "input" targetValue (\str -> Signal.message address (Columns(contentToValue str))),
      type' "range",
      Attr.max "10",
      Attr.min "1"
      ][],
     input [
      on "input" targetValue (\str -> Signal.message address (Rows(contentToValue str))),
      type' "range",
      Attr.max "10",
      Attr.min "1"
      ][],
     input [
      on "input" targetValue (\str -> Signal.message address (Width(contentToValue2 str))),
      type' "range",
      Attr.max "50",
      Attr.min "10"
      ][],
     input [
      on "input" targetValue (\str -> Signal.message address (Height(contentToValue2 str))),
      type' "range",
      Attr.max "50",
      Attr.min "10"
      ][],

      stage (P4 model.width model.height) model.columns model.rows model.tile
    ]

m = {
  columns= 10,
  rows= 10,
  width= 20,
  height =20,
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
