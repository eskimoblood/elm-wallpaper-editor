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

type alias Point = {x:Float, y:Float}
type alias Line = List Point
type alias Tile = List Line

type alias Model = {
  columns: Int,
  rows: Int,
  group: Group,
  tile: Tile
}

type Action
  = Columns Int
  | Rows Int


contentToValue : String -> Int
contentToValue str =
    let
      result = String.toInt str
    in
      case result of
        Ok v -> v
        Err e -> 0

update : Action -> Model -> Model
update action model =
  case action of
    Rows value ->
      {model | rows = value}

    Columns value ->
      {model | columns = value}

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [input [
      on "input" targetValue (\str -> Signal.message address (Columns(contentToValue str))),
      type' "range",
      Attr.max "10",
      Attr.min "1"
      ][],

      stage (P4 10 10) model.columns model.rows model.tile
    ]

m = {
  columns= 10,
  rows= 10,
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
