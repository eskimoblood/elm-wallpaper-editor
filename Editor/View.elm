module Editor.View where

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, targetValue)

import Editor.Ui.Slider exposing (slider)
import Editor.Ui.GroupSelect exposing (groupSelect)
import Editor.Ui.Raster exposing (raster)

import Editor.Util.Convert as Convert
import Editor.Ui.PatternStage exposing (stage)
import Editor.Model exposing (Model)
import Editor.Action exposing (..)
import Editor.Types exposing (..)


view : Signal.Address Action -> Model -> Html
view address model =
  div [Attr.class "row", Attr.id "String"]
    [
      div [Attr.class "sidebar"][
        slider {min= "1", max= "10", address= address, createAction= \str -> RasterSize(Convert.toFloat str)},
        slider {min= "1", max= "10", address= address, createAction= \str -> Columns(Convert.toInt str)},
        slider {min= "1", max= "10", address= address, createAction= \str -> Rows(Convert.toInt str)},
        groupSelect address,
        raster model address,
        button [
          on "click" targetValue (\_ -> Signal.message address ClearTiles)
        ][Html.text "Clear"],
        button [
          on "click" targetValue (\_ -> Signal.message address Random)
        ][Html.text "Random"]
      ],
      div[ Attr.class "main lalasd"][
        stage model
      ]

    ]
