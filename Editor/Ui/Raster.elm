module Editor.Ui.Raster where

import Editor.Util.Raster exposing (rasterCoords)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Html.Events exposing (on, targetValue)
import Html exposing (..)
import Html.Attributes as Attr
import Json.Decode exposing (..)

import Editor.Types exposing (..)
import Editor.Action exposing (..)
import WallpaperGroup.Geom.BoundingBox exposing (..)
import Mouse as Mouse
import Debug exposing (log)
renderPoint : Point -> Svg
renderPoint p =
  Svg.circle [cx (toString p.x), cy (toString p.y), r "1"][]

mousePosition : Decoder (Float,Float)
mousePosition =
    object2 (,)
      ("offsetX" := Json.Decode.float)
      ("offsetY" := Json.Decode.float)


raster : BoundingBox -> Float -> Signal.Address Action -> Html
raster bounding steps address=
  let
    points = rasterCoords steps bounding
  in
    div
    [
      on "mousedown" mousePosition (\c -> Signal.message address (LineStart c)),
      on "mousemove" mousePosition (\c -> Signal.message address (LineMove c)),
      on "mouseup" mousePosition (\c -> Signal.message address (LineEnd c))
    ][
      Svg.svg [
          version "1.1", x "0", y "0"
        ]
        [
          g [
          ](List.map renderPoint points)
        ]
      ]
