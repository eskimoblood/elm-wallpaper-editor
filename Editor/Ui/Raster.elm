module Editor.Ui.Raster where


import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Html exposing (..)
import Html.Attributes as Attr
import Json.Decode exposing (..)

import Editor.Types exposing (..)
import Editor.Action exposing (..)
import Editor.Model exposing (..)
import Editor.Util.Raster exposing (rasterCoords)

import WallpaperGroup.Geom.BoundingBox exposing (..)

renderPoint : Point -> Svg
renderPoint p =
  Svg.circle [cx (toString p.x), cy (toString p.y), r "1"][]


mousePosition : Decoder Point
mousePosition =
    object2 (\x y -> {x=x, y=y})
      ("offsetX" := Json.Decode.float)
      ("offsetY" := Json.Decode.float)


sendTo : Signal.Address Action -> (Point -> Action) -> Point -> Signal.Message
sendTo address action point =
  Signal.message address (action point)


preview : Model -> Svg.Svg
preview model =
  let
    childs =
      if model.isDrawing then
          [
            Svg.line [
              x1 (toString model.lineStart.x),
              y1 (toString model.lineStart.y),
              x2 (toString model.lineEnd.x),
              y2 (toString model.lineEnd.y),
              stroke "grey"
            ][]
          ]
      else
       []
  in
    g [] childs


raster : Model -> Signal.Address Action -> Html
raster model address=
  let
    sendAction = sendTo address
  in
    div
    [
      on "mousedown" mousePosition (sendAction LineStart),
      on "mousemove" mousePosition (sendAction LineMove),
      on "mouseup" mousePosition (sendAction LineEnd),
      Attr.style [
        ("margin", "10px")
      ]
    ][
      Svg.svg [
          version "1.1", x "0", y "0"
        ]
        [
          g [](List.map renderPoint model.rasterCoords),
          preview model
        ]
      ]
