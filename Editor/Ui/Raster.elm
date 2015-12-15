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
import Editor.Util.Svg exposing (renderTile)
import Debug exposing (log)
import WallpaperGroup.Geom.BoundingBox exposing (..)

renderPoint : Point -> Svg
renderPoint p =
  Svg.circle
    [ cx (toString p.x)
    , cy (toString p.y), r "1"
    ]
    []



mousePosition : Decoder (Point, Bool)
mousePosition =
    object3 (\x y altKeyPressed -> ({x=x, y=y}, altKeyPressed))
      ("offsetX" := Json.Decode.float)
      ("offsetY" := Json.Decode.float)
      ("altKey" := Json.Decode.bool)


sendTo : Signal.Address Action -> (Point -> Action) -> Point -> Signal.Message
sendTo address action point =
  Signal.message address (action point)


preview : DrawingState -> Svg.Svg
preview model =
  let
    childs =
      if model.isDrawing then
          [ Svg.line
            [ x1 (toString model.lineStart.x)
            , y1 (toString model.lineStart.y)
            , x2 (toString model.lineEnd.x)
            , y2 (toString model.lineEnd.y)
            , stroke "grey"
            ]
            []
          ]
      else
       []
  in
    g [] childs

sendMousePosition : ((Point -> Action) -> Point -> Signal.Message) -> (Point -> Action) -> (Point, Bool) -> Signal.Message
sendMousePosition sendAction action mouseData =
  sendAction action (fst mouseData)

sendMousePositionOrDelete : ((Point -> Action) -> Point -> Signal.Message)  -> (Point, Bool) -> Signal.Message
sendMousePositionOrDelete sendAction  mouseData =
  let
    mouseData = log "m" mouseData
  in
    if (snd mouseData) then
      sendAction DeleteLine (fst mouseData)
    else
      sendAction LineStart (fst mouseData)

raster : DrawingState -> Tile -> Signal.Address Action -> Html
raster model tile address=
  let
    sendAction = sendTo address
  in
    div
    [ on "mousedown" mousePosition (sendMousePositionOrDelete sendAction)
    , on "mousemove" mousePosition (sendMousePosition sendAction LineMove)
    , on "mouseup" mousePosition (sendMousePosition sendAction LineEnd)
    , Attr.class "drawingArea"
    ]
    [ Svg.svg
        [ version "1.1", x "0", y "0"
        ]
        [ g [](List.map renderPoint model.rasterCoords)
        , preview model
        , Svg.g [stroke "red"] (renderTile tile)
        ]
      ]
