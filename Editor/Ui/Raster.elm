module Editor.Ui.Raster where

import Editor.Util.Raster exposing (rasterCoords)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Editor.Types exposing (..)
import WallpaperGroup.Geom.BoundingBox exposing (..)
import Mouse as Mouse
import Debug exposing (log)
renderPoint : Point -> Svg
renderPoint p =
  Svg.circle [cx (toString p.x), cy (toString p.y), r "1"][]


type alias Model = {
  start: (Int, Int),
  end: (Int, Int),
  draw: Bool
}

initialModel : Model
initialModel =
  {
    start= (0, 0),
    end= (0, 0),
    draw= False
  }

type Action
  = Start
  | Move
  | End

update : (Action,  (Int, Int))  -> Model -> Model
update (action, p) model =
  let
    l = log "model" "model"
  in
    case action of
      Start -> {model |   start = p,   draw= True }
      Move -> {model | end = p}
      End -> {model | draw= False}

draw = Signal.mailbox End

stream : Signal (Action,  (Int, Int))
stream = Signal.map2 (,) draw.signal Mouse.position

view = Signal.foldp update initialModel stream


raster : BoundingBox -> Float -> Svg
raster bounding steps =
  let
    points = rasterCoords steps bounding
    a =  log "number" 1
  in
    svg [
        version "1.1", x "0", y "0"
      ]
      [
        g [

        onMouseDown  (Signal.message draw.address Start),
        onMouseMove (Signal.message draw.address Move),
        onMouseUp (Signal.message   draw.address End)
        ](List.map renderPoint points)
      ]
