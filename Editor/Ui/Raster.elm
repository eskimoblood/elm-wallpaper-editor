module Editor.Ui.Raster (..) where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Html exposing (..)
import Html.Attributes as Attr
import Json.Decode exposing (..)
import Editor.Types exposing (..)
import Editor.Action exposing (..)
import Editor.Model exposing (..)
import Editor.Util.Svg exposing (renderTiles, renderTile, renderLine, renderPaths)
import WallpaperGroup.Geom.BoundingBox exposing (..)
import WallpaperGroup.Group exposing (..)
import String

renderPoint : Point -> Svg
renderPoint p =
    Svg.circle
        [ cx (toString p.x)
        , cy (toString p.y)
        , r ".7"
        ]
        []


bounding : BoundingBox -> Svg
bounding boundingBox =
    case boundingBox of
        Rect p1 p2 p3 p4 ->
            g
                [ class "single-tile" ]
                [ renderLine [ p1, p2 ]
                , renderLine [ p2, p3 ]
                , renderLine [ p3, p4 ]
                , renderLine [ p4, p1 ]
                ]

        Triangle p1 p2 p3 ->
            g
                [ class "single-tile" ]
                [ renderLine [ p1, p2 ]
                , renderLine [ p2, p3 ]
                , renderLine [ p3, p1 ]
                ]


boundingBoxAsPath : BoundingBox -> Tile
boundingBoxAsPath boundingBox =
    case boundingBox of
        Rect p1 p2 p3 p4 ->
            [ [ p1, p2, p3, p4 ] ]

        Triangle p1 p2 p3 ->
            [ [ p1, p2, p3 ] ]


mousePosition : Decoder ( Point, Bool )
mousePosition =
    object3
        (\x y altKeyPressed -> ( { x = x, y = y }, altKeyPressed ))
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


onMouseMove : Signal.Address Action -> ( Point, Bool )  -> Signal.Message
onMouseMove address   mouseData =
    sendTo address LineMove (fst mouseData)


onMouseDown :  Signal.Address Action -> ( Point, Bool ) -> Signal.Message
onMouseDown address mouseData =
    if (snd mouseData) then
        sendTo address DeleteLine (fst mouseData)
    else
        sendTo address LineStart (fst mouseData)

onMouseUp :  Signal.Address Action -> ( Point, Bool ) -> Signal.Message
onMouseUp address mouseData =
    if (snd mouseData) then
        Signal.message address NoOp
    else
        sendTo address LineEnd (fst mouseData)


raster : DrawingState -> Tile -> Group -> BoundingBox -> Signal.Address Action -> Html
raster model tile group boundingBox address =
        div
            [ on "mousedown" mousePosition (onMouseDown address)
            , on "mousemove" mousePosition (onMouseMove address )
            , on "mouseup" mousePosition (onMouseUp address)
            , Attr.class "drawingArea"
            ]
            [ Svg.svg
                [ version "1.1"
                , x "0"
                , y "0"
                , class ("preview " ++ (toString group)|>String.toLower)
                ]
                [ renderPaths group 1 1 (boundingBoxAsPath boundingBox)
                , renderTiles group 1 1 tile
                , Svg.g [] (List.map renderPoint model.rasterCoords)
                , preview model
                , renderTile tile
                , bounding boundingBox
                ]
            ]
