module Editor.Ui.PatternStage (..) where

import Svg exposing (..)
import Html exposing (div)
import Svg.Attributes as Attr exposing (..)
import Editor.Types exposing (Tile, Point, MultiLine, Bezier)
import Editor.Util.Svg exposing (lineToAttribute, renderTiles, pointToString)
import String


renderLine : ( Float, MultiLine ) -> Svg
renderLine ( noise, line ) =
    Svg.line (List.foldl lineToAttribute [] line) []


renderPath : Bezier -> Svg
renderPath { p1, p2, c1, c2, opacity, color, strokeWidth } =
    let
        path =
            String.join
                " "
                [ "M"
                , (pointToString p1)
                , "C"
                , (pointToString c2)
                , (pointToString c1)
                , (pointToString p2)
                , "C"
                , (pointToString c1)
                , (pointToString c2)
                , (pointToString p1)
                , "Z"
                ]
    in
        Svg.path
            [ d path
            , Attr.strokeWidth (toString strokeWidth)
            , stroke color
            , Attr.opacity "1"
            , fill color
            , class "tile"
            ]
            []


renderTile : List Bezier -> Svg
renderTile tile =
    Svg.g [] (List.map renderPath tile)


renderColorizedNoisyTiles : List (List Bezier) -> Svg
renderColorizedNoisyTiles tiles =
    Svg.g
        [ id "stage" ]
        (List.map renderTile tiles)


getPatternAsString : List (List Bezier) -> String
getPatternAsString model =
    (a [] [ text "test" ])
        |> toString


stage : List (List Bezier) -> Svg
stage model =
    div
        []
        [ div
            [ id "patternSvg" ]
            [ svg
                [ version "1.1"
                , x "0"
                , y "0"
                , xmlns "http://www.w3.org/2000/svg"
                ]
                [ renderColorizedNoisyTiles model
                ]
            ]
        , svg
            [ id "filter-container" ]
            [ defs
                []
                [ Svg.filter
                    [ id "filter" ]
                    [ feImage
                        [ xlinkHref "#stage"
                        , x "0"
                        , y "-100"
                        , result "IMAGEFILL"
                        ]
                        []
                    , feTile
                        [ in' "IMAGEFILL"
                        , result "TILEPATTERN"
                        ]
                        []
                    , feFlood [ floodColor "#ffffff", result "BG-COLOR" ] []
                    , feMerge
                        [ result "BG-PATTERN" ]
                        [ feMergeNode [ in' "BG-COLOR" ] []
                        , feMergeNode [ in' "TILEPATTERN" ] []
                        ]
                    , feComposite
                        [ operator "in"
                        , in' "BG-PATTERN"
                        , in2 "SourceAlpha"
                        ]
                        []
                    ]
                ]
            ]
        ]
