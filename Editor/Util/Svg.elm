module Editor.Util.Svg (..) where

import WallpaperGroup.Pattern as Pattern
import WallpaperGroup.Group exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Editor.Types exposing (Point, MultiLine, Tile)
import String as String


lineToAttribute : Point -> List Svg.Attribute -> List Svg.Attribute
lineToAttribute { x, y } attributes =
    if List.length attributes == 0 then
        List.append [ x1 (toString x), y1 (toString y) ] attributes
    else
        List.append [ x2 (toString x), y2 (toString y) ] attributes


pointToString : Point -> String
pointToString p =
    (toString p.x) ++ " " ++ (toString p.y)


renderPath : MultiLine -> Svg
renderPath multiLine =
    let
        path = "M " ++ (List.map pointToString multiLine |> String.join (" L ")) ++ "z"
    in
        Svg.path
            [ d path
            , class "tile"
            ]
            []


renderPaths : Group -> Int -> Int -> Tile -> Svg
renderPaths group columns rows tile =
    Svg.g
        []
        (List.map (\t -> List.map renderPath t) (Pattern.pattern group columns rows tile)
            |> List.concat
        )


renderLine : MultiLine -> Svg
renderLine line =
    Svg.line (List.foldl lineToAttribute [] line) []


renderTile : Tile -> Svg
renderTile tile =
    Svg.g [ stroke "grey" ] (List.map renderLine tile)


renderTiles : Group -> Int -> Int -> Tile -> Svg
renderTiles group columns rows tile =
    Svg.g
        []
        (List.map renderTile (Pattern.pattern group columns rows tile))
