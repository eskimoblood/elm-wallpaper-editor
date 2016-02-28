module Editor.Util.Pattern (..) where

import Editor.Util.Noise as Noise
import Editor.Util.TileSize exposing (..)
import Editor.Types exposing (Tile, Point, MultiLine, Bezier)
import Editor.Model exposing (Model)
import WallpaperGroup.Pattern as Pattern
import Array


getColor : List String -> Float -> String
getColor colors noise =
    let
        i = floor ((toFloat (List.length colors)) * ((1 + noise) / 2))

        color =
            Array.fromList colors
                |> Array.get i
    in
        case color of
            Just cl ->
                cl

            Nothing ->
                "grey"


scalePoint : { a | groupType : String } -> Point -> Point
scalePoint {  groupType } p =
    { x = p.x / (getPreviewTileSize groupType) *  (getTileSize groupType)
    , y = p.y / (getPreviewTileSize groupType) *  (getTileSize groupType)
    }


getTileLength : List Tile -> Int
getTileLength tiles =
    List.length (Maybe.withDefault [] (List.head tiles))


randomPoint : ( Point, Point ) -> Float -> Int -> ( Point, Point )
randomPoint ( p1, p2 ) noise noiseDesctruction =
    let
        angle = noise * pi * 4

        rad = noise * (toFloat noiseDesctruction)
    in
        ( { x = p1.x + (cos angle) * rad, y = p1.y - (sin angle) * rad }
        , { x = p2.x - (cos angle) * rad, y = p2.y + (sin angle) * rad }
        )


calcPath : Int -> List String -> ( Float, MultiLine ) -> Bezier
calcPath noiseDesctruction colors ( noise, line ) =
    let
        p1 = Maybe.withDefault { x = 0, y = 0 } (List.head line)

        p2 = Maybe.withDefault { x = 0, y = 0 } (List.reverse line |> List.head)

        ( c1, c2 ) = randomPoint ( p1, p2 ) -noise noiseDesctruction

        color = getColor colors noise
    in
        { p1 = p1
        , p2 = p2
        , c1 = c1
        , c2 = c2
        , color = color
        , opacity = 1
        , strokeWidth = abs (sin noise) * 4
        }


calcTile : Int -> List String -> List ( Float, MultiLine ) -> List Bezier
calcTile noiseDesctruction colors tile =
    let
        lines = List.filter (\( s, _ ) -> (abs s) >= 0.1) tile
    in
        List.map (calcPath noiseDesctruction colors) lines


updatePatternInModel : Model -> Model
updatePatternInModel model =
    let
        patternState = model.patternState

        group = patternState.group

        columns = patternState.columns

        rows = patternState.rows

        tile = List.map (List.map (scalePoint patternState)) patternState.tile

        groups = Pattern.pattern group rows columns tile

        maxZ = getTileLength groups

        ( noise, seed ) = Noise.noise model maxZ

        noisyGroups = (List.map2 (List.map2 (,)) noise groups)

        noiseDesctruction = patternState.noiseDesctruction

        colors = model.colorState.selectedGradient

        pattern = List.map (calcTile noiseDesctruction colors) noisyGroups
    in
        { model | patternState = { patternState | pattern = pattern }, seed = seed }
