module Editor.Action (..) where

import Editor.Model exposing (..)
import Editor.Types exposing (..)
import Editor.Util.Geom exposing (..)
import Editor.Util.TileSize exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import Editor.Util.Pattern exposing (updatePatternInModel)
import Editor.Util.History exposing (..)
import Editor.Util.Color exposing (..)
import WallpaperGroup.Group exposing (..)
import Effects exposing (Effects, none)
import WallpaperGroup.Pattern as Pattern
import Random
import Array
import Effects exposing (..)
import Editor.Signals exposing (requestPalette)
import Task


type Action
    = NoOp
    | Columns Int
    | Rows Int
    | NoiseX Int
    | NoiseY Int
    | NoiseZ Int
    | NoiseDesctruction Int
    | Group String
    | RasterSize Float
    | LineStart Point
    | LineMove Point
    | LineEnd Point
    | DeleteLine Point
    | ClearTiles
    | Random
    | Undo
    | Redo
    | StartColorSearch String
    | NewColors (Result String (List (List String)))
    | SelectPalette (List String)
    | TogglePallete Bool
    | ClosePallete
    | UpadtePattern


getGroup : String -> Float -> Float -> Group
getGroup groupType height width =
    if groupType == "P1" then
        P1 width height
    else if groupType == "P2" then
        P2 width height
    else if groupType == "Pm" then
        Pm width height
    else if groupType == "Pg" then
        Pg width height
    else if groupType == "Cm" then
        Cm width height
    else if groupType == "P2mm" then
        P2mm width height
    else if groupType == "P2mg" then
        P2mg width height
    else if groupType == "P2gg" then
        P2gg width height
    else if groupType == "C2mm" then
        C2mm width height
    else if groupType == "P4" then
        P4 width height
    else if groupType == "P4mm" then
        P4mm width height
    else if groupType == "P4mg" then
        P4mg width height
    else if groupType == "P3" then
        P3 width
    else if groupType == "P3m1" then
        P3m1 width
    else if groupType == "P31m" then
        P31m width
    else if groupType == "P6" then
        P6 width
    else
        P1 width height


getRandom : Random.Seed -> Int -> Int -> ( Int, Random.Seed )
getRandom seed min max =
    let
        generator = Random.int min max
    in
        Random.generate generator seed


getRandomCoord : List Point -> Random.Seed -> Int -> MultiLine
getRandomCoord points seed i =
    let
        seed' = Random.initialSeed (i + (fst (getRandom seed Random.minInt Random.maxInt)))

        ( i1, seed'' ) = getRandom seed' 0 (List.length points - 1)

        ( i2, _ ) = getRandom seed'' 0 (List.length points - 1)

        getValue item =
            case item of
                Just i ->
                    i

                Nothing ->
                    { x = 0, y = 0 }
    in
        [ points
            |> Array.fromList
            |> Array.get i1
            |> getValue
        , points
            |> Array.fromList
            |> Array.get i2
            |> getValue
        ]


update : Action -> Model -> ( Model, Effects Action )
update action model =
    let
        patternState = model.patternState

        drawingState = model.drawingState

        colorState = model.colorState
    in
        case action of
            NoOp ->
                ( model, Effects.none )

            Rows value ->
                let
                    model = addHistory model
                in
                    ( updatePatternInModel { model | patternState = { patternState | rows = value } }, Effects.none )

            Columns value ->
                let
                    model = addHistory model
                in
                    ( updatePatternInModel { model | patternState = { patternState | columns = value } }, Effects.none )

            NoiseX value ->
                let
                    model = addHistory model
                in
                    ( updatePatternInModel { model | patternState = { patternState | noiseX = value } }, Effects.none )

            NoiseY value ->
                let
                    model = addHistory model
                in
                    ( updatePatternInModel { model | patternState = { patternState | noiseY = value } }, Effects.none )

            NoiseZ value ->
                let
                    model = addHistory model
                in
                    ( updatePatternInModel { model | patternState = { patternState | noiseZ = value } }, Effects.none )

            NoiseDesctruction value ->
                let
                    model = addHistory model
                in
                    ( updatePatternInModel { model | patternState = { patternState | noiseDesctruction = value } }, Effects.none )

            Group value ->
                let
                    model = addHistory model

                    previewGroupSize = getPreviewTileSize value
                    groupSize = getTileSize value

                    previewGroup = getGroup value previewGroupSize previewGroupSize

                    boundingBox = Pattern.bounding (previewGroup)
                in
                    ( updatePatternInModel
                        { model
                            | patternState =
                                { patternState
                                    | group = getGroup value groupSize groupSize
                                    , previewGroup = previewGroup
                                    , groupType = value
                                    , boundingBox = boundingBox
                                }
                            , drawingState =
                                { drawingState
                                    | rasterCoords = rasterCoords patternState.rasterSize (Pattern.bounding (getGroup value previewGroupSize previewGroupSize))
                                }
                        }
                    , Effects.none
                    )

            RasterSize value ->
                let
                    model = addHistory model

                    previewGroupSize = getPreviewTileSize model.patternState.groupType
                in
                    ( updatePatternInModel
                        { model
                            | patternState =
                                { patternState
                                    | rasterSize = value
                                    , tile = []
                                }
                            , drawingState =
                                { drawingState
                                    | rasterCoords = rasterCoords value (Pattern.bounding (getGroup patternState.groupType previewGroupSize previewGroupSize))
                                }
                        }
                    , Effects.none
                    )

            LineStart mousePosition ->
                ( { model
                    | drawingState =
                        { drawingState
                            | lineStart = snap drawingState.rasterCoords mousePosition
                            , lineEnd = snap drawingState.rasterCoords mousePosition
                            , isDrawing = True
                        }
                  }
                , Effects.none
                )

            LineMove mousePosition ->
                if drawingState.isDrawing then
                    ( { model
                        | drawingState =
                            { drawingState
                                | lineEnd = snap drawingState.rasterCoords mousePosition
                            }
                      }
                    , Effects.none
                    )
                else
                    ( model, Effects.none )

            LineEnd mousePosition ->
                if drawingState.isDrawing then
                    let
                        model = addHistory model

                        tile = [ drawingState.lineStart, drawingState.lineEnd ] :: patternState.tile
                    in
                        ( updatePatternInModel
                            { model
                                | drawingState = { drawingState | isDrawing = False }
                                , patternState = { patternState | tile = tile }
                            }
                        , Effects.none
                        )
                else
                    ( model, Effects.none )

            DeleteLine mousePosition ->
                let
                    model = addHistory model

                    tile = List.filter (lineIsNearPoint mousePosition 5) patternState.tile
                in
                    ( updatePatternInModel
                        { model
                            | patternState = { patternState | tile = tile }
                        }
                    , Effects.none
                    )

            ClearTiles ->
                let
                    model = addHistory model
                in
                    ( { model
                        | patternState = { patternState | tile = [], noise = [], pattern = [] }
                      }
                    , Effects.none
                    )

            Random ->
                let
                    model = addHistory model

                    ( i, seed' ) = getRandom model.seed 3 10

                    tile = List.map (getRandomCoord drawingState.rasterCoords seed') [1..i]
                in
                    ( updatePatternInModel
                        { model
                            | patternState = { patternState | tile = tile }
                            , seed = seed'
                        }
                    , Effects.none
                    )

            Undo ->
                ( (undo model), Effects.none )

            Redo ->
                ( (redo model), Effects.none )

            StartColorSearch str ->
                let
                    model = addHistory model

                    sendTask =
                        Signal.send requestPalette.address str
                            `Task.andThen` (\_ -> Task.succeed NoOp)
                            |> Effects.task
                in
                    ( { model
                        | colorState = { colorState | colorSearch = str, loading = True }
                      }
                    , sendTask
                    )

            NewColors colors ->
                case colors of
                    Ok c ->
                        ( { model
                            | colorState = { colorState | palettes = c, loading = False }
                          }
                        , Effects.none
                        )

                    Err e ->
                        ( { model
                            | colorState = { colorState | palettes = [], loading = False }
                          }
                        , Effects.none
                        )

            SelectPalette palette ->
                let
                    model = addHistory model

                    p = toGradient palette
                in
                    ( updatePatternInModel
                        { model
                            | colorState = { colorState | selectedGradient = p, selectedPalette = palette }
                        }
                    , Effects.none
                    )

            TogglePallete shown ->
                if shown then
                    ( { model
                        | colorState = { colorState | paletteOpen = shown }
                      }
                    , Effects.none
                    )
                else
                    ( model
                    , Effects.task (Task.sleep 300 `Task.andThen` \_ -> Task.succeed ClosePallete)
                    )

            ClosePallete ->
                ( { model
                    | colorState = { colorState | paletteOpen = False }
                  }
                , Effects.none
                )

            UpadtePattern ->
                ( updatePatternInModel model
                , Effects.none
                )
