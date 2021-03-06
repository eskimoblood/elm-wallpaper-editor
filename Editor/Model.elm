module Editor.Model (..) where

import WallpaperGroup.Group exposing (..)
import WallpaperGroup.Geom.BoundingBox exposing (..)
import Editor.Types exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import WallpaperGroup.Pattern as Pattern
import Random


type alias PatternState =
    { columns : Int
    , rows : Int
    , noiseX : Int
    , noiseY : Int
    , noiseZ : Int
    , noiseDesctruction : Int
    , group : Group
    , previewGroup : Group
    , groupType : String
    , boundingBox : BoundingBox
    , rasterSize : Float
    , tile : Tile
    , noise : List (List Float)
    , pattern : List (List Bezier)
    }


type alias DrawingState =
    { lineStart : Point
    , lineEnd : Point
    , isDrawing : Bool
    , rasterCoords : List Point
    }


type alias ColorState =
    { colorSearch : String
    , palettes : List (List String)
    , selectedPalette : List String
    , selectedGradient : List String
    , loading : Bool
    , paletteOpen : Bool
    }


type alias Model =
    { patternState : PatternState
    , drawingState : DrawingState
    , colorState : ColorState
    , undoStack : List PatternState
    , redoStack : List PatternState
    , seed : Random.Seed
    , showHelp : Bool
    , patternAsURI : String
    }


initialPatternState : PatternState
initialPatternState =
    { columns = 10
    , rows = 10
    , noiseX = 10
    , noiseY = 10
    , noiseZ = 10
    , noiseDesctruction = 0
    , groupType = "P3m1"
    , rasterSize = 4
    , boundingBox = Pattern.bounding (P3m1 172 )
    , tile = []
    , group = P3m1 50
    , previewGroup = P3m1 172
    , noise = []
    , pattern = []
    }


initialDrawingState : DrawingState
initialDrawingState =
    { lineStart = { x = 0, y = 0 }
    , lineEnd = { x = 0, y = 0 }
    , isDrawing = False
    , rasterCoords = rasterCoords 4 (Pattern.bounding (P3m1 172))
    }


initialColorState : ColorState
initialColorState =
    { colorSearch = ""
    , palettes = []
    , selectedPalette = []
    , selectedGradient = []
    , loading = False
    , paletteOpen = False
    }


initialModel : Model
initialModel =
    { patternState = initialPatternState
    , drawingState = initialDrawingState
    , colorState = initialColorState
    , seed = Random.initialSeed 31415
    , undoStack = []
    , redoStack = []
    , showHelp = False
    ,patternAsURI ="#"
    }
