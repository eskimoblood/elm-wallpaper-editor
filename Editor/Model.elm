module Editor.Model where

import  WallpaperGroup.Group exposing (..)
import  WallpaperGroup.Geom.BoundingBox exposing (..)
import Editor.Types exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import WallpaperGroup.Pattern as Pattern
import Random

type alias PatternState =
  { columns : Int
  , rows : Int
  , width : Float
  , height : Float
  , group : Group
  , previewGroup : Group
  , groupType : String
  , boundingBox : BoundingBox
  , rasterSize : Float
  , tile : Tile
  , noise : List(List Float)
  }

type alias DrawingState =
  { lineStart : Point
  , lineEnd : Point
  , isDrawing : Bool
  , rasterCoords: List Point
  }

type alias ColorState =
  { colorSearch : String
  , palettes : List (List String)
  , selectedPalette :  List String
  , loading : Bool
  }

type alias Model =
  { patternState : PatternState
  , drawingState : DrawingState
  , colorState : ColorState
  , undoStack: List PatternState
  , redoStack: List PatternState
  , seed: Random.Seed
  }

initialPatternState : PatternState
initialPatternState =
  { columns = 10
  , rows = 10
  , width = 40
  , height = 40
  , groupType = "P1"
  , rasterSize = 4
  , boundingBox = Pattern.bounding (P1 150 150)
  , tile = []
  , group = P1 40 40
  , previewGroup = P1 150 150
  , noise = []
  }

initialDrawingState : DrawingState
initialDrawingState =
  { lineStart = {x=0, y=0}
  , lineEnd = {x=0, y=0}
  , isDrawing = False
  , rasterCoords = rasterCoords 4 (Pattern.bounding (P1 150 150))
  }

initialColorState : ColorState
initialColorState =
  { colorSearch = ""
  , palettes = []
  , selectedPalette = []
  , loading = True
  }

initialModel : Model
initialModel =
  { patternState = initialPatternState
  , drawingState = initialDrawingState
  , colorState = initialColorState
  , seed = Random.initialSeed 31415
  , undoStack = []
  , redoStack = []
  }
