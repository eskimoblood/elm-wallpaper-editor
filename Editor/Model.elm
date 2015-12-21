module Editor.Model where

import  WallpaperGroup.Group exposing (..)
import  WallpaperGroup.Geom.BoundingBox exposing (..)
import Editor.Types exposing (..)
import Editor.Util.Raster exposing (rasterCoords)
import WallpaperGroup.Pattern as Pattern


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
  }

type alias DrawingState =
  { lineStart : Point
  , lineEnd : Point
  , isDrawing : Bool
  , rasterCoords: List Point
  }

type alias Model =
  { patternState : PatternState
  , drawingState : DrawingState
  , undoStack: List PatternState
  , redoStack: List PatternState
  , seed: Int
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
  }

initialDrawingState : DrawingState
initialDrawingState =
  { lineStart = {x=0, y=0}
  , lineEnd = {x=0, y=0}
  , isDrawing = False
  , rasterCoords = rasterCoords 4 (Pattern.bounding (P1 150 150))
  }

initialModel : Model
initialModel =
  { patternState = initialPatternState
  , drawingState = initialDrawingState
  , seed = 0
  , undoStack = []
  , redoStack = []
  }
