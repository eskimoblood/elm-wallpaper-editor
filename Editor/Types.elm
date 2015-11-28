module Editor.Types where


type alias Point = {x:Float, y:Float}
type alias MultiLine = List Point
type alias Line = {start: Point, end: Point}
type alias Tile = List MultiLine
