module Editor.Types where


type alias Point = {x:Float, y:Float}
type alias MultiLine = List Point
type alias Line = {start: Point, end: Point}
type alias Tile = List MultiLine
type alias Bezier = {p1: Point, c1: Point, c2: Point, p2: Point, color: String, opacity: Float}
