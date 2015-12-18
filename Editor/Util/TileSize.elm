module Editor.Util.TileSize where

import Regex exposing (..)


getTileSize : String -> Float
getTileSize groupType =
    if (contains (regex "(P6)|(P31m)") groupType) then
      133
    else if (contains (regex "P3") groupType) then
      115
    else
      100
