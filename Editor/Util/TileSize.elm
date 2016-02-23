module Editor.Util.TileSize (..) where

import Regex exposing (..)


getPreviewTileSize : String -> Float
getPreviewTileSize groupType =
    if (contains (regex "(P6)|(P31m)") groupType) then
        200
    else if (contains (regex "P3") groupType) then
        172
    else
        150


getTileSize : String -> Float
getTileSize groupType =
    if (contains (regex "(P6)|(P31m)|(Cm)") groupType) then
        70
    else if (contains (regex "P3") groupType) then
        50  
    else
        40
