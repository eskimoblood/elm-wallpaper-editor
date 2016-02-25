module Editor.Util.Groups (..) where

import WallpaperGroup.Group exposing (..)


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
