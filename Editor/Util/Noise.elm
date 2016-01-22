module Editor.Util.Noise where

import Noise exposing (..)
import Editor.Model exposing(Model)
import Random


noise : Model -> Int-> (List (List Float), Random.Seed)
noise model z =
  let
    maxX = toFloat model.patternState.columns
    maxY = toFloat model.patternState.rows
    maxZ = toFloat z
    noiseX = toFloat model.patternState.noiseX
    noiseY = toFloat model.patternState.noiseY
    noiseZ = toFloat model.patternState.noiseZ
    seed =  model.seed
  in
    if (maxX == 0 || maxY == 0 || maxZ == 0 ) then
      ([], seed)
    else
      let
        (perm, newSeed) = permutationTable model.seed
        list = List.foldr (
          \x r -> List.foldr (
            \y r -> (List.foldr (
              \z r -> (noise3d  perm (x /noiseX) (y/noiseY) (z/noiseZ)) :: r
            ) [] [1..maxZ]) :: r
          ) r [1..maxY]
        ) [] [1..maxX]
      in
       (list, newSeed)
