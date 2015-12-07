module Editor.Util.Convert where

import String


toInt : String -> Int
toInt str =
    let
      result = String.toInt str
    in
      case result of
        Ok v -> v
        Err e -> 0


toFloat : String -> Float
toFloat str =
    let
      result = String.toFloat str
    in
      case result of
        Ok v -> v
        Err e -> 0
