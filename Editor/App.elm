module Editor.App where


{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

@docs main
-}

import StartApp.Simple exposing (start)
import Html exposing (..)

import Editor.View exposing (view)
import Editor.Action exposing (update)
import Editor.Model exposing (initialModel)

{-|-}
main : Signal.Signal Html.Html
main =
  start
    { model = initialModel
    , update = update
    , view = view
    }
