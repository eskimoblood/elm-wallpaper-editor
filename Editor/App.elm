module Editor.App (main, app) where

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

@docs main

@docs app

@docs app
-}

--

import StartApp exposing (start)
import Html exposing (..)
import Effects exposing (none)
import Editor.View exposing (view)
import Editor.Action exposing (..)
import Editor.Signals exposing (requestPaletteFilter)
import Editor.Model exposing (Model, initialModel)
import Signal
import Task
import Json.Decode as Json exposing ((:=), list, string, decodeValue)
import Effects exposing (Never)


{-| -}
app : StartApp.App Model
app =
    start
        { init = ( initialModel, Effects.none )
        , update = update
        , view = view
        , inputs = [ responseColor ]
        }

--


{-| -}
main : Signal.Signal Html.Html
main =
    app.html


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks


port request : Signal String
port request =
    requestPaletteFilter


port responseColors : Signal Json.Value
responseColor : Signal Action
responseColor =
    Signal.map ((decodeValue palettes) >> NewColors) responseColors


palettes : Json.Decoder (List (List String))
palettes =
    list (("colors" := list string))
