module Editor.Ui.Help (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Editor.Action exposing (..)
import Html.Events exposing (on, targetValue)


help : Bool -> Signal.Address Action -> List Html
help showHelp address =
    if showHelp then
        [ div
            [ class "modal" ]
            [ div
                []
                [ p
                    []
                    [ text "Pattern editor based on "
                    , a
                        [ href "https://en.wikipedia.org/wiki/Wallpaper_group"
                        , target "_blank"
                        ]
                        [ text "wallpaper groups" ]
                    , text ". The pattern are distorted with perlin noise, so its not possible to get predictable results. "
                    , text "Draw lines in the raster section of the pattern. Lines can be deleted by alt-click. "
                    ]
                , p
                    []
                    [ text "Its written in Elm, the code is on "
                    , a
                        [ href "https://github.com/eskimoblood/elm-wallpaper-editor"
                        , target "_blank"
                        ]
                        [ text "Github" ]
                    , text ". The color palettes search is powered by the "
                    , a
                        [ href "http://www.colourlovers.com/api"
                        , target "_blank"
                        ]
                        [ text "COLOURlovers API" ]
                    ]
                , button
                    [ on "click" targetValue (\_ -> Signal.message address (ToggleHelp False))
                    ]
                    [ text "Close" ]
                ]
            ]
        ]
    else
        []
