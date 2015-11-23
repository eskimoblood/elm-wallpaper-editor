module Editor.Action where

import Editor.Model exposing (Model)


type Action
  = Columns Int
  | Rows Int
  | Width Float
  | Height Float

update : Action -> Model -> Model
update action model =
  case action of
    Rows value ->
      {model | rows = value}

    Columns value ->
      {model | columns = value}

    Width value ->
      {model | width = value}

    Height value ->
      {model | height = value}
