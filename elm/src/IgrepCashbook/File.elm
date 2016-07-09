module IgrepCashbook.File exposing
  ( init
  , parse
  , update
  , view
  , Model
  , Msg (SelectOrUnselect)
  )

import IgrepCashbook.Line as Line

import Html exposing (..)
import Html.Attributes exposing (type', checked)
import Html.Events as Events exposing (on)
import Json.Decode as Json
import List
import String


type alias Model =
  { name : String
  , lines : List Line.Model
  , isSelected: Bool
  }

type Msg = SelectOrUnselect Bool

init : String -> Model
init s = Model s [] False


parse : String -> String -> Model
parse fileName data = Model fileName (parseToLines data) True


update : Msg -> Model -> Model
update (SelectOrUnselect b) m = { m | isSelected = b }


parseToLines : String -> List Line.Model
parseToLines =
  String.split "\n" >> Line.parseList


view : Model -> List (Html Msg)
view m =
  [ input
      [ onChange <| SelectOrUnselect <| not m.isSelected
      , type' "checkbox"
      , checked m.isSelected
      ]
      []
  , text m.name
  ]


onChange : msg -> Attribute msg
onChange message =
  on "change" (Json.succeed message)
