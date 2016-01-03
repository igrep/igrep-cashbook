module IgrepCashbook.File
  ( init
  , parse
  , update
  , view
  , Model
  , Action (SelectOrUnselect)
  ) where

import IgrepCashbook.Line as Line

import Html exposing (..)
import Html.Attributes exposing (type', checked)
import Html.Events as Events exposing (on)
import Json.Decode as Json exposing (value)
import List
import Signal exposing (Address)
import String


type alias Model =
  { name : String
  , lines : List Line.Model
  , isSelected: Bool
  }

type Action = SelectOrUnselect Bool

init : String -> Model
init s = Model s [] False


parse : String -> String -> Model
parse fileName data = Model fileName (parseToLines data) True


update : Action -> Model -> Model
update (SelectOrUnselect b) m = { m | isSelected = b }


parseToLines : String -> List Line.Model
parseToLines =
  String.split "\n" >> Line.parseList


view : Address Action -> Model -> List Html
view a m =
  [ input
      [ onChange a <| SelectOrUnselect <| not m.isSelected
      , type' "checkbox"
      , checked m.isSelected
      ]
      []
  , text m.name
  ]


onChange : Signal.Address a -> a -> Attribute
onChange =
  messageOn "change"


-- Copied from Html.Events
messageOn : String -> Signal.Address a -> a -> Attribute
messageOn name addr msg =
  on name value (\_ -> Signal.message addr msg)
