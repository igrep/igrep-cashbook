module IgrepCashbook.File
  ( init
  -- , update
  , parse
  , isCalculated
  , Model
  ) where

import IgrepCashbook.Line as Line

import String
import List


type alias Model =
  { name : String
  , lines : List Line.Model
  }


init : String -> Model
init s = Model s []


parse : String -> String -> Model
parse fileName = Model fileName << parseToLines


parseToLines : String -> List Line.Model
parseToLines =
  String.split "\n" >> Line.parseList


isCalculated : Model -> Bool
isCalculated = (.lines) >> (not << List.isEmpty)
