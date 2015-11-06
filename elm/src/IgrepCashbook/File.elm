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
  , items : List Line.Model
  }


init : String -> Model
init s = Model s []


parse : String -> String -> Model
parse fileName data = Model fileName <| parseLines data


-- TODO: implement
parseLines : String -> List Line.Model
parseLines _ = []


isCalculated : Model -> Bool
isCalculated = (.items) >> (not << List.isEmpty)
