module IgrepCashbook.File
  ( init
  -- , update
  , parse
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


parse : String -> List Line.Model
parse _ = []
