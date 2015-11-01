module IgrepCashbook.Line
  ( Model
  , SuccessLine
  , WrongLine
  , parse
  ) where

import String

type alias Model = Result WrongLine SuccessLine

type alias SuccessLine =
  { price : Int
  , group : String
  }

type alias WrongLine =
  { errorMessage : String
  , content : String
  }

parse : String -> Model
parse _ = Err
  { errorMessage = "Not Implemented"
  , content = ""
  }
