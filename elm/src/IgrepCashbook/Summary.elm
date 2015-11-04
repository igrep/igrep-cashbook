module IgrepCashbook.Summary
  ( Model
  , calculate
  , init
  , view
  ) where

import IgrepCashbook.File

import Dict exposing (Dict)

import Html exposing (..)

type alias Model =
  { expenditures    : Dict String Int
  , incomes         : Dict String Int
  }

-- TODO: implement
calculate : List IgrepCashbook.File.Model -> Model
calculate fs = init


init : Model
init = Model Dict.empty Dict.empty


view : Model -> Html
view m =
  div [] <|
    [ h1 [] [text "Expenditures"]
    , table [] (trsFromSummary m.expenditures)
    , h1 [] [text "Incomes"]
    , table [] (trsFromSummary m.incomes)
    ]


-- TODO: implement
trsFromSummary : Dict String Int -> List Html
trsFromSummary s = []
