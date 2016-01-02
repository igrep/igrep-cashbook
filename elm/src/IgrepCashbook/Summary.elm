module IgrepCashbook.Summary
  ( Model
  , SubSummary
  , calculate
  , init
  , view
  ) where

import IgrepCashbook.File
import IgrepCashbook.Line

import Dict exposing (Dict)
import Html exposing (..)
import List
import Maybe
import Result.Extra

type alias Model =
  { expenditures : SubSummary
  , incomes      : SubSummary
  , total        : Int
  }

type alias SubSummary =
  { breakdown : Dict String Int
  , subTotal  : Int
  }


init : Model
init =
  Model initSubSummary initSubSummary 0


initSubSummary : SubSummary
initSubSummary =
  SubSummary Dict.empty 0


calculate : List IgrepCashbook.File.Model -> Model -> Model
calculate fs m =
  List.foldl addFile m fs


addFile : IgrepCashbook.File.Model -> Model -> Model
addFile f m =
  f.items
    |> Result.Extra.partitionList
    |> snd
    |> List.foldl addLine m


addLine : IgrepCashbook.Line.SuccessLine -> Model -> Model
addLine l m =
  if l.price >= 0 then
    { m | total = m.total + l.price, incomes      = addLineToSubSummary l m.incomes }
  else
    { m | total = m.total + l.price, expenditures = addLineToSubSummary l m.expenditures }


addLineToSubSummary : IgrepCashbook.Line.SuccessLine -> SubSummary -> SubSummary
addLineToSubSummary l s =
  SubSummary
    (Dict.update l.group (priceAppender l.price) s.breakdown)
    (s.subTotal + l.price)


priceAppender : Int -> Maybe Int -> Maybe Int
priceAppender newPrice maybePrice =
  Just <| newPrice + (Maybe.withDefault 0 maybePrice)


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
