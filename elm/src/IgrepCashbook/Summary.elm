module IgrepCashbook.Summary
  ( Model
  , SubSummary
  , calculate
  , init
  , view
  ) where

import IgrepCashbook.File
import IgrepCashbook.Line as Line

import Dict exposing (Dict)
import Html exposing (..)
import List
import Maybe

type alias Model =
  { expenditures : SubSummary
  , incomes      : SubSummary
  , total        : Int
  , errors       : List Line.Wrong
  }

type alias SubSummary =
  { breakdown : Dict String Int
  , subTotal  : Int
  }


init : Model
init =
  Model initSubSummary initSubSummary 0 []


initSubSummary : SubSummary
initSubSummary =
  SubSummary Dict.empty 0


calculate : List IgrepCashbook.File.Model -> Model -> Model
calculate fs m =
  List.foldl addFile m fs


addFile : IgrepCashbook.File.Model -> Model -> Model
addFile f m =
  List.foldr addLine m f.items


addLine : Line.Model -> Model -> Model
addLine l m =
  case l of
    Ok sl ->
      if sl.price >= 0 then
        { m | total = m.total + sl.price, incomes      = addLineToSubSummary sl m.incomes }
      else
        { m | total = m.total + sl.price, expenditures = addLineToSubSummary sl m.expenditures }
    Err wl ->
      { m | errors = wl :: m.errors }


addLineToSubSummary : Line.Success -> SubSummary -> SubSummary
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
    , table [] (trsFromSubSummary m.expenditures)
    , h1 [] [text "Incomes"]
    , table [] (trsFromSubSummary m.incomes)
    , h1 [] [text <| "Total: " ++ toString m.total]
    ]


trsFromSubSummary : SubSummary -> List Html
trsFromSubSummary s =
  (Dict.toList s.breakdown |> List.map toTableRow) ++ [trSubTotal s]


trSubTotal : SubSummary -> Html
trSubTotal s =
  toTableRow ("Sub Total", s.subTotal)


toTableRow : (String, Int) -> Html
toTableRow (header, value) =
  tr [] [th [] [text header], td [] [text (toString <| abs value)]]


subTotal : SubSummary -> Int
subTotal =
  (.breakdown) >> Dict.values >> List.sum
