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
  , errors       : Dict String (List Line.Wrong)
  }

type alias SubSummary =
  { breakdown : Dict String Int
  , subTotal  : Int
  }


init : Model
init =
  Model initSubSummary initSubSummary 0 Dict.empty


initSubSummary : SubSummary
initSubSummary =
  SubSummary Dict.empty 0


calculate : List IgrepCashbook.File.Model -> Model
calculate fs =
  List.foldl addFile init fs


addFile : IgrepCashbook.File.Model -> Model -> Model
addFile f m =
  List.foldr (addLine f.name) m f.lines


addLine : String -> Line.Model -> Model -> Model
addLine fileName l m =
  case l of
    Ok sl ->
      if sl.price >= 0 then
        { m | total = m.total + sl.price, incomes      = addLineToSubSummary sl m.incomes }
      else
        { m | total = m.total + sl.price, expenditures = addLineToSubSummary sl m.expenditures }
    Err wl ->
      { m | errors = Dict.update fileName (errorAppender wl) m.errors }


addLineToSubSummary : Line.Success -> SubSummary -> SubSummary
addLineToSubSummary l s =
  SubSummary
    (Dict.update l.group (priceAppender l.price) s.breakdown)
    (s.subTotal + l.price)


priceAppender : Int -> Maybe Int -> Maybe Int
priceAppender newPrice maybePrice =
  Just <| newPrice + (Maybe.withDefault 0 maybePrice)


errorAppender : Line.Wrong -> Maybe (List Line.Wrong) -> Maybe (List Line.Wrong)
errorAppender newError maybeErrors =
  Just <| newError :: (Maybe.withDefault [] maybeErrors)


view : Model -> Html
view m =
  div [] <|
    [ viewErrors m.errors
    , h1 [] [text <| "Total: " ++ toString m.total]
    , h1 [] [text "Expenditures"]
    , table [] (trsFromSubSummary m.expenditures)
    , h1 [] [text "Incomes"]
    , table [] (trsFromSubSummary m.incomes)
    ]


viewErrors : Dict String (List Line.Wrong) -> Html
viewErrors es =
  if Dict.isEmpty es then
    text ""
  else
    let fileNameAndWrongLines = Dict.toList es
    in
    div [] <|
      [ h1 [] [text "Errors"]
      , ul [] <| List.map (uncurry liErrorsOfFile) fileNameAndWrongLines
      ]


liErrorsOfFile : String -> List Line.Wrong -> Html
liErrorsOfFile fileName wls =
  li [] <|
    (text fileName)::[ol [] <| List.map Line.liWrong wls]


trsFromSubSummary : SubSummary -> List Html
trsFromSubSummary s =
  [trSubTotal s] ++ (Dict.toList s.breakdown |> List.map toTableRow)


trSubTotal : SubSummary -> Html
trSubTotal s =
  toTableRow ("Sub Total", s.subTotal)


toTableRow : (String, Int) -> Html
toTableRow (header, value) =
  tr [] [th [] [text header], td [] [text (toString <| abs value)]]


subTotal : SubSummary -> Int
subTotal =
  (.breakdown) >> Dict.values >> List.sum
