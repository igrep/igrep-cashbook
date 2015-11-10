module IgrepCashbook.FileList
  ( Model
  , init
  , Action(..)
  , update
  , view
  , extractFromHtml
  , latestFileNameOf
  , collectCalculatedFiles
  , fromPaths
  ) where

import IgrepCashbook.File as File

import Dict exposing (Dict)
import List exposing (map, filterMap, head)
import Maybe
import Regex exposing (regex, find, HowMany(..))
import String
import Signal
import Task exposing (onError, succeed)

import Html exposing (..)
import Effects exposing (Effects, Never)
import Http

import Debug exposing (log, crash)


type alias FileList =
  { files : Dict String File.Model
  }


type alias Model = Result String FileList


init : Model
init = Err loading


type Action = ReplaceByData String | ParseAndSet String String


update : Action -> Model -> Model
update a m =
  case a of
    ReplaceByData data ->
      if String.isEmpty data
      then
        Err noDefaultPath
      else
        let fileNames = extractFromHtml data
            fileNameAndFiles =
              map (\fileName -> ( fileName, File.init fileName )) fileNames
        in
        Ok <| FileList <| Dict.fromList fileNameAndFiles
    ParseAndSet fileName data ->
      case m of
        Ok fileList ->
          Ok <|
            { fileList
            | files <-
              Dict.insert fileName (File.parse fileName data) fileList.files
            }
        Err e ->
          let _ = log "Assertion failure this should not be executed except in test. The Error was: " e
          in
              Ok <| { files = Dict.singleton fileName <| File.parse fileName data }


view : Model -> Html
view m =
  case m of
    Ok p ->
      ul [] <| map (li [] << singleton << text) <| Dict.keys p.files
    Err s ->
      div [] [text s]


{-| Get a list of IgrepCashbook file names from an HTML String returned by a file server.
  I usually write cashbooks on files named such as "15-09.txt", "15-10.txt" and so on.
  The "15" is the year when I buy the cashbook items.
  The "09" or "10" is the month when I buy the cashbook items.

  A cashbook file prfixed with "-" is for the coming month.
  For example, I write income item on "15-11-.txt" after getting salary at October, 2015.
  Then rename it into "15-11.txt" after it gets November, 2015.
-}
extractFromHtml : String -> List String
extractFromHtml =
    find All (regex "href=\"(\\d\\d-\\d\\d\\-?.txt)\"")
      >> filterMap ((.submatches) >> head >> unwrap)


latestFileNameOf : Model -> String
latestFileNameOf m =
  case m of
    Ok fileList ->
      fileList.files
        |> Dict.keys
        |> List.filter (not << String.endsWith "-.txt")
        |> List.maximum
        |> Maybe.withDefault ""
    Err _ ->
      ""


collectCalculatedFiles : Model -> List File.Model
collectCalculatedFiles m =
  case m of
    Ok fileList ->
      fileList.files |> Dict.values |> List.filter File.isCalculated
    Err _ ->
      []


-- for testing
fromPaths : List String -> Model
fromPaths =
  List.foldr (\s -> update <| ParseAndSet s "") init


unwrap : Maybe (Maybe a) -> Maybe a
unwrap m =
  case m of
    Just x -> x
    _ -> Nothing


singleton : a -> List a
-- singleton = (::[])
singleton x = [x]


noDefaultPath : String
noDefaultPath =
  """
  No default cashbook files found.
  Seems here isn't a cashbook directory and/or
  you haven't lanched a web server there.
  """


loading : String
loading = "Loading cashbook files..."
