module IgrepCashbook.App
  ( Model
  , init
  , update
  , view
  ) where

import IgrepCashbook.Summary as Summary
import IgrepCashbook.FileList as FileList

import Debug exposing (..)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Http
import String
import Task exposing (andThen, onError, succeed)
import TaskTutorial exposing (getCurrentTime)


type alias Model =
  { fileList : FileList.Model -- Loaded cashbook files
  , summary  : Summary.Model  -- Calculated cashbook summary by group
  }


type Action =
  FetchFileListData String
    | FetchCashbookData String String


init : (Model, Effects Action)
init =
  ( Model FileList.init Summary.init
  , initialFetch
  )


update : Action -> Model -> (Model, Effects Action)
update a m =
  case a of
    FetchFileListData s ->
      let m' =
        { m | fileList = FileList.replaceByData s m.fileList }
      in
      (m', fetchFile <| FileList.latestFileNameOf m'.fileList)
    FetchCashbookData fileName s ->
      let m' = { m | fileList = (FileList.parseAndSet fileName s) m.fileList }
          files = FileList.collectCalculatedFiles m'.fileList
      in
      ({ m' | summary = Summary.calculate files m'.summary }, Effects.none)


initialFetch : Effects Action
initialFetch =
  fetchFromPathToTask "/" FetchFileListData


fetchFile : String -> Effects Action
fetchFile fileName =
  if String.isEmpty fileName then
    let _ = log "Can't get a cashbook file. Isn't this a cashbook file directory?"
    in
        Effects.none
  else
    fetchFromPathToTask ("/" ++ fileName) (FetchCashbookData fileName)


fetchFromPathToTask : String -> (String -> Action) -> Effects Action
fetchFromPathToTask path dataToAction =
  let getData =
        getCurrentTime
          `andThen` \time ->
            Http.getString (path ++ "?_=" ++ toString time)
                `onError` (\e -> let _ = log "ERROR" e in succeed "")
  in
  Task.map dataToAction getData |> Effects.task


view : Signal.Address Action -> Model -> Html
view _ m =
  div [] <|
    [ FileList.view m.fileList
    , Summary.view m.summary
    ]
