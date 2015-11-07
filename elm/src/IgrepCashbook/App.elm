module IgrepCashbook.App
  ( init
  , update
  , view
  ) where

import IgrepCashbook.Summary as Summary
import IgrepCashbook.FileList as FileList

import Debug exposing (..)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Http
import Task exposing (onError, succeed)


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
      ( { m
          | fileList <- FileList.update ( FileList.ReplaceByData s ) m.fileList
        }
      , fetchFile <| FileList.latestFileNameOf m.fileList
      )
    FetchCashbookData fileName s ->
      ( { m
          | fileList <- FileList.update ( FileList.ParseAndSet fileName s ) m.fileList
          , summary <- Summary.calculate <| FileList.collectCalculatedFiles m.fileList
        }
      , Effects.none
      )


-- TODO: Refactor
initialFetch : Effects Action
initialFetch =
  Http.getString "/"
      `onError` (\e -> let _ = log "ERROR" e in succeed "")
    |> Task.map FetchFileListData
    |> Effects.task


-- TODO: Refactor
fetchFile : String -> Effects Action
fetchFile fileName =
  Http.getString ( "/" ++ fileName )
      `onError` (\e -> let _ = log "ERROR" e in succeed "")
    |> Task.map (FetchCashbookData fileName)
    |> Effects.task


view : Signal.Address Action -> Model -> Html
view _ m =
  div [] <|
    [ FileList.view m.fileList
    , Summary.view m.summary
    ]