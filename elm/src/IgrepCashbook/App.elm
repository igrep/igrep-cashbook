module IgrepCashbook.App exposing
  ( Model
  , init
  , update
  , view
  )

import IgrepCashbook.Summary as Summary
import IgrepCashbook.FileList as FileList

import Html exposing (..)
import Html.App as Html
import Http
import String
import Task exposing (andThen)
import Time


type alias Model =
  { fileList : FileList.Model -- Loaded cashbook files
  , summary  : Summary.Model  -- Calculated cashbook summary by group
  }


type Msg =
  FetchFileListData String
    | FetchCashbookData String String
    | ModifyFileList FileList.Msg


init : (Model, Cmd Msg)
init =
  ( Model FileList.init Summary.init
  , initialFetch
  )


update : Msg -> Model -> (Model, Cmd Msg)
update a m =
  case a of
    FetchFileListData s ->
      let m' =
        { m | fileList = FileList.replaceByData s m.fileList }
      in
      (m', fetchFile <| FileList.latestFileNameOf m'.fileList)
    FetchCashbookData fileName s ->
      let m' = { m | fileList = (FileList.parseAndSet fileName s) m.fileList }
      in
          (updateSummary m', Cmd.none)
    ModifyFileList fileListAction ->
      let (fileList', fileNameToFetch) = FileList.update fileListAction m.fileList
          m' = { m | fileList = fileList' }
      in
          case fileNameToFetch of
            Just fileName -> (m', fetchFile fileName)
            -- No fileNameToFetch means the file is unselected.
            _ -> (updateSummary m', Cmd.none)


updateSummary : Model -> Model
updateSummary m =
  { m | summary = Summary.calculate <| FileList.collectSelected m.fileList }


initialFetch : Cmd Msg
initialFetch =
  fetchFromPathToTask "/" FetchFileListData


fetchFile : String -> Cmd Msg
fetchFile fileName =
  if String.isEmpty fileName then
    let _ = Debug.log "Can't get a cashbook file. Isn't this a cashbook file directory?"
    in
      Cmd.none
  else
    fetchFromPathToTask ("/" ++ fileName) (FetchCashbookData fileName)


fetchFromPathToTask : String -> (String -> Msg) -> Cmd Msg
fetchFromPathToTask path dataToAction =
  let getData =
        Time.now `andThen` \time ->
          Http.getString (path ++ "?_=" ++ toString time)
  in
    Task.perform (\e -> Debug.crash <| "Assertion failure: " ++ toString e) dataToAction getData


view : Model -> Html Msg
view m =
  div [] <|
    [ Html.map ModifyFileList (FileList.view m.fileList)
    , Summary.view m.summary
    ]
