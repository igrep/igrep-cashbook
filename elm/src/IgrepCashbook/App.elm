module IgrepCashbook.App exposing
  ( Model
  , Msg(ReplaceLocation)
  , initModelFromLocation
  , update
  , view
  )

import IgrepCashbook.Summary as Summary
import IgrepCashbook.FileList as FileList
import IgrepCashbook.UrlHandler as UrlHandler

import Html exposing (..)
import Http
import Navigation
import String
import Task exposing (andThen, onError)
import Time


type alias Model =
  { fileList : FileList.Model -- Loaded cashbook files
  , summary  : Summary.Model  -- Calculated cashbook summary by group
  }


type Msg =
  FetchFileListData String
    | FetchCashbookData String String
    | ModifyFileList FileList.Msg
    | ReplaceLocation Navigation.Location


init : Model
init = Model FileList.init Summary.init


initModelFromLocation : Navigation.Location -> (Model, Cmd Msg)
initModelFromLocation location =
  let paths = (UrlHandler.parse location).paths
  in
    if List.isEmpty paths then
      (init, initialFetch)
    else
      (init, fetchTxtFiles paths)


update : Msg -> Model -> (Model, Cmd Msg)
update a m1 =
  case a of
    FetchFileListData s ->
      let m2 =
        { m1 | fileList = FileList.replaceByData s m1.fileList }
      in
        (m2, fetchFile <| FileList.latestFileNameOf m2.fileList)
    FetchCashbookData fileName s ->
      let m2 = { m1 | fileList = (FileList.parseAndSet fileName s) m1.fileList }
      in
        (updateSummary m2, Cmd.none)
    ModifyFileList fileListAction ->
      let (fileList_, fileNameToFetch) = FileList.update fileListAction m1.fileList
          m2 = { m1 | fileList = fileList_ }
      in
        case fileNameToFetch of
          Just fileName -> (m2, fetchFile fileName)
          -- No fileNameToFetch means the file is unselected.
          _ -> (updateSummary m2, Cmd.none)
    ReplaceLocation location ->
      let paths = (UrlHandler.parse location).paths
      in
        if List.isEmpty paths then
          (m1, Cmd.none)
        else
          (init, fetchTxtFiles paths)


updateSummary : Model -> Model
updateSummary m =
  { m | summary = Summary.calculate <| FileList.collectSelected m.fileList }


initialFetch : Cmd Msg
initialFetch =
  fetchFromPathToCmd "/" FetchFileListData


fetchFile : String -> Cmd Msg
fetchFile fileName =
  if String.isEmpty fileName then
    let _ = Debug.log "Can't get a cashbook file. Isn't this a cashbook file directory?"
    in
      Cmd.none
  else
    fetchFromPathToCmd ("/" ++ fileName) (FetchCashbookData fileName)


fetchTxtFile : String -> Cmd Msg
fetchTxtFile fileName =
  fetchFile <| fileName ++ ".txt"


fetchTxtFiles : List String -> Cmd Msg
fetchTxtFiles fileNames =
  Cmd.batch <| List.map fetchTxtFile fileNames


fetchFromPathToCmd : String -> (String -> Msg) -> Cmd Msg
fetchFromPathToCmd path dataToAction =
  let getData =
        Time.now
          |> andThen (\time -> Http.toTask <| Http.getString (path ++ "?_=" ++ toString time))
          |> onError (\e -> Debug.crash <| "Assertion failure: " ++ toString e)
  in
    Task.perform dataToAction getData


view : Model -> Html Msg
view m =
  div [] <|
    [ Html.map ModifyFileList (FileList.view m.fileList)
    , Summary.view m.summary
    ]
