module IgrepCashbook.FileList exposing
  ( Model
  , Msg (..)
  , init
  , replaceByData
  , parseAndSet
  , update
  , view
  , extractFromHtml
  , latestFileNameOf
  , collectSelected
  , fromPaths
  )

import IgrepCashbook.File as File

import Dict exposing (Dict)
import List exposing (filterMap, head)
import Maybe
import Regex exposing (regex, find, HowMany(..))
import String

import Html exposing (..)
import Html.App as Html


type alias FileList =
  { files : Dict String File.Model
  }


type alias Model = Result String FileList

type Msg = ModifyFile String File.Msg


init : Model
init = Err loading


replaceByData : String -> Model -> Model
replaceByData data m =
  if String.isEmpty data then
    Err noDefaultPath
  else
    let fileNames = extractFromHtml data
        fileNameAndFiles =
          List.map (\fileName -> (fileName, File.init fileName)) fileNames
    in
    Ok <| FileList <| Dict.fromList fileNameAndFiles


parseAndSet : String -> String -> Model -> Model
parseAndSet fileName data m =
  case m of
    Ok fileList ->
      Ok <|
        { fileList
        | files =
            Dict.insert fileName (File.parse fileName data) fileList.files
        }
    Err e ->
      let _ = logAssertionFailure e
      in
          Ok <| { files = Dict.singleton fileName <| File.parse fileName data }


update : Msg -> Model -> (Model, Maybe String)
update (ModifyFile fileName fileAction) m =
  case m of
    Ok fileList ->
      let files' =
            Dict.update fileName (fileUpdater fileName fileAction) fileList.files
          (File.SelectOrUnselect isSelected) = fileAction
      in
        ( Ok <| { files = files' }
        , if isSelected then Just fileName else Nothing
        )
    Err e ->
      let _ = logAssertionFailure e
      in
        (m, Nothing)


fileUpdater : String -> File.Msg -> Maybe File.Model -> Maybe File.Model
fileUpdater fileName fileAction =
  Just << File.update fileAction << Maybe.withDefault (File.init fileName)


view : Model -> Html Msg
view m =
  case m of
    Ok p ->
      let files = Dict.values p.files
      in
          if List.isEmpty files then
            div [] [text "No cashbook files found. Isn't this a cashbook file directory?"]
          else
              ul [] <| List.map (\f -> li [] <| List.map (Html.map (ModifyFile f.name)) (File.view f)) files
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


collectSelected : Model -> List File.Model
collectSelected m =
  case m of
    Ok fileList ->
      fileList.files |> Dict.values |> List.filter (.isSelected)
    Err _ ->
      []


-- for testing
fromPaths : List String -> Model
fromPaths =
  List.foldr (\s -> parseAndSet s "") init


unwrap : Maybe (Maybe a) -> Maybe a
unwrap m =
  case m of
    Just x -> x
    _ -> Nothing


singleton : a -> List a
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


logAssertionFailure : String -> String
logAssertionFailure e =
  Debug.log "Assertion failure this should not be executed except in test. The Error was: " e
