module IgrepCashbook.PathList
  ( init
  , update
  , view
  , extractFromHtml
  ) where

import List exposing (map, filterMap, head)
import Maybe
import Regex exposing (regex, find, HowMany(..))
import Result
import String
import Signal
import Task exposing (onError, succeed)

import Html exposing (..)
import Effects exposing (Effects, Never)
import Http

import Debug exposing (log)


type alias PathList =
  { paths : List String
  }


type alias Model = Result String PathList


init : (Model, Effects Action)
init = ( Result.Err loading, initialFetch )


type Action = Replace (List String)


update : Action -> Model -> (Model, Effects Action)
update a m =
  case a of
    Replace ss ->
      if List.isEmpty ss
      then ( Result.Err noDefaultPath, Effects.none )
      else ( Result.Ok (PathList ss), Effects.none )


initialFetch : Effects Action
initialFetch =
  Http.getString "/"
      `onError` (\e -> let _ = log "ERROR" e in succeed "")
    |> Task.map (Replace << extractFromHtml)
    |> Effects.task


view : Signal.Address Action -> Model -> Html
view a m =
  case m of
    Result.Ok p ->
      ul [] <| map (li [] << singleton << text) p.paths
    Result.Err s ->
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
