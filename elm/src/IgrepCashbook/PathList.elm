module IgrepCashbook.PathList
  ( extractFromHtml
  ) where

import Regex exposing (regex, find, HowMany(..))
import List exposing (map, filter, head)
import String
import Maybe

import Debug exposing (log)

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
      >> map ((.submatches) >> head >> unwrap >> Maybe.withDefault "")
      >> filter (not << String.isEmpty)

unwrap : Maybe (Maybe a) -> Maybe a
unwrap m =
  case m of
    Just x -> x
    _ -> Nothing
