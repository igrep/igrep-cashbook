module IgrepCashbook.UrlHandler
  exposing
    ( ParsedUrl
    , parse
    )


import Navigation
import String


type alias ParsedUrl =
  { paths : List String
  }


parse : Navigation.Location -> ParsedUrl
parse location =
  ParsedUrl <| List.filter (not << String.isEmpty) <| String.split "&" <| String.dropLeft 1 location.hash
