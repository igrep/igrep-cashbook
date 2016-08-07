module IgrepCashbook.UrlHandler
  exposing
    ( ParsedUrl
    , parser
    , pathList
    )


import Navigation
import String


type ParsedUrl =
  ParsedUrl (List String)


parser : Navigation.Parser ParsedUrl
parser =
  Navigation.makeParser parse


parse : Navigation.Location -> ParsedUrl
parse location =
  ParsedUrl <| String.split "&" <| String.dropLeft 1 location.hash


pathList : ParsedUrl -> List String
pathList (ParsedUrl paths) =
  paths
