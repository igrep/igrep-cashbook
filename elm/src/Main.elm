module Main exposing (..)

import IgrepCashbook.App

import Html.App as Html
import Task exposing (Task)


-- TODO: see main's type main : Signal Html
main =
  Html.program
  { init = IgrepCashbook.App.init
  , update = IgrepCashbook.App.update
  , view = IgrepCashbook.App.view
  , subscriptions = always Sub.none
  }
