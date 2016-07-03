module Main exposing (..)

import IgrepCashbook.App

import Effects exposing (Never)
import Html exposing (Html)
import Task exposing (Task)
import Signal

import StartApp


app : StartApp.App IgrepCashbook.App.Model
app = StartApp.start
  { init = IgrepCashbook.App.init
  , update = IgrepCashbook.App.update
  , view = IgrepCashbook.App.view
  , inputs = []
  }


main : Signal Html
main = app.html


port tasks : Signal (Task Never ())
port tasks = app.tasks
