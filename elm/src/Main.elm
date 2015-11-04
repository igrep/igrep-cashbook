module Main where

import IgrepCashbook.App

import Effects exposing (Never)
import Task exposing (Task)
import Signal

import StartApp


app = StartApp.start
  { init = IgrepCashbook.App.init
  , update = IgrepCashbook.App.update
  , view = IgrepCashbook.App.view
  , inputs = []
  }


main = app.html


port tasks : Signal (Task Never ())
port tasks = app.tasks
