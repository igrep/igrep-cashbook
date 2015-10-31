module Main where

import IgrepCashbook.PathList

import Effects exposing (Never)
import Task exposing (Task)
import Signal

import StartApp

app = StartApp.start
  { init = IgrepCashbook.PathList.init
  , update = IgrepCashbook.PathList.update
  , view = IgrepCashbook.PathList.view
  , inputs = []
  }


main = app.html


port tasks : Signal (Task Never ())
port tasks = app.tasks
