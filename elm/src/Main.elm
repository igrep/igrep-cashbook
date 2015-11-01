module Main where

import IgrepCashbook.FileList

import Effects exposing (Never)
import Task exposing (Task)
import Signal

import StartApp

app = StartApp.start
  { init = IgrepCashbook.FileList.init
  , update = IgrepCashbook.FileList.update
  , view = IgrepCashbook.FileList.view
  , inputs = []
  }


main = app.html


port tasks : Signal (Task Never ())
port tasks = app.tasks
