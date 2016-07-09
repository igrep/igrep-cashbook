module Main exposing (..)

import IgrepCashbook.App

import Html.App as Html


main : Program Never
main =
  Html.program
  { init = IgrepCashbook.App.init
  , update = IgrepCashbook.App.update
  , view = IgrepCashbook.App.view
  , subscriptions = always Sub.none
  }
