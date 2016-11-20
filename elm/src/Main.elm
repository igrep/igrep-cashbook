module Main exposing (..)

import IgrepCashbook.App

import Html


main : Program Never IgrepCashbook.App.Model IgrepCashbook.App.Msg
main =
  Html.program
  { init = IgrepCashbook.App.init
  , update = IgrepCashbook.App.update
  , view = IgrepCashbook.App.view
  , subscriptions = always Sub.none
  }
