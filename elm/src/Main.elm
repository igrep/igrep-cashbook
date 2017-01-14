module Main exposing (..)

import IgrepCashbook.App

import Navigation


main : Program Never IgrepCashbook.App.Model IgrepCashbook.App.Msg
main =
  Navigation.program IgrepCashbook.App.ReplaceLocation
    { init = IgrepCashbook.App.initModelFromLocation
    , update = IgrepCashbook.App.update
    , view = IgrepCashbook.App.view
    , subscriptions = always Sub.none
    }
