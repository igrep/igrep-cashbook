module Main exposing (..)

import IgrepCashbook.App
import IgrepCashbook.UrlHandler

import Navigation


main : Program Never
main =
  Navigation.program
    IgrepCashbook.UrlHandler.parser
    { init = IgrepCashbook.App.init
    , update = IgrepCashbook.App.update
    , urlUpdate = IgrepCashbook.App.urlUpdate
    , view = IgrepCashbook.App.view
    , subscriptions = always Sub.none
    }
