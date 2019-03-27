module Main where

import qualified Control.Foldl      as Foldl
import qualified Data.Text.Lazy.IO  as Lazy
import           System.Environment (getArgs)
import           System.IO          (stderr)

import qualified IgrepCashbook


main :: IO ()
main = do
  (err, out) <-
    IgrepCashbook.summaryToConsoleOutput
      <$> (Foldl.foldM (IgrepCashbook.buildSummary Lazy.readFile) =<< getArgs)
  Lazy.putStr out
  Lazy.hPutStr stderr err
