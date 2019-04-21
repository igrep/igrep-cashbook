module Main where

import qualified Control.Foldl      as Foldl
import qualified Data.Text.Lazy     as Lazy
import qualified Data.Text.Lazy.IO  as Lazy
import           System.Environment (getArgs)
import           System.IO          (IOMode (ReadMode), hSetEncoding, openFile,
                                     stderr, utf8)

import qualified IgrepCashbook


main :: IO ()
main = do
  (err, out) <-
    IgrepCashbook.summaryToConsoleOutput
      <$> (Foldl.foldM (IgrepCashbook.buildSummary readUtf8File) =<< getArgs)
  Lazy.putStr out
  Lazy.hPutStr stderr err


readUtf8File :: FilePath -> IO Lazy.Text
readUtf8File path = do
  hd <- openFile path ReadMode
  hSetEncoding hd utf8
  Lazy.hGetContents hd
