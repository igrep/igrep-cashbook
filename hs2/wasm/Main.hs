{-# LANGUAGE OverloadedStrings #-}

import           Asterius.Types        (JSString (JSString), JSVal (JSVal),
                                        fromJSString, toJSString)
import qualified Control.Foldl         as Foldl
import           Data.Functor.Identity (Identity (runIdentity))
import           Data.Monoid           ((<>))
import qualified Data.Text.Lazy        as TL

import qualified IgrepCashbook

foreign export javascript "sumIgrepCashbook" sumIgrepCashbook :: JSString -> JSString -> JSString

sumIgrepCashbook :: JSString -> JSString -> JSString
sumIgrepCashbook inPath inContents =
  let paths = [fromJSString inPath]
      readF _ = return . TL.pack . fromJSString $ inContents
      (err, out) =
        IgrepCashbook.summaryToConsoleOutput
          . runIdentity
          $ Foldl.foldM (IgrepCashbook.buildSummary readF) paths
  in
    toJSString $ TL.unpack (err <> "\n\n\n"  <> out)


main :: IO ()
main = return ()
