{-# LANGUAGE OverloadedStrings #-}

import qualified Test.FileSystem.Fake  as Fake
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Control.Foldl         as Foldl
import           Control.Monad         (forM_)
import           Data.Char             (isPrint)
import           Data.Either
import qualified Data.Map.Strict       as M
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as Lazy
import           Text.Megaparsec

import qualified IgrepCashbook


main :: IO ()
main = hspec $ do
  describe "IgrepCashbook.buildSummary" $
    it "sums up cashbook files" $ do
      let files = M.fromList
            [ ( "1.txt"
              , Lazy.unlines
                [ "15/07/25"
                , " 基本給  +200,000  給料"
                , "15/08/02"
                , " バス  216  交通費"
                , "15/08/03"
                , " 本  2,880  勉強"
                , "15/08/04"
                , " CD 売却  +10  その他"
                , "15/08/08"
                , "# This is a comment"
                , " CD  2_100  娯楽"
                ]
              )
            , ("2.txt"
              , Lazy.unlines
                [ "19/02/25"
                , " 基本給  +240,000  給料"
                , " 通信料  2000  その他  # comment"
                , "19/03/01"
                , " カレー  950  外食"
                , " バス 216  交通費"
                , "19/03/03"
                , " 本 売却  +250  その他"
                ]
              )
            ]
          action =
            IgrepCashbook.summaryToConsoleOutput
              <$> Foldl.foldM (IgrepCashbook.buildSummary Fake.readPath) (M.keys files)
          expectedErr =
            "[WARNING] 2.txt:6:1:10:\n  |\n1 |  \12496\12473 216  \20132\36890\36027\n  |          ^\nunexpected '\20132'\nexpecting '+', Amount expression, or whitespace\n"
          expectedOut = Lazy.unlines
            [ "## EXPENDITURES ##"
            , "勉強\t2880"
            , "娯楽\t2100"
            , "その他\t2000"
            , "外食\t950"
            , "交通費\t216"
            , "小計\t8146"
            , ""
            , "#### INCOMES ####"
            , "給料\t440000"
            , "その他\t260"
            , "小計\t440260"
            , ""
            , "合計\t432114"
            ]
      Fake.evalFileSystemM files action `shouldBe` Right (expectedErr, expectedOut)
  describe "IgrepCashbook.parseLines" $ do
    it "parses date lines, comments, and entry lines" $ do
      let input = Lazy.unlines
            [ "15/07/25"
            , " 基本給  +200,000  給料"
            , "15/08/02"
            , " バス  216  交通費"
            , "15/08/03"
            , " 本  2,880  勉強"
            , "15/08/04"
            , " CD 売却  +10  その他"
            , "15/08/08"
            , "# This is a comment"
            , " CD  2_100  娯楽"
            ]
          expected = map Right
            [ IgrepCashbook.Entry True 200000  "給料"
            , IgrepCashbook.Entry False 216  "交通費"
            , IgrepCashbook.Entry False 2880  "勉強"
            , IgrepCashbook.Entry True 10  "その他"
            , IgrepCashbook.Entry False 2100  "娯楽"
            ]
      IgrepCashbook.parseLines "test" input `shouldBe` expected

    let errLines =
          [ " 基本給  +200,000  "
          , "   216  交通費"
          , " CD 売却  *10  その他"
          , " Zero  0  その他"
          , " CD  -2_100  娯楽"
          , " CD 2_100  娯楽"
          ]
        ignoredLines =
          [ "15/07/25"
          , "15/08/02"
          , "15/08/03"
          , "15/08/04"
          , "15/08/08"
          , "# This is a comment"
          ]
        gen = do
          els <- sublistOf errLines
          ils <- sublistOf ignoredLines
          mixedLines <- shuffle $ els ++ ils
          pure (els, mixedLines)
     in prop "given lines containing errors, reports errors as many as the actual number of error lines"
        . forAll gen $ \(els, mixedLines) -> do
          let input = Lazy.unlines $ map Lazy.pack mixedLines
              actual = IgrepCashbook.parseLines "test" input
          actual `shouldSatisfy` all isLeft
          length actual `shouldBe` length els

    it "given an empty lines, returns empty entries" $
      IgrepCashbook.parseLines "test" "" `shouldBe` []

  describe "IgrepCashbook.entryLine" $ do

    let entryG =
          IgrepCashbook.Entry
            <$> arbitrary
            <*> (arbitrary `suchThat` (> 0))
            <*> icTextG
          where
            icTextG =
              (Text.strip . Text.pack <$> arbitrary)
                `suchThat` isIcText
            isIcText t =
              not (Text.null t)
                && not ("  " `Text.isInfixOf` t)
                && Text.all isPrint t
    prop "parses arbitrary cashbook entry separated by two spaces" $ forAll entryG $ \e -> do
      let el = IgrepCashbook.toEntryLine e
      parse IgrepCashbook.entryLine "test" el `shouldParse` e

    let mkCases (ex, expected) =
          [ (" item*  " <> ex <> "  group", mkEx expected)
          , (" item*+  " <> "+(" <> ex <> ")" <> "  income", mkIn expected)
          , (" itemNoSpace*  " <> expNoSpace <> "  group", mkEx expected)
          , (" itemNoSpace*+  " <> "+(" <> expNoSpace <> ")" <> "  income", mkIn expected)
          ]
          where
            expNoSpace = Lazy.replace " " "" ex
            mkEx a = IgrepCashbook.Entry False a "group"
            mkIn a = IgrepCashbook.Entry True a "income"
        cases = zip [(1 :: Int)..] $ concatMap mkCases
          [ ("1_0 + 2_0 + 3,0", 60)
          , ("1 + 2 * 3", 7)
          , ("(1 + 2) * 3", 9)
          , ("2 * 4 + 3", 11)
          , ("2 * (4 + 0)", 8)
          , ("10_1 * 2 * 3", 606)
          , ("6 / 3 * 1_05", 210)
          ]
    forM_ cases $ \(i, (input, expected)) ->
      it ("parses cashbook entries containing an expression " ++ show input ) $
        parse IgrepCashbook.entryLine (show i) input `shouldParse` expected
