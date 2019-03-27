{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Control.Monad         (forM_)
import           Data.Char             (isPrint)
import           Data.Either
import           Data.Monoid           ((<>))
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as Lazy
import           Text.Megaparsec

import qualified IgrepCashbook


main :: IO ()
main = hspec $ do
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

    it "given lines containing errors, reports the error" $ do
      let input = Lazy.unlines
            [ "15/07/25"
            , " 基本給  +200,000  "
            , "15/08/02"
            , "   216  交通費"
            , "15/08/03"
            , ""
            , "15/08/04"
            , " CD 売却  *10  その他"
            , " Zero  0  その他"
            , "15/08/08"
            , "# This is a comment"
            , " CD  -2_100  娯楽"
            ]
      IgrepCashbook.parseLines "test" input `shouldSatisfy` all isLeft

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
              ((Text.strip . Text.pack) <$> arbitrary)
                `suchThat` isIcText
            isIcText t =
              not (Text.null t)
                && not ("  " `Text.isInfixOf` t)
                && Text.all isPrint t
    prop "parses arbitrary cashbook entry separated by two spaces" $ forAll entryG $ \e -> do
      let el = IgrepCashbook.toEntryLine e
      parse IgrepCashbook.entryLine "test" el `shouldParse` e

    let mkCases(ex, expected) =
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
