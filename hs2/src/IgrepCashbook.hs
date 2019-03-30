{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module IgrepCashbook
  ( Entry(..)
  , buildSummary
  , summaryToConsoleOutput
  , parseLines
  , entryLine
  , toEntryLine
  ) where


import           Control.Applicative            (many, optional, (<|>))
import           Control.Arrow                  ((>>>))
import           Control.Foldl                  (FoldM (FoldM))
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import           Data.Char                      (isDigit)
import           Data.Either                    (partitionEithers)
import           Data.Foldable                  (foldMap)
import           Data.Functor                   (($>))
import           Data.List                      (partition, sortOn)
import           Data.Map.Monoidal.Strict       (MonoidalMap)
import qualified Data.Map.Monoidal.Strict       as MonoidalMap
import           Data.Maybe                     (mapMaybe)
import           Data.Monoid                    (Sum (..), (<>))
import           Data.Ord                       (Down (Down))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy                 as Lazy
import qualified Data.Text.Lazy.Builder         as TextBuilder
import qualified Data.Text.Lazy.Read            as Lazy
import           Data.Void                      (Void)
import           Text.Megaparsec                (ParseErrorBundle, Parsec,
                                                 anySingleBut,
                                                 errorBundlePretty, label,
                                                 noneOf, notFollowedBy, oneOf,
                                                 parse, skipMany, skipSome,
                                                 takeWhile1P, try, (<?>))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L


data Summary =
  Summary
    { summaryExpenditures :: !SubSummary
    , summaryIncomes      :: !SubSummary
    , summaryErrors       :: ![LineError]
    } deriving (Eq, Show)

newtype SubSummary =
  SubSummary
    { subSummaryBreakdown :: MonoidalMap Text (Sum Int)
    } deriving (Eq, Show, Semigroup, Monoid)

data Entry =
  Entry
    { entryIsIncome :: !Bool
    , entryAmount   :: !Int
    , entryGroup    :: !Text
    } deriving (Eq, Show)

type CashbookLine = Maybe Entry

type Parser = Parsec Void Lazy.Text

type LineError = ParseErrorBundle Lazy.Text Void


buildSummary :: Monad m => (FilePath -> m Lazy.Text) -> FoldM m FilePath Summary
buildSummary readF = FoldM step initial return
  where
    step s path = do
      (errors, entries) <- partitionEithers . parseLines path <$> readF path
      let (incomes, expenditures) = partition entryIsIncome entries
      return $ Summary
        (summaryExpenditures s <> subSummaryFromEntries expenditures)
        (summaryIncomes s <> subSummaryFromEntries incomes)
        (summaryErrors s <> errors)
    initial = return $ Summary mempty mempty mempty


summaryToConsoleOutput :: Summary -> (Lazy.Text, Lazy.Text)
summaryToConsoleOutput s = (err, out)
  where
    err = TextBuilder.toLazyText $ foldMap bErr $ summaryErrors s
    bErr e = "[WARNING] " <> TextBuilder.fromString (errorBundlePretty e)
    out = TextBuilder.toLazyText $
      "## EXPENDITURES ##\n"
        <> subSummaryToTextBuilder (summaryExpenditures s)
        <> "\n#### INCOMES ####\n"
        <> subSummaryToTextBuilder (summaryIncomes s)
        <> "\n"
        <> buildSummaryLine "合計" (summaryTotal s)


buildSummaryLine :: TextBuilder.Builder -> Int -> TextBuilder.Builder
buildSummaryLine header val =
  header <> "\t" <> TextBuilder.fromString (show val) <> "\n"


subSummaryToTextBuilder :: SubSummary -> TextBuilder.Builder
subSummaryToTextBuilder s =
  foldMap buildSummaryLine' (sortOn (Down . snd) $ MonoidalMap.assocs $ subSummaryBreakdown s)
    <> buildSummaryLine "小計" (subSummarySubTotal s)
  where
    buildSummaryLine' (h, Sum v) =
      buildSummaryLine (TextBuilder.fromText h) v


subSummaryFromEntries :: [Entry] -> SubSummary
subSummaryFromEntries = foldMap f
  where
    f Entry {..} =
      SubSummary $ MonoidalMap.singleton entryGroup $ Sum entryAmount


subSummarySubTotal :: SubSummary -> Int
subSummarySubTotal =
  getSum . sum . MonoidalMap.elems . subSummaryBreakdown


summaryTotal :: Summary -> Int
summaryTotal Summary {..} =
  subSummarySubTotal summaryIncomes - subSummarySubTotal summaryExpenditures


parseLines :: String -> Lazy.Text -> [Either LineError Entry]
parseLines sn =
  Lazy.lines
    >>> zipWith (\i -> parse cashbookLine (sn ++ " / " ++ show i)) [(1 :: Int)..]
    >>> mapMaybe f
  where
    f (Left e)         = Just $ Left e
    f (Right (Just e)) = Just $ Right e
    f (Right Nothing)  = Nothing


cashbookLine :: Parser CashbookLine
cashbookLine = try dateLine <|> try comment <|> (Just <$> entryLine)


dateLine :: Parser CashbookLine
dateLine = do
  spacesInLine
  skipSome digitChar
  _ <- char '/'
  skipSome digitChar
  _ <- char '/'
  skipSome digitChar
  pure Nothing


comment :: Parser CashbookLine
comment = do
  spacesInLine
  _ <- char '#'
  restOfLine
  pure Nothing


-- | Public for testing
entryLine :: Parser Entry
entryLine = do
  _ <- char ' '
  _ <- icText
  separator2
  entryIsIncome <- isIncome
  entryAmount <- validAmount
  -- NOTE: validAmount can consume one trailing space
  separator1
  entryGroup <- icText
  restOfLine
  pure Entry {..}
  where
    isIncome =
      (char '+' $> True) <|> pure False

    validAmount :: Parser Int
    validAmount = do
      a <- expr
      if a > 0
        then return a
        else fail $ "Amount must be positive: " ++ show a
      where
        -- Most of the code in this where block is copied from the sample in
        -- https://hackage.haskell.org/package/megaparsec-6.4.0/docs/Text-Megaparsec-Expr.html

        expr :: Parser Int
        expr = makeExprParser term table <?> "Amount expression"

        term :: Parser Int
        term = parens expr <|> lexeme amount <?> "Amount term"

        amount :: Parser Int
        amount =
          either fail (pure . fst) . Lazy.decimal . Lazy.filter isDigit
            =<< takeWhile1P (Just "digit") isIcDigit

        table :: [[Operator Parser Int]]
        table =
          [ [binary "*" (*), binary "/" div]
          , [binary "+" (+), binary "-" (-)]
          ]

        binary
          :: Lazy.Text
          -> (Int -> Int -> Int)
          -> Operator Parser Int
        binary n f = InfixL (f <$ symbol n)

        parens :: Parser a -> Parser a
        parens p = char '(' *> p <* symbol ")"

        symbol :: Lazy.Text -> Parser Lazy.Text
        symbol = L.symbol exprSpace

        lexeme :: Parser a -> Parser a
        lexeme = L.lexeme exprSpace

        exprSpace :: Parser ()
        exprSpace = void $ optional $ char ' '

    separator1 =
      void $ takeWhile1P (Just "whitespace") (== ' ')

    separator2 =
      char ' ' *> separator1


-- | IC Text: Igrep Cashbook Text.
icText :: Parser Text
icText = label "Igrep Cashbook Text" $ do
  first <- nonBlank
  left <- many (nonBlank <|> try (char ' ' <* notFollowedBy (char ' ')))
  pure $ Text.pack $ first : left
  where
    nonBlank = noneOf (" \t\v\f\r\n" :: String)


-- | Returns True if the character is one of valid characters of
--   IC Digit (igrep cashbook digit) characters.
isIcDigit :: Char -> Bool
isIcDigit c = isDigit c || c == '_' || c == ','


spacesInLine :: Parser ()
spacesInLine = skipMany $ oneOf (" \t\v\f\x3000" :: String)


restOfLine :: Parser ()
restOfLine = skipMany $ anySingleBut '\n'


-- | Only for testing
toEntryLine :: Entry -> Lazy.Text
toEntryLine e =
  " Testing Item  "
    <> sign <> Lazy.pack (show $ entryAmount e) <> "  "
    <> Lazy.fromStrict (entryGroup e) <> "  "
    <> "After group"
  where
    sign = if entryIsIncome e then "+" else ""
