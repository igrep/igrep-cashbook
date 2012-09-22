module IgrepCashbook2
( CashbookLine(..)
, dateRegex
, priceRegex
, parseWithoutDate
)
where

-- for new style cashbook

import qualified IgrepCashbook as Old ( Item, isCommentLine, parseItemLine )
import Data.String.Utils (join)
import Text.Regex.Posix

data CashbookLine =
--  Comment String
--  | Item
  Item
    { getDate  :: Maybe String
    , getName :: String
    , getPrice :: Int
    , isIncome :: Bool
    , getGroup :: String }

parseWithoutDate :: String -> [ Either String CashbookLine ]
parseWithoutDate c = map parseLineWithoutDate nls'
  where
    ls = lines c
    ns = [1..]
    nls = zip ns ls
    nls' = filter (\(_, x) ->  isItemLine x ) nls

    parseLineWithoutDate :: (Int, String) -> Either String CashbookLine
    parseLineWithoutDate (n, x) = itemFromLine Nothing n x

dateRegex :: String
dateRegex = "^([01][0-9]/)?[0-9][0-9]/[0-9][0-9]$"

priceRegex :: String
priceRegex = "^\\+?[1-9][_,0-9]*$"

isItemLine :: String -> Bool
isItemLine x = not $ Old.isCommentLine x || isDateLine x

isDateLine :: String -> Bool
isDateLine x = x =~ dateRegex

parseItemLine :: String -> Old.Item
parseItemLine (' ':s) = Old.parseItemLine s
parseItemLine s = Old.parseItemLine s

emptyItem :: String
emptyItem = "invalid item: empty item. no data given"
noPriceAndGroup :: String
noPriceAndGroup = "invalid item: neither price nor group given"
noGroup :: String
noGroup = "invalid item: no group given"
invalidPrice :: String
invalidPrice = "invalid item: malformed price"

itemFromLine :: Maybe String -> Int -> String -> Either String CashbookLine
itemFromLine d n x = validate $ parseItemLine x
  where
  validate [] = Left emptyItem
  validate [name] = Left $ mkMsg noPriceAndGroup name
  validate [name, signedPrice] = Left $ mkMsg noGroup ( name ++ "  " ++ signedPrice )
  validate i@(name:signedPrice:group:_)
    | signedPrice =~ priceRegex = Right $ mkItem d name signedPrice group
    | otherwise = Left $ mkMsg invalidPrice $ join "  " i

  mkMsg :: String -> String -> String
  mkMsg e c = e ++ " \"" ++ c ++ "\" " ++ " at line " ++ show n

mkItem :: Maybe String -> String -> String -> String -> CashbookLine
mkItem d n s g = Item d n p ip g
  where
    p = mkPrice s
    ip = isIncomePrice s

    mkPrice :: String -> Int
    mkPrice x = read $ filter isNumberChar x
    isNumberChar x = x `elem` ['0'..'9']

isIncomePrice :: String -> Bool
isIncomePrice s = ( s !! 0 ) == '+'
