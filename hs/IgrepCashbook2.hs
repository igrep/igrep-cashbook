module IgrepCashbook2
( CashbookLine
, Item
, isComment
, isItemLine
, isDateLine
, dateRegex
, parseItemLine
, priceRegex
, validateItem
, getName
, getSignedPrice
, getPrice
, getGroup
, isIncomePrice
)
where

-- for new style cashbook

import qualified IgrepCashbook as Old
import Text.Regex.Posix

data CashbookLine =
  Comment String
  | Item
    { name :: String
    , price :: Int
    , group :: String
    , date  :: Maybe String }

parseWithoutDate :: String -> ( Int, Either String CashbookLine )
parseWithoutDate c = 
  map ( \(n, l) -> ( n, validateItem $ parseItemLine l ) ) is
  where
    ls = lines c
    ns = [ 1..( length ls ) ]
    is = selectItemLine ns ls

isComment :: Item -> Bool
isComment = Old.isComment

isItemLine :: String -> Bool
isItemLine x = not $ isCommentLine x || isDateLine x

isDateLine :: String -> Bool
isDateLine x = x =~ dateRegex
isDateLine _ = False

dateRegex :: String
dateRegex = "^[01][0-9]/[0-9][0-9]/[0-9][0-9]$"

parseItemLine :: String -> Item
parseItemLine (' ':s) = Old.parseLine s
parseItemLine s = Old.parseLine s

emptyItem :: String
emptyItem = "invalid item: empty item. no data given"
noPriceAndGroup :: String
noPriceAndGroup = "invalid item: neither price nor group given"
noGroup :: String
noGroup = "invalid item: no group given"
invalidPrice :: String
invalidPrice = "invalid item: malformed price"

priceRegex :: String
priceRegex = "^\\+?[1-9][_,0-9]*$"

validateItem :: Item -> Either String Item
validateItem [] = Left emptyItem
validateItem [name] = Left $ noPriceAndGroup ++ show name
validateItem i@[_name, _signedPrice] = Left $ noGroup ++ show i
validateItem i@(_name:signedPrice:_group:_)
  | signedPrice =~ priceRegex = Right i
  | otherwise = Left $ invalidPrice ++ show i

getName :: Item -> String
getName = ( !! 0)

getSignedPrice :: Item -> String
getSignedPrice = ( !! 1)

getPrice :: Item -> Int
getPrice i = read $ filter isNumberChar $ getSignedPrice i
  where
    isNumberChar x = x `elem` ['0'..'9']

getGroup :: Item -> String
getGroup = ( !! 2)

isIncomePrice :: String -> Bool
isIncomePrice s = ( s !! 0 ) == '+'
