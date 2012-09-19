module IgrepCashbook2
( CashbookLine
, Comment
, Item
, dateRegex
, priceRegex
, parseWithoutDate
, getName
, getSignedPrice
, getPrice
, getGroup
)
where

-- for new style cashbook

import qualified IgrepCashbook as Old
impprt Data.String.Utils (join)
import Text.Regex.Posix

data CashbookLine =
  Comment String
  | Item
    { date  :: Maybe String
    , name :: String
    , price :: Int
    , group :: String }

parseWithoutDate :: String -> [ Either String CashbookLine ]
parseWithoutDate c = map parseLineWithoutDate nls'
  where
    ls = lines c
    ns = [ 1..( length ls ) ]
    nls = zip ns ls
    nls' = filter (\(n, x) ->  not $ isDateLine x ) nls
    parseLineWithoutDate (n, x)
      | Old.isCommentLine x = Right Comment x
      | otherwise = parseItemLineWithoutDate n x
    parseItemLineWithoutDate = itemFromLine Nothing

{-
isItemLine :: String -> Bool
isItemLine x = not $ Old.isCommentLine x || isDateLine x
-}

isDateLine :: String -> Bool
isDateLine x = x =~ dateRegex

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

itemFromLine :: Maybe String -> Int -> String -> Either String CashbookLine
itemFromLine d n x = validate $ Old.parseLine x
  where
  validate [] = Left emptyItem
  validate [name] = Left $ mkMsg noPriceAndGroup name
  validate [name, signedPrice] = Left $ mkMsg noGroup ( name ++ "  " ++ signedPrice )
  validate i@(name:signedPrice:group:_)
    | signedPrice =~ priceRegex = Right $ mkItem d name signedPrice group
    | otherwise = Left $ mkMsg invalidPrice $ join "  " i

  mkMsg :: String -> String -> String
  mkMsg e c = concat [ e, "\"", c, "\"", " at line ", show n  ]

mkItem :: Maybe String -> String -> String -> String -> CashbookLine
mkItem d n s g = Item d n p g
  where
    p' :: Int
    p' = mkPrice s
    p :: Int
    p = if isIncomePrice s
          then p'
          else negate p'
    mkPrice = read $ filter isNumberChar
    isNumberChar x = x `elem` ['0'..'9']

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
