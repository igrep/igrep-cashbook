module IgrepCashbook2
( Item
, isComment
, isItemLine
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
import Text.Regex

type Item = Old.Item

isComment = Old.isComment

isItemLine :: String -> Bool
isItemLine (' ':_) = True
isItemLine _ = False

parseItemLine :: String -> Item
parseItemLine (' ':s) = Old.parseLine s
parseItemLine s = [s]

emptyItem = "invalid item: empty item. no data given"
noPriceAndGroup = "invalid item: neither price nor group given"
noGroup = "invalid item: no group given"
invalidPrice = "invalid item: malformed price"

priceRegex = mkRegex "^+?[1-9][_,0-9]*$"

validateItem :: Item -> Either String Item
validateItem [] = Left emptyItem
validateItem [name] = Left $ noPriceAndGroup ++ show name
validateItem i@[_name, _signedPrice] = Left $ noGroup ++ show i
validateItem i@(_name:signedPrice:_group:_)
  | signedPrice =~ priceRegex -> Right i
  | otherwise -> Left $ invalidPrice ++ show i

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
