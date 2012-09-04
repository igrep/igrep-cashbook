module IgrepCashbook2
( Item
, isComment
, isItemLine
, parseItemLine
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

emptyItem = ""

validateItem :: Item -> Either String Item
validateItem [] = Left emptyItem
validateItem [name] = Left $ noPriceAndGroup ++ show name
validateItem i@[_name, _signedPrice] = Left $ noGroup ++ show i
validateItem i@(_name:signedPrice:_group:_)
  | signedPrice =~ priceRegex -> Right i
  | otherwise -> Left $ invalidPrice ++ show i
