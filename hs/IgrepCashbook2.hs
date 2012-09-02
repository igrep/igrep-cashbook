module IgrepCashbook2
( Item
, isComment
, isItemLine
)
where

-- for new style cashbook

import Text.Regex

import qualified IgrepCashbook as Old

type Item = Old.Item

isComment = Old.isComment

isItemLine :: String -> Bool
isItemLine (' ':_) = True
isItemLine _ = False
