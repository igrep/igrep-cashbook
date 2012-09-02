module IgrepCashbook2
( Item
, isComment
, isItemLine
, parseItemLine
)
where

-- for new style cashbook

import qualified IgrepCashbook as Old

type Item = Old.Item

isComment = Old.isComment

isItemLine :: String -> Bool
isItemLine (' ':_) = True
isItemLine _ = False

parseItemLine :: String -> Item
parseItemLine (' ':s) = Old.parseLine s
parseItemLine s = [s]
