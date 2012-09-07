module IgrepCashbook
( Item
, parseLine
, getDate
, isComment
)
where

-- for old style cashbook

import Text.Regex

type Item = [String]

parseLine :: String -> Item
parseLine s@('#':_) = [s]
parseLine s = splitRegex r s
  where
    r = mkRegex "  +|\t"

getDate :: Item -> String
getDate = (!!0)

isComment :: Item -> Bool
isComment [('#':_)] = True
isComment _ = False
