module IgrepCashbook
( Item
, parseLine
, parseItemLine
, getDate
, isComment
, isCommentLine
)
where

import Data.String.Utils (replace)

-- for old style cashbook

type Item = [String]

parseLine :: String -> Item
parseLine s
  | isCommentLine s = [s]
  | otherwise = parseItemLine $ expandTab s

expandTab :: String -> String
expandTab = replace "\t" "  "

parseItemLine :: String -> Item
parseItemLine s = lastCell : accum
  where
    ( lastCell, accum, _ ) = folded
    folded = foldr f ("", [], False) s
    -- The 3rd value of the tuple means "in spaces"
    f :: Char -> (String, Item, Bool) -> (String, Item, Bool)
    f ' ' (' ':t, i, False) = (t, i, True)
    f ' ' (t, i, True) = (t, i, True)
    f c (t, i, True) = ([c], t:i, False)
    f c (t, i, False) = (c:t, i, False)

getDate :: Item -> String
getDate = (!!0)

isComment :: Item -> Bool
isComment [x] = isCommentLine x
isComment _ = False

isCommentLine :: String -> Bool
isCommentLine ('#':_) = True
isCommentLine _ = False
