module IgrepCashbook
( Item
, parseLine
, getDate
, isComment
, isCommentLine
)
where

-- for old style cashbook

type Item = [String]

parseLine :: String -> Item
parseLine s@('#':_) = [s]
parseLine s = fst folded : snd folded
  where
    folded = foldr f ("", []) s
    f :: Char -> (String, Item) -> (String, Item)
    f ' ' (' ':t, i) = ("", t:i)
    f '\t' (t, i) = ("", t:i)
    f c (t, i) = (c:t, i)

getDate :: Item -> String
getDate = (!!0)

isComment :: Item -> Bool
isComment [x] = isCommentLine x

isCommentLine :: String -> Bool
isCommentLine ('#':_) = True
isCommentLine _ = False
