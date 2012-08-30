module IgrepCashbook where

import Text.Regex

-- General function
equalBy :: (Eq b) => ( a -> b ) -> a -> a -> Bool
equalBy f a1 a2 = f a1 == f a2

type Item = [String]

parseLine :: String -> Item
parseLine "" = [""]
parseLine s@('#':_) = [s]
parseLine = splitRegex "  +|\t"

getDate :: Item -> String
getDate = (!!0)

isComment :: Item -> Bool
isComment [('#':_)] = True
isComment _ = False
