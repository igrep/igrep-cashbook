module MigrateCashbook where

import IgrepCashbook

import qualified Data.Map as Map
import Data.List
import Text.Regex

convertLines :: [String] -> [String]
convertLines ls =
  map formatGroup $ groupBy isSameDate $ map ( fixIncomeLine . parseLine ) ls
  where
    isSameDate :: Item -> Item -> Bool
    isSameDate ['#':_] _ = True
    isSameDate _ ['#':_] = True
    isSameDate = equalBy getDate

fixIncomeLine :: Item -> Item
fixIncomeLine [x] = [x]
fixIncomeLine [day, name, price]
  | name ~= "" = day:name:price:["給料"]
  | otherwise = day:name:price:["その他"]
fixIncomeLine xs = xs

formatGroup :: [Item] -> [String]
formatGroup [] = ""
-- how to handle comment lines
formatGroup xs = ( getDateOfGroup xs ):( map stripDate xs )
  where
    getDateOfGroup xs = getDate $ find ( not . isComment ) xs
    stripDate :: Item -> String
    stripDate [s@('#':_)] = s
    stripDate ( day:name:price:group:xs ) = " " ++ name ++ "  " ++ price ++ "  " ++ group ++ concat xs
    stripDate xs = error "Invalid data: " ++ show xs
