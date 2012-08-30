import qualified Data.Map as Map
import Control.Monad
import Data.List
import System.IO

import IgrepCashbook

convertLines :: [String] -> [String]
convertLines ls =
  map formatGroup $ groupBy sameDate $ map ( fixIncomeLine . parseLine ) ls
  where
    sameDate :: Item -> Item -> Bool
    sameDate ['#':_] _ = True
    sameDate _ ['#':_] = True
    sameDate i1 i2 = equalBy getDate i1 i2

fixIncomeLine :: Item -> Item
fixIncomeLine [day, name, price]
--         add regex representing salary
  | name ~= "" = day:name:price:["給料"]
  | otherwise = day:name:price:["その他"]
fixIncomeLine xs = xs

formatGroup :: [Item] -> [String]
formatGroup [] = ""
formatGroup xs = ( getDateOfGroup xs ):( map stripDate xs )
  where
    getDateOfGroup xs = getDate $ find ( not . isComment ) xs
    stripDate :: Item -> String
    stripDate [s@('#':_)] = s
    stripDate ( day:name:price:group:xs ) = " " ++ name ++ "  " ++ price ++ "  " ++ group ++ concat xs
    stripDate xs = error "Invalid data: " ++ show xs

main = do
  args <- getArgs
  forM args ( \ a -> do
    contents <- readFile a
    let new_money = unlines convertLines lines contents
    writeFile new_money $ a ++ ".new" )
