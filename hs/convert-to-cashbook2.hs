import qualified Data.Map as Map
import Control.Monad
import Data.List
import Text.Regex
import System.IO

-- General function
equalBy :: (Eq b) => ( a -> b ) -> a -> a -> Bool
equalBy f a1 a2 = f a1 == f a2

type Item = [String]

getDate :: Item -> String
getDate = (!!0)

convertLines :: [String] -> [String]
convertLines ls =
  map formatGroup $ groupBy isSameDate $ map ( fixIncomeLine . parse ) ls
  where
    isSameDate :: Item -> Item -> Bool
    isSameDate ['#':_] _ = True
    isSameDate _ ['#':_] = True
    isSameDate = equalBy getDate

parse :: String -> Item
parse s@('#':_) = [s]
parse = splitRegex "  +|\t"

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
    isComment [('#':_)] = True
    isComment _ = False
    stripDate :: Item -> String
    stripDate [s@('#':_)] = s
    stripDate ( day:name:price:group:xs ) = " " ++ name ++ "  " ++ price ++ "  " ++ group ++ concat xs
    stripDate xs = error "Invalid data: " ++ show xs

main = do
  args <- getArgs
  forM args \ a -> do
    contents <- readFile a
    let new_money = unlines $ convertLine . lines contents
    writeFile new_money $ a ++ ".new"
