import qualified Data.Map as Map
import Data.List
import qualified Data.String.Utils as Str
import Control.Monad
-- import System.IO
import System.Environment (getArgs)

import IgrepCashbook

-- general function
equalBy :: (Eq b) => ( a -> b ) -> a -> a -> Bool
equalBy f a1 a2 = f a1 == f a2
--

convertLines :: [String] -> [String]
convertLines c =
  concatMap formatGroup gs
  where
    is = map ( fixIncomeLine . parseLine ) c
    gs = groupBy sameDate is
    sameDate :: Item -> Item -> Bool
    sameDate [('#':_)] [('#':_)] = True
    sameDate [('#':_)] _ = False
    sameDate _ [('#':_)] = True
    sameDate i1 i2 = equalBy getDate i1 i2

fixIncomeLine :: Item -> Item
fixIncomeLine [day, name, price]
  | isSaraly name = day:name:price:["給料"]
  | otherwise = day:name:price:["その他"]
  where
    isSaraly x =
      x `isInfixOf` "給料" || x `isInfixOf` "財形貯蓄"
fixIncomeLine xs = xs

formatGroup :: [Item] -> [String]
formatGroup [] = []
formatGroup xs = concatDateGroup dateOfGroup withoutDate
  where
    dateOfGroup :: Maybe String
    dateOfGroup = fmap getDate $ find ( not . isComment ) xs
    withoutDate :: [String]
    withoutDate = map stripDate xs
    concatDateGroup :: Maybe String -> [String] -> [String]
    concatDateGroup Nothing g = g
    concatDateGroup (Just d ) g = d:g

mkItemStr :: String -> String -> String -> String
mkItemStr name price gr =
  " " ++ name ++ "  " ++ price ++ "  " ++ gr

stripDate :: Item -> String
stripDate [s@('#':_)] = s
stripDate ( _day:name:price:gr:[] ) =
  mkItemStr name price gr
stripDate ( _day:name:price:gr:xs ) =
  ( mkItemStr name price gr ) ++ "  " ++ Str.join "  " xs
stripDate xs = Str.join "  " xs

main :: IO ()
main = do
  args <- getArgs
  forM_ args ( \ a -> do
    contents <- readFile a
    let new_money = unlines $ convertLines $ lines contents
    writeFile ( a ++ ".new" ) new_money  )
