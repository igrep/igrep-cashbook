module SumCashbook where

import qualified Data.Map as Map
import Data.List

import IgrepCashbook
import IgrepCashbook2

{-splitBy2spaces :: String -> [String]-}
{-splitBy2spaces "" = []-}
{-splitBy2spaces "  " = []-}

type Group       = String
type SignedPrice = String
type Summary     = Map.Map Group Int

isIncome :: ( Group, SignedPrice ) -> Bool
isIncome _ '+':_ = True
isIncome _ _ = False

getGroupPrice :: String -> Int -> [String] -> (Group, SignedPrice)
getGroupPrice f i [] = error "[ERROR] Invalid Argument: Empty item!" ++ " at " ++ f ++ ": " ++ i ++ "."
getGroupPrice f i [ _ ] = error "[ERROR] Invalid Argument: Price and group not found!" ++ " at " ++ f ++ ": " ++ i ++ "."
getGroupPrice f i [ _, _ ] = error "[ERROR] Invalid Argument: Group not found!" ++ " at " ++ f ++ ": " ++ i ++ "."
getGroupPrice _ _ xs = ( xs !! 2, xs !! 1 )

parseItemLine :: String -> [String]
parseItemLine = splitRegexPR "  +|\t"

-- Is there any solution to handle errors?
selectItemLine :: String -> [Int] -> [String] -> [String]
selectItemLine fn ns ls = filter isItemLine $ zip ns ls 
  where
    isItemLine :: ( Int, String ) -> Bool
    isItemLine ( n, l ) = ( ItemLine == ) . classifyLine fn n l

classifyLine :: String -> Int -> String -> LineType
classifyLine _ _ "" = CommentLine
classifyLine _ _ '#':_ = CommentLine
classifyLine _ _ ' ':_ = ItemLine
classifyLine f i s
  | s =~ "^[0-9]" = DateLine
  | otherwise = error "[ERROR] Invalid line: " ++ show s ++ " at " ++ f ++ ": " ++ i ++ "."

data LineType = CommentLine | ItemLine | DateLine deriving (Show)

{-data Item { isIncome :: Bool-}
          {-, group :: Group-}
          {-, price :: Int } deriving ( Show, Ord )-}

sortBySum :: Summary -> [ ( Group, Int ) ]
sortBySum s = map ( \ k -> ( k, getPrice k ) ) ks
  where
    ks = sortBy ( \ a b -> getPrice a `compare` getPrice b ) Map.keys s
    getPrice = ( flip Map.lookup ) s

line_nums = [1..]

itemLines = selectItemLine filename line_nums lines
groupPrices = map getGroupPrice $ map parseItemLine itemLines
( incomes, expenditures ) = partition isIncome groupPrices
inSummary = Map.fromListWith (+) incomes
exSummary = Map.fromListWith (+) expenditures

inSum = sum $ Map.values inSummary
exSum = sum $ Map.valies exSummary

formatSummary :: [ ( Group, Int ) ] -> String
formatSummary xs = concatMap formatItem xs

maxDigit = max inD exD
  where
    inD = length show inSum
    exD = length show exSum

formatItem ( g, i ) = g ++ "\t%" ++ printf ( "%" ++ show maxDigit ++ "d\n" ) i

main = do
  args <- getArgs
  filename =  args !! 0
  contents <- readFile filename
  lines =  line contents 

  putStrLn "# INCOME #"
  putStrLn formatSummary exSummary
  putStrLn formatItem ( "Sum", exSum )
  putStrLn "# EXPENDITURES #"
  putStrLn formatSummary inSummary
  putStrLn formatItem ( "Sum", inSum )
