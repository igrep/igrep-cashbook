import qualified Data.Map as Map
import Data.List
import System.IO

import IgrepCashbook2

type Group       = String
type SignedPrice = String
type Summary     = Map.Map Group Int

-- Item: synonim of [String] so far
parseContents :: String -> [(Int, Item)]
parseContents c =
 map ( \(n, l) -> ( name, n, parseItemLine l ) ) is
 where
   ls = lines c
   ns = [ 1..( length ls ) ]
   is = selectItemLine ns ls

selectItemLine :: [Int] -> [String] -> [(Int, String)]
selectItemLine ns ls =
  filter ( \(n, l) -> isItemLine l ) $ zip ns ls

main = do
  args <- getArgs
  forM args (\a -> do
    contents <- readFile a

    let items = parseContents contents
    let items' = rejectInvalidItems items
    let ( inItems, exItems ) = classifyItems items'
    let exSummary = summarizeItems exItems
    let inSummary = summarizeItems inItems
    let exSum = sum $ Map.values exSummary
    let inSum = sum $ Map.values inSummary

    putStrLn "# EXPENDITURES #"
    putStrLn formatSummary exSummary
    putStrLn formatItem "Sum" exSum
    putStrLn "# INCOME #"
    putStrLn formatSummary inSummary
    putStrLn formatItem "Sum" inSum )
