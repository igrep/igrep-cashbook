import qualified Data.Map as Map
import Data.List
import System.IO
import Control.Monad

import IgrepCashbook2

-- Item: synonim of [String] so far
parseContents :: String -> [(Int, Either String Item)]
parseContents c =
  map ( \(n, l) -> ( n, validateItem parseItemLine l ) ) is
  where
    ls = lines c
    ns = [ 1..( length ls ) ]
    is = selectItemLine ns ls

selectItemLine :: [Int] -> [String] -> [(Int, String)]
selectItemLine ns ls =
  filter ( \(n, l) -> isItemLine l ) $ zip ns ls

itemsAndErrors :: [(Int, Either String Item)] -> ([Item], [String])
itemsAndErrors xs = foldr f ([], []) xs
  where
    f :: (Int, Either String Item) -> ([Item], [String]) -> ([Item], [String])
    f (n, Left s) (is, ss) = (is, (s ++ " at line " ++ read n):ss)
    f (_, Right i) (is, ss) = (i:is, ss)

warnErrors :: String -> [String] -> IO ()
warnErrors path es = forM_ es $ (\e -> do
  hPutStrLn stderr "[WARNING] " ++ e ++ " of " ++ path ++ ". OMMITED!")

incomesAndExpenditures :: [Item] -> ([Item], [Item])
incomesAndExpenditures is = partition ( isIncomePrice . getSignedPrice ) is

type Summary = Map.Map Group Int

summarizeItems :: [Item] -> Summary
summarizeItems is =
  Map.fromListWith (+) $ map (\i -> (getName i, getPrice i) ) is

digit :: Int -> Int
digit = length . show

main = do
  args <- getArgs
  items <- forM args (\a -> do
    contents <- readFile a

    let items = parseContents contents
    let ( items', errors ) = itemsAndErrors items
    warnErrors a errors
    return items' )

  let ( inItems, exItems ) = incomesAndExpenditures concat items
  let exSummary = summarizeItems exItems
  let inSummary = summarizeItems inItems
  let exSum = sum $ Map.values exSummary
  let inSum = sum $ Map.values inSummary
  let sumDigit = max $ (digit exSum) (digit inSum) 
  let groupLen = 10 -- fixed so far

  putStrLn "# EXPENDITURES #"
  putStrLn formatSummary groupLen sumDigit exSummary
  putStrLn formatItem groupLen sumDigit "Sum" exSum
  putStrLn "# INCOME #"
  putStrLn formatSummary groupLen sumDigit inSummary
  putStrLn formatItem sumDigit groupLen "Sum" inSum
