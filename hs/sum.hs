import qualified Data.Map as Map
import Data.List
import System.IO
import Control.Monad

import IgrepCashbook2

type Summary     = Map.Map Group Int

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
  -- how to warn upon stderr?
  hPutStrLn StdErr "[ERROR] " ++ e ++ " of " ++ path ++ ".")

incomesAndExpenditures :: [Item] -> ([Item], [Item])
incomesAndExpenditures is = partition (\i -> isIncomePrice getSignedPrice i ) is

main = do
  args <- getArgs
  forM args (\a -> do
    contents <- readFile a

    let items = parseContents contents
    let ( items', errors ) = itemsAndErrors items
    warnErrors a errors
    let ( inItems, exItems ) = incomesAndExpenditures items'
    let exSummary = summarizeItems exItems
    let inSummary = summarizeItems inItems
    let exSum = sum $ Map.values exSummary
    let inSum = sum $ Map.values inSummary

    -- TODO: print summary of all files in specified in the argument
    putStrLn "# EXPENDITURES #"
    putStrLn formatSummary exSummary
    putStrLn formatItem "Sum" exSum
    putStrLn "# INCOME #"
    putStrLn formatSummary inSummary
    putStrLn formatItem "Sum" inSum )
