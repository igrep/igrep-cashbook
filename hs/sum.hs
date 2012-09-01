import qualified Data.Map as Map
import Data.List
import System.IO

import IgrepCashbook2

type Group       = String
type SignedPrice = String
type Summary     = Map.Map Group Int

main = do
  args <- getArgs
  forM args (\a -> do
    contents <- readFile a

    let items = parseContents contents
    let ( inItems, exItems ) = classifyItems items
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
