import Data.List
import Data.Char
import System.IO
import System.Environment
import Control.Monad
import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.Text as Text
import Data.Text ( Text )
import Data.Either (partitionEithers)
import Data.Ord (comparing)

import IgrepCashbook2

-- general functions

useTextFunc :: (Text -> Text) -> String -> String
useTextFunc f s = Text.unpack $ f $ Text.pack s

justifyRight :: Int -> Char -> String -> String
justifyRight i c s = useTextFunc ( Text.justifyRight i c ) s

justifyLeft :: Int -> Char -> String -> String
justifyLeft i c s = useTextFunc ( Text.justifyLeft i c ) s

--

warnErrors :: String -> [String] -> IO ()
warnErrors path es = forM_ es $ (\e -> do
  hPutStrLn stderr $ ( "[WARNING] " ++ e ++ " of " ++ path ++ ". OMMITED!" ))

incomesAndExpenditures :: [CashbookLine] -> ([CashbookLine], [CashbookLine])
incomesAndExpenditures = partition isIncome

type Summary = Map String Int

summarizeItems :: [CashbookLine] -> Summary
summarizeItems is =
  Map.fromListWith (+) $ map (\i -> (getGroup i, getPrice i) ) is

digit :: Int -> Int
digit = length . show

formatSummary :: Int -> Int -> Summary -> String
formatSummary l d s = concatMap f $ sortBy ( comparing snd ) $ Map.toList s
  where
    f (g, i) = formatSumItem l d g i

formatSumItem :: Int -> Int -> String -> Int -> String
formatSumItem l d g i =
  justifyLeft jl ' '  g ++ justifyRight d ' ' ( show i ) ++ "\n"
  where
    jl = l - ( length $ filter ( not . is8bitChar ) g )
    is8bitChar :: Char -> Bool
    is8bitChar = ( < 255 ) . ord

main :: IO ()
main = do
  args <- getArgs
  items <- forM args (\a -> do
    contents <- readFile a

    let items = parseWithoutDate contents
    let ( errors, items' ) = partitionEithers items
    warnErrors a errors
    return items' )

  let ( inItems, exItems ) = incomesAndExpenditures $ concat items
  let exSummary = summarizeItems exItems
  let inSummary = summarizeItems inItems
  let exSum = sum $ Map.elems exSummary
  let inSum = sum $ Map.elems inSummary
  let wholeSum = inSum - exSum
  let sumDigit = max (digit exSum) (digit inSum) 
  let groupLen = 8 -- fixed so far

  putStrLn "# 支出 #"
  putStr   $ formatSummary groupLen sumDigit exSummary
  putStr   $ formatSumItem groupLen sumDigit "合計" exSum
  putStr   "\n"
  putStrLn "# 収入 #"
  putStr   $ formatSummary groupLen sumDigit inSummary
  putStr   $ formatSumItem groupLen sumDigit "合計" inSum
  putStr   "\n"
  putStr   $ formatSumItem groupLen sumDigit "総合計" wholeSum
