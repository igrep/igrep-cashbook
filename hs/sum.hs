import Data.List
import Data.Char
import System.IO
import System.Environment
import Control.Monad
import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.Text as Text
import Data.Text ( Text )

import IgrepCashbook2

-- general functions

useTextFunc :: (Text -> Text) -> String -> String
useTextFunc f s = Text.unpack $ f $ Text.pack s

justifyRight :: Int -> Char -> String -> String
justifyRight i c s = useTextFunc ( Text.justifyRight i c ) s

justifyLeft :: Int -> Char -> String -> String
justifyLeft i c s = useTextFunc ( Text.justifyLeft i c ) s

--

selectItemLine :: [Int] -> [String] -> [(Int, String)]
selectItemLine ns ls =
  filter ( \(n, l) -> isItemLine l ) $ zip ns ls

itemsAndErrors :: [(Int, Either String Item)] -> ([Item], [String])
itemsAndErrors = foldr f ([], [])
  where
    f :: (Int, Either String Item) -> ([Item], [String]) -> ([Item], [String])
    f (n, Left s) (is, ss) = (is, (s ++ " at line " ++ show n):ss)
    f (_, Right i) (is, ss) = (i:is, ss)

warnErrors :: String -> [String] -> IO ()
warnErrors path es = forM_ es $ (\e -> do
  hPutStrLn stderr ( "[WARNING] " ++ e ++ " of " ++ path ++ ". OMMITED!" ))

incomesAndExpenditures :: [Item] -> ([Item], [Item])
incomesAndExpenditures = partition ( isIncomePrice . getSignedPrice )

type Summary = Map String Int

summarizeItems :: [Item] -> Summary
summarizeItems is =
  Map.fromListWith (+) $ map (\i -> (getName i, getPrice i) ) is

digit :: Int -> Int
digit = length . show

formatSummary :: Int -> Int -> Summary -> String
formatSummary l d s = concatMap f $ Map.toList s
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

    let items = parseContents contents
    let ( items', errors ) = itemsAndErrors items
    warnErrors a errors
    return items' )

  let ( inItems, exItems ) = incomesAndExpenditures $ concat items
  let exSummary = summarizeItems exItems
  let inSummary = summarizeItems inItems
  let exSum = sum $ Map.elems exSummary
  let inSum = sum $ Map.elems inSummary
  let sumDigit = max (digit exSum) (digit inSum) 
  let groupLen = 10 -- fixed so far

  putStrLn "# EXPENDITURES #"
  putStrLn $ formatSummary groupLen sumDigit exSummary
  putStrLn $ formatSumItem groupLen sumDigit "Sum" exSum
  putStrLn "# INCOME #"
  putStrLn $ formatSummary groupLen sumDigit inSummary
  putStrLn $ formatSumItem groupLen sumDigit "Sum" inSum
