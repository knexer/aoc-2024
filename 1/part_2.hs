import System.Environment
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

readInput :: IO String
readInput = do
    args <- getArgs
    readFile (head args)

parseInput :: String -> ([Integer], [Integer])
parseInput contents = do
    let splitContents = lines contents
    let moreSplit = map words splitContents
    let firstList = map (read . head) moreSplit
    let secondList = map (read . head . tail) moreSplit
    (firstList, secondList)

difference :: (Integer, Integer) -> Integer
difference (first, second) = abs (first - second)

countOccurences :: [Integer] -> Map Integer Integer
-- how to implement??
-- make a list of tuples? and then mapify it?
-- list of tuples can be made recursively:
--   grab the first element and all subsequent ones that equal it, count those
--   add to output list

countNext :: [Integer] [(Integer, Integer)] => [Integer] [(Integer, Integer)]
countNext input output = do
    let nextKey = head input
    -- the rest of the input list could have some number of nextKeys
    -- more recursion???
    -- there must be a simpler formulation
    -- can we just do a single flat recursion?
    input output

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let (firstList, secondList) = parseInput contents
    let sortedFirstList = sort firstList
    let sortedSecondList = sort secondList
    -- collapse second list into a map from value to num occurences
    -- then apply that to each element of first list, and sum
    let recombined = zip sortedFirstList sortedSecondList
    let differences = map difference recombined
    let totalDifference = sum differences
    print totalDifference
