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

-- countOccurrences :: [Integer] -> Map Integer Integer
-- how to implement??
-- make a list of tuples? and then mapify it?
-- list of tuples can be made recursively:
--   grab the first element and all subsequent ones that equal it, count those
--   add to output list

-- countNext :: [Integer] [(Integer, Integer)] -> [Integer] [(Integer, Integer)]
-- countNext input output = do
--     let nextKey = head input
--     -- the rest of the input list could have some number of nextKeys
--     -- more recursion???
--     -- there must be a simpler formulation
--     -- can we just do a single flat recursion?
--     input output

-- we have: two lists of numbers, sorted
-- we want: this weird asymmetric 'similarity score' thing
-- actually... is it symmetric? I think it is. It's the product of:
-- the value
-- how many times it is in list 1
-- how many times it is in list 2

-- so we could do:
-- get the smallest number from either list
-- count how many times it is in list 1 and list 2
-- calculate the incremental value
-- strip those off from the head

nextNum :: ([Integer], [Integer]) -> Integer
nextNum ([], list) = 0
nextNum (list, []) = 0
nextNum (first, second)
    | head first < head second = nextNum (firstRemainder, second)
    | head first > head second = nextNum (first, secondRemainder)
    | otherwise                = head first * firstCount * secondCount + nextNum (firstRemainder, secondRemainder)
    where (firstCount, firstRemainder) = countNum (head first, first)
          (secondCount, secondRemainder) = countNum (head second, second)

countNum :: (Integer, [Integer]) -> (Integer, [Integer])
countNum (num, []) = (0, [])
countNum (num, list)
    | head list == num = (1 + count, remainder)
    | otherwise        = (0, list)
    where (count, remainder) = countNum (num, tail list)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let (firstList, secondList) = parseInput contents
    let sortedFirstList = sort firstList
    let sortedSecondList = sort secondList
    print (nextNum (sortedFirstList, sortedSecondList))
