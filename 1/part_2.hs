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

nextNum :: ([Integer], [Integer]) -> Integer
nextNum ([], []) = 0
nextNum (first@(x:_), second@(y:_))
    | x < y     = nextNum (firstRemainder, second)
    | x > y     = nextNum (first, secondRemainder)
    | otherwise = x * firstCount * secondCount + nextNum (firstRemainder, secondRemainder)
    where (firstCount, firstRemainder) = countNum (x, first)
          (secondCount, secondRemainder) = countNum (y, second)

countNum :: (Integer, [Integer]) -> (Integer, [Integer])
countNum (num, []) = (0, [])
countNum (num, list@(x:xs))
    | x == num  = (1 + count, remainder)
    | otherwise = (0, list)
    where (count, remainder) = countNum (num, xs)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let (firstList, secondList) = parseInput contents
    let sortedFirstList = sort firstList
    let sortedSecondList = sort secondList
    print (nextNum (sortedFirstList, sortedSecondList))
