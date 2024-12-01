import System.Environment
import Data.List (sort)

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

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let (firstList, secondList) = parseInput contents
    let sortedFirstList = sort firstList
    let sortedSecondList = sort secondList
    let recombined = zip sortedFirstList sortedSecondList
    let differences = map difference recombined
    let totalDifference = sum differences
    print totalDifference
