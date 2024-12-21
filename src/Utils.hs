module Utils where
import Data.Char (isDigit)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delimiter x = as : split delimiter (drop 1 bs)
  where
    (as, bs) = span (/= delimiter) x

coordinates :: [[(Int, Int)]]
coordinates = map rowNumberToIds [0..]
  where
    rowNumberToIds rowNumber = map (,rowNumber) [0..]

zipWithCoords :: [[a]] -> [[((Int, Int), a)]]
zipWithCoords = zipWith zip coordinates

extractNumbers :: String -> [Int]
extractNumbers [] = []
extractNumbers string
  | length numbers == 0 = []
  | otherwise = read numbers : extractNumbers rest
  where
    (numbers, rest) = span isDigitOrNegative (dropWhile (not . isDigitOrNegative) string)
    isDigitOrNegative c = isDigit c || c == '-'
