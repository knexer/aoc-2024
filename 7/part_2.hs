import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)

maybeDiv :: Int -> Int -> Maybe Int
maybeDiv big smol
  | big `rem` smol == 0 = Just (big `div` smol)
  | otherwise = Nothing

reverseVals :: (Int, [Int]) -> (Int, [Int])
reverseVals (goal, vals) = (goal, reverse vals)

canMake :: (Int, [Int]) -> Bool
canMake (goal, [val]) = val == goal
canMake (goal, val1:val2:vals) = canMake (goal, added:vals) || canMake (goal, multed:vals) || canMake (goal, concatenated:vals)
    where added = val1 + val2
          multed = val1 * val2
          concatenated :: Int = read (show val1 ++ show val2)

canBeMade :: (Int, [Int]) -> Bool
canBeMade (goal, [val]) = val == goal
canBeMade (goal, val:vals)
  | isJust divided = canBeMade (fromJust divided, vals) || withoutDivision
  | otherwise = withoutDivision
  where divided = goal `maybeDiv` val
        subtracted = goal - val
        concatenated :: Int = read (show val ++ show goal)
        withoutDivision = canBeMade (subtracted, vals) || canBeMade (concatenated, vals)

split :: Eq a => a -> [a] -> [[a]]
split delimiter [] = []
split delimiter x = as : split delimiter (drop 1 bs)
  where (as, bs) = span (/= delimiter) x

parseInput :: String -> (Int, [Int])
parseInput input = (testValue, map read (tail byWhitespace))
    where byWhitespace = split ' ' input
          testValue = (read . takeWhile (':' /=) . head) byWhitespace

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)
    print $ canMake (19, [1, 9])
    print $ map parseInput contents
    print $ sum $ map fst $ filter canMake $ map parseInput contents

