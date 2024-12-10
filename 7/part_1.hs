import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)

maybeDiv :: Int -> Int -> Maybe Int
maybeDiv big smol
  | big `rem` smol == 0 = Just (big `div` smol)
  | otherwise = Nothing

reverseVals :: (Int, [Int]) -> (Int, [Int])
reverseVals (goal, vals) = (goal, reverse vals)

canBeMade :: (Int, [Int]) -> Bool
canBeMade (goal, [val]) = val == goal
canBeMade (goal, val:vals)
  | isJust divided = canBeMade (fromJust divided, vals) || canBeMade (subtracted, vals)
  | otherwise = canBeMade (subtracted, vals)
  where divided = goal `maybeDiv` val
        subtracted = goal - val

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
    print $ map parseInput contents
    print $ sum $ map fst $ filter canBeMade $ map (reverseVals . parseInput) contents

