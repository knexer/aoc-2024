module D13.Part_1 where

import Data.Char (isDigit)
import Data.Maybe (catMaybes)

type Vector = (Int, Int)

data Machine = Machine
  { a :: Vector,
    b :: Vector,
    goal :: Vector
  }
  deriving (Show)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delimiter x = as : split delimiter (drop 1 bs)
  where
    (as, bs) = span (/= delimiter) x

extractNumbers :: String -> [Int]
extractNumbers [] = []
extractNumbers string
  | length numbers == 0 = []
  | otherwise = read numbers : extractNumbers rest
  where
    (numbers, rest) = span isDigit (dropWhile (not . isDigit) string)

parseVector :: String -> Vector
parseVector = makeVector . extractNumbers
  where
    makeVector (a : b : _) = (a, b)
    makeVector _ = undefined

parseMachine :: [String] -> Machine
parseMachine strings =
  Machine
    { a = parseVector (strings !! 0),
      b = parseVector (strings !! 1),
      goal = parseVector (strings !! 2)
    }

pressB :: Machine -> Int -> Maybe Int
pressB machine aPresses
  | x == (fst . goal) machine && y == (snd . goal) machine = Just bPresses
  | otherwise = Nothing
  where
    remainingGoal = (fst . goal) machine - aPresses * (fst . a) machine
    bPresses = remainingGoal `div` ((fst . b) machine)
    x = aPresses * (fst . a) machine + bPresses * (fst . b) machine
    y = aPresses * (snd . a) machine + bPresses * (snd . b) machine

minTokens :: Machine -> Maybe Int
minTokens machine
  | length justTokens > 0 = Just (foldr1 min justTokens)
  | otherwise = Nothing
  where
    allPresses = map (pressB machine) [0 .. 100]
    justTokens = (catMaybes . map tokens) (zip [0 .. 100] allPresses)

tokens :: (Int, Maybe Int) -> Maybe Int
tokens (_, Nothing) = Nothing
tokens (a, (Just b)) = Just (3 * a + b)

parse :: String -> [Machine]
parse input = map parseMachine (((split "") . lines) input)

main :: String -> IO ()
main = print . sum . catMaybes . (map minTokens) . parse
