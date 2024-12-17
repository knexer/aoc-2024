module D13.Part_1 where

import Data.Char (isDigit)

type Vector = (Int, Int)

data Machine = Machine
  { a :: Vector,
    b :: Vector,
    goal :: Vector
  } deriving Show

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

parse :: String -> [Machine]
parse input = map parseMachine (((split "") . lines) input)

main :: String -> IO ()
main contents = do
  let machines = parse contents
  mapM_ print machines
  print ""
