module D14.Part_2 where

import Data.Char (isDigit)
import Data.Foldable (for_)

type Vector = (Int, Int)

data Robot = Robot
  { pos :: Vector,
    velocity :: Vector
  }
  deriving (Show)

extractNumbers :: String -> [Int]
extractNumbers [] = []
extractNumbers string
  | length numbers == 0 = []
  | otherwise = read numbers : extractNumbers rest
  where
    (numbers, rest) = span isDigitOrNegative (dropWhile (not . isDigitOrNegative) string)
    isDigitOrNegative c = isDigit c || c == '-'

parseRobot :: String -> Robot
parseRobot str =
  Robot
    { pos = (nums !! 0, nums !! 1),
      velocity = (nums !! 2, nums !! 3)
    }
  where
    nums = extractNumbers str

moveBot :: (Int, Int) -> Robot -> Robot
moveBot size bot =
  Robot
    { pos = newPos,
      velocity = velocity bot
    }
  where
    newPos = ((mod unModPosX (fst size)), (mod unModPosY (snd size)))
    unModPosX = (fst . pos) bot + (fst . velocity) bot
    unModPosY = (snd . pos) bot + (snd . velocity) bot

renderBots :: (Int, Int) -> [Robot] -> [[Int]]
renderBots size bots = map (renderBotsRow (fst size) . calcBotsInRow) [0..(snd size - 1)]
  where
    calcBotsInRow i = filter (\bot -> (snd . pos) bot == i) bots
    renderBotsRow width botsInRow = map (numBotsInCol botsInRow) [0..width - 1]
    numBotsInCol botsInRow i = (length . filter (\bot -> (fst . pos) bot == i)) botsInRow

symDistance :: [[Int]] -> Int
symDistance xs = sum $ map (uncurry symDistanceRow) (zip xs flipped)
  where
    flipped = map reverse xs
    symDistanceRow x y = sum $ map (abs . uncurry (-)) (zip x y)

printBotsRow :: [Int] -> String
printBotsRow row = concat (map show row)

calcMinimums :: (Int, Int) -> [Robot] -> ([Robot] -> Int) -> Int -> Int -> [([Robot], Int, Int)]
calcMinimums size robots eval bestScore iteration
  | score < bestScore = (robots, score, iteration) : next
  | otherwise = next
  where
    nextRobots = map (moveBot size) robots
    nextScore = if score < bestScore then score else bestScore
    score = eval robots
    next = calcMinimums size nextRobots eval nextScore (iteration + 1)

printMinimum :: (Int, Int) -> ([Robot], Int, Int) -> IO ()
printMinimum size (robots, score, iteration) = do
    for_ (renderBots size robots) (print . printBotsRow)
    print $ "Score: " ++ show score
    print $ "Iteration: " ++ show iteration

main :: String -> IO ()
main contents = do
  let robots = map parseRobot (lines contents)
  let size = (101, 103)
  print . symDistance $ renderBots size robots
  let minSizes :: [([Robot], Int, Int)] = calcMinimums size robots (symDistance . renderBots size) 1000 0
  for_ minSizes (printMinimum size)
  
--   print ""
