module D14.Part_1 where

import Data.Char (isDigit)
import Debug.Trace (traceShowId)
import Data.Maybe (catMaybes)

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

moveBot :: (Int, Int) -> Int -> Robot -> Vector
moveBot size steps bot = ((mod unModPosX (fst size)), (mod unModPosY (snd size)))
  where
    unModPosX = (fst . pos) bot + steps * (fst . velocity) bot
    unModPosY = (snd . pos) bot + steps * (snd . velocity) bot

data Quadrant = NW | NE | SW | SE deriving (Eq, Show)

posToQuadrant :: (Int, Int) -> Vector -> Maybe Quadrant
posToQuadrant size pos
  | x < midX && y < midY = Just NW
  | x < midX && y > midY = Just SW
  | x > midX && y < midY = Just NE
  | x > midX && y > midY = Just SE
  | otherwise = Nothing
  where
    x :: Double = (fromIntegral . fst) pos
    y :: Double = (fromIntegral . snd) pos
    midX :: Double = (fromIntegral . (subtract 1) . fst) size / 2.0
    midY :: Double = (fromIntegral . (subtract 1) . snd) size / 2.0

main :: String -> IO ()
main contents = do
  let robots = map parseRobot (lines contents)
  let quadrants = catMaybes $ map (posToQuadrant (101, 103) . (moveBot (101, 103) 100)) robots
  let nw = length $ filter (== NW) quadrants
  let sw = length $ filter (== SW) quadrants
  let ne = length $ filter (== NE) quadrants
  let se = length $ filter (== SE) quadrants

  print $ nw * sw * ne * se
