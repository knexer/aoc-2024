module D20.Part_1 where

import Data.List (find, transpose)
import Data.Maybe (catMaybes, fromMaybe, isJust)

type Track = [[(Char, Maybe Int)]]

-- search for places where there are two numbers, two steps apart, that are X distance apart
countGoodCheats :: Int -> Track -> Int
countGoodCheats num track = rows track + cols track + diags
  where
    delta = num + 2
    rows = sum . map (countGoodCheatsRow delta)
    cols = rows . transpose
    diags = sum $ map (uncurry (countGoodCheatsDiagonal delta)) (zip track (drop 1 track))

countGoodCheatsRow :: Int -> [(Char, Maybe Int)] -> Int
countGoodCheatsRow num (a : b : c : rest) = (if isGoodCheat then 1 else 0) + (countGoodCheatsRow num (b : c : rest))
  where
    isGoodCheatPred left right = abs (left - right) >= num
    isGoodCheat = fromMaybe False $ liftA2 isGoodCheatPred (snd a) (snd c)
countGoodCheatsRow _ _ = 0

countGoodCheatsDiagonal :: Int -> [(Char, Maybe Int)] -> [(Char, Maybe Int)] -> Int
countGoodCheatsDiagonal num (topLeft : topRight : tops) (botLeft : botRight : bots) =
  (if isGoodCheatUpRight then 1 else 0)
    + (if isGoodCheatDownRight then 1 else 0)
    + countGoodCheatsDiagonal num (topRight : tops) (botRight : bots)
  where
    isGoodCheatPred left right = abs (left - right) >= num
    isGoodCheatUpRight = fromMaybe False $ liftA2 isGoodCheatPred (snd botLeft) (snd topRight)
    isGoodCheatDownRight = fromMaybe False $ liftA2 isGoodCheatPred (snd topLeft) (snd botRight)
countGoodCheatsDiagonal _ _ _ = 0

-- flood fill the maze with distances
-- slow recursive way is slow but that's OK
floodFill :: Int -> [[(Char, Maybe Int)]] -> [[(Char, Maybe Int)]]
floodFill num oldTrack = if done then cols else floodFill ((num + 1)) cols
  where
    rows = map (floodFillRow num) oldTrack
    cols = (transpose . map (floodFillRow num) . transpose) rows
    done = (isJust . snd . head . catMaybes . map (find (\x -> fst x == 'E'))) cols

floodFillRow :: Int -> [(Char, Maybe Int)] -> [(Char, Maybe Int)]
floodFillRow _ [] = undefined
floodFillRow _ [x] = [x]
floodFillRow num (a : b : rest)
  | snd a == Just num = a : maybeFlood b : rest
  | snd b == Just num = maybeFlood a : floodFillRow num (b : rest)
  | otherwise = a : floodFillRow num (b : rest)
  where
    maybeFlood x = if (fst x == '.' || fst x == 'E') && snd x == Nothing then (fst x, Just (num + 1)) else x

parse :: String -> Track
parse contents = (map (map (\c -> if c == 'S' then (c, Just 0) else (c, Nothing))) . lines) contents

main :: String -> IO ()
-- main = print . floodFill 0 . parse
main = print . countGoodCheats 100 . floodFill 0 . parse
