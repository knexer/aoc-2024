{-# OPTIONS_GHC -Wno-type-defaults #-}
module D11.Part_1 where

import Data.List (unfoldr)
import Data.Foldable (for_)

blink :: [Int] -> [Int]
blink [] = []
blink (stone:stones)
  | stone == 0 = 1 : blink stones
  | even len = (read . take (div len 2)) stoneStr : (read . drop (div len 2)) stoneStr : blink stones
  | otherwise = stone * 2024 : blink stones
  where
    stoneStr = show stone
    len = length stoneStr


main :: String -> IO ()
main contents = do
    let ogStones = map read (words contents)
    let blinks = 25
    let blinkStones = take (blinks + 1) (unfoldr (\stones -> Just (stones, blink stones)) ogStones)
    let lengths = map length blinkStones

    for_ (zip [0..] lengths) print
    print $ last lengths
