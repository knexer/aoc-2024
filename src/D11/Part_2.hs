module D11.Part_2 where

import Data.List (unfoldr)
import Data.Foldable (for_)
import qualified Data.MultiSet as MultiSet

blink :: MultiSet.MultiSet Int -> MultiSet.MultiSet Int
-- blink set = MultiSet.join ((fmap blinkOne) set)
blink = MultiSet.join . MultiSet.map blinkOne

blinkOne :: Int -> MultiSet.MultiSet Int
blinkOne stone
  | stone == 0 = MultiSet.fromList [1]
  | even len = MultiSet.fromList [(read . take (div len 2)) stoneStr, (read . drop (div len 2)) stoneStr]
  | otherwise = MultiSet.fromList [stone * 2024]
  where
    stoneStr = show stone
    len = length stoneStr

-- It's too slow!
-- maybe we can work in lengths directly?
-- or memoize?
-- or bit of this, bit of that? Like identify cycles or reductions and apply them
-- like the way the splitting and multiplying works, the single digits will be quite common
-- but other numbers will always happen, e.g. in the input
-- so, what if we figured out the pattern for (0, 1, ..., 9) and any time we saw those we could short circuit it?
-- and doing that recursively, ofc, which is the tricksy bit

-- basically it feels like a wacky mix of:
-- a) dynamic programming for what happens with the 1s, and
-- b) just work through the others, but cull the single digits via the DP

-- in non haskell land, I would do:
-- make an array of (number of blinks x number on stone) that is the number of stones after X blinks on a stone with Y value
-- then memoize/dp to fill it in, from number of blinks = 1 up to number of blinks = 75
-- except, number on stone could be unbounded so that part should be like a hashmap instead

main :: String -> IO ()
main contents = do
    let stones = MultiSet.fromList $ map read (words contents)
    let blinks = 75
    let blinkStones = take (blinks + 1) (unfoldr (\stones -> Just (stones, blink stones)) stones)
    let lengths = map length blinkStones

    for_ (zip [0..] lengths) print
    print $ last lengths

