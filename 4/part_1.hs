{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
import System.Environment
import Data.Text (count, pack, unpack)
import Data.List (transpose)
import Data.Sequence (mapWithIndex)

rotate90 :: [String] -> [String]
rotate90 = transpose . reverse

rotate45 :: [String] -> [String]
rotate45 str = rotate90 skewed
  -- eww its gross though
  -- maybe we dont though
  -- but, if we do though
  -- could do each triangle separately
  -- or could we pad it and do some shifting?
  -- holy shit that fucking worked
  where
    numCols = length $ head str

    pad :: (String, Int) -> String
    pad (s, i) = replicate i '_' ++ s ++ replicate (numCols - i - 1) '_'

    skewed = map pad (zip str [0..])



-- 123
-- 456
-- 789

-- 123__
-- _456_
-- __789

-- 123__
-- _____
-- _456_
-- _____
-- __789

-- __3__
-- _2_6_
-- 1_5_9
-- _4_8_
-- __7__


-- 123
-- 456
-- 789

-- -- pad with _
-- _123_
-- _456_
-- _789_

-- -- skew it
-- 123__
-- _456_
-- __789

-- -- rotate 90
-- __1
-- _42
-- 753
-- 86_
-- 9__

-- -- pad with _, differently
-- 123__
-- _____
-- _456_
-- _____
-- __789

-- -- rotate 90
-- ____1
-- __4_2
-- 7_5_3
-- 8_6__
-- 9____

-- --skew it
-- __1__
-- _4_2_
-- 7_5_3
-- _8_6_
-- __9__

-- -- let's try even sizes
-- 12
-- 34

-- --should become
-- 1
-- 32
-- 4
-- --pad it
-- _12
-- _34
-- --skew it
-- 12_
-- _34
-- --pad it, differently
-- 12_
-- ___
-- _34
-- --rotate 90
-- __1
-- 3_2
-- 4__
-- --skew it
-- _1_
-- 3_2
-- _4_
-- -- oh, we can skip the second skew! and the second pad!

-- 123
-- 456
-- 789

-- should become:
-- 3
-- 26
-- 159
-- 48
-- 7

rotations :: [[String] -> [String]] =
    [ id
    , rotate90
    , rotate90.rotate90
    , rotate90.rotate90.rotate90
    , rotate45
    , rotate45.rotate90
    , rotate45.rotate90.rotate90
    , rotate45.rotate90.rotate90.rotate90
    ]

countXMAS :: [String] -> Int
countXMAS str = sum $ map (count (pack "XMAS") . pack) str

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)
    -- print contents
    -- print $ rotate90 contents
    -- print $ rotate45 contents
    -- print $ map ($ contents) rotations
    print $ sum $ map (countXMAS . ($ contents)) rotations

