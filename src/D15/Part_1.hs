module D15.Part_1 where

import Data.Foldable (for_)
import Data.List (transpose)
import Utils (zipWithCoords)

type Warehouse = [String]

type Move = Char

-- parse input
parse :: String -> (Warehouse, [Move])
parse input = (warehouse, concat $ drop 1 moves)
  where
    (warehouse, moves) = span (/= "") $ lines input

-- iterate the map to the end
-- iterate the map with one instruction
moveBot :: Move -> Warehouse -> Warehouse
moveBot nextMove warehouse
  | nextMove == '>' = moveRight warehouse
  | nextMove == '<' = (map reverse . moveRight . map reverse) warehouse
  | nextMove == 'v' = (transpose . moveRight . transpose) warehouse
  | nextMove == '^' = (reverse . transpose . moveRight . transpose . reverse) warehouse
  | otherwise = undefined

moveRight :: Warehouse -> Warehouse
moveRight = map moveRightRow

-- find the next gap after the robot, move it before the robot
-- if there is no such gap, `gap` will be []
moveRightRow :: String -> String
moveRightRow row
  | length beforeGap < length beforeWall = beforeBot ++ gap ++ beforeGap ++ afterGap
  | otherwise = row
  where
    (beforeBot, notBeforeBot) = span (/= '@') row
    (beforeGap, notBeforeGap) = span (/= '.') notBeforeBot
    (beforeWall, _) = span (/= '#') notBeforeBot
    gap = take 1 notBeforeGap
    afterGap = drop 1 notBeforeGap

-- calculate GPS
-- calculate GPS of each box
calcGPS :: Warehouse -> Int
calcGPS warehouse = (sum . map (sum . map gps)) withCoordinates
  where
    withCoordinates = zipWithCoords warehouse
    gps spot | snd spot == 'O' = (fst . fst) spot + 100 * (snd . fst) spot
             | otherwise = 0

main :: String -> IO ()
main contents = do
  let (warehouse, moves) = parse contents
  let movers :: [Warehouse -> Warehouse] = (reverse . map moveBot) moves

  for_ ((foldr (.) id movers) warehouse) print
  print $ calcGPS $ (foldr (.) id movers) warehouse
  print ""
