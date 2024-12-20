module D15.Part_2 where

import Data.Foldable (for_)
import Utils (zipWithCoords)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Debug.Trace (traceShowId, traceShow)

type Warehouse = [String]

type Move = Char

-- parse input
parse :: String -> (Warehouse, [Move])
parse input = (warehouse, concat $ drop 1 moves)
  where
    (warehouse, moves) = span (/= "") $ lines input

widen :: Warehouse -> Warehouse
widen (row:rows) = concatMap widenSpot row:widen rows
widen [] = []

widenSpot :: Char -> [Char]
widenSpot '.' = ".."
widenSpot '@' = "@."
widenSpot '#' = "##"
widenSpot 'O' = "[]"
widenSpot _ = undefined

-- iterate the map to the end
-- iterate the map with one instruction
moveBot :: Move -> Warehouse -> Warehouse
moveBot nextMove warehouse
  | nextMove == '>' = moveRight warehouse
  | nextMove == '<' = (map reverse . moveRight . map reverse) warehouse
  | nextMove == 'v' = moveDown warehouse
  | nextMove == '^' = (reverse . moveDown . reverse) warehouse
  | otherwise = undefined

-- this one harder
-- two passes - canMoveDown then actuallyMoveDown?
-- or - those can be the same, but with a tuple return type

moveDown :: Warehouse -> Warehouse
moveDown warehouse = fromMaybe warehouse (pushDown (repeat Nothing) warehouse)

pushDown :: [Maybe Char] -> Warehouse -> Maybe Warehouse
pushDown _ [] = Just []
pushDown pushingCols (row:rows)
  | canPushRow && isJust pushedRest = Just (pushedRow:fromJust pushedRest)
  | otherwise = Nothing
  where
    npc = nextPushingCols (zip pushingCols row)
    pushedRest = pushDown npc rows
    blockedPushes = map (\(p1, r1) -> (isJust p1 && r1 == '#')) (zip pushingCols row)
    canPushRow :: Bool = (not . or) blockedPushes
    pushedRow = map (\(new, prev, current) -> if new then '.' else (fromMaybe current prev)) (zip3 newlyPushedCols pushingCols row)
    newlyPushedCols :: [Bool] = map (\(next, prev) -> isJust next && (not . isJust) prev) (zip npc pushingCols)
    -- expand pushingCols if there's a robot
    -- expand pushingCols left or right if there's half of a box where there's a True
    -- shrink pushingCols if there's a gap (right?)
    -- return bool is false if any pushingCols are on a wall, otherwise recurse
    -- return bool is true if we are at the bottom (base case)

nextPushingCols :: [(Maybe Char, Char)] -> [Maybe Char]
nextPushingCols ((p1, r1):(p2, r2):rest)
  | pushBox == True = Just r1:Just r2:nextPushingCols rest
  | otherwise = pushFirst:nextPushingCols ((p2, r2):rest)
  where
    isRobot = r1 == '@'
    isBox = r1 == '['
    -- isWall = r1 == '#'
    -- isGap = r1 == '.'
    pushBox = isBox && (isJust p1 || isJust p2)
    pushFirst = if isRobot || pushBox then Just r1 else Nothing
nextPushingCols [(_, r1)] = [(if r1 == '@' then Just '@' else Nothing),Nothing]
nextPushingCols [] = []

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
    gps spot
      | snd spot == '[' = (fst . fst) spot + 100 * (snd . fst) spot
      | otherwise = 0

main :: String -> IO ()
main contents = do
  let (warehouse, moves) = parse contents
  let thiccHouse = widen warehouse
  let movers :: [Warehouse -> Warehouse] = (map moveBot) moves

  for_ thiccHouse print
  print "becomes"

  let finalWarehouse = (foldr (flip (.)) id movers) thiccHouse
  for_ finalWarehouse print
  print $ calcGPS finalWarehouse
