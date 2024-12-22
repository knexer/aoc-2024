module D18.Part_1 where

import Data.Set qualified as Set
import Utils (extractNumbers, finiteCoordinates)

type Position = (Int, Int)

parse :: String -> Set.Set Position
parse = Set.fromList . take 1024 . map parsePosition . lines
  where
    parsePosition position = (numbers !! 0, numbers !! 1)
      where
        numbers = extractNumbers position

allLocations :: Set.Set Position
allLocations = Set.fromList . concat $ finiteCoordinates 70 70

adjacentLocations :: Position -> Set.Set Position
adjacentLocations pos =
  Set.fromList
    [ (fst pos + 1, snd pos),
      (fst pos - 1, snd pos),
      (fst pos, snd pos - 1),
      (fst pos, snd pos + 1)
    ]

shortestPath :: Set.Set Position -> Set.Set Position -> Set.Set Position -> Int -> Int
shortestPath valid frontier donezo steps
  | (70, 70) `elem` frontier = steps
  | otherwise = shortestPath valid nextFrontier nextDonezo (steps + 1)
  where
    adjToFrontier = (Set.unions . Set.map adjacentLocations) frontier
    nextFrontier = Set.intersection valid $ Set.difference adjToFrontier (Set.union frontier donezo)
    nextDonezo = Set.union donezo frontier

main :: String -> IO ()
main contents = print pathLength
  where
    valid = Set.difference allLocations (parse contents)
    pathLength = shortestPath valid (Set.fromList [(0, 0)]) Set.empty 0
