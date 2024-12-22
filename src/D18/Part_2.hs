module D18.Part_2 where

import Data.Set qualified as Set
import Utils (extractNumbers, finiteCoordinates)
import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)
import Debug.Trace (traceShow)

type Position = (Int, Int)

parse :: String -> [Position]
parse = map parsePosition . lines
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

shortestPath :: Set.Set Position -> Set.Set Position -> Set.Set Position -> Int -> Maybe Int
shortestPath valid frontier donezo steps
  | (70, 70) `elem` frontier = Just steps
  | Set.size frontier == 0 = Nothing
  | otherwise = shortestPath valid nextFrontier nextDonezo (steps + 1)
  where
    adjToFrontier = (Set.unions . Set.map adjacentLocations) frontier
    nextFrontier = Set.intersection valid $ Set.difference adjToFrontier (Set.union frontier donezo)
    nextDonezo = Set.union donezo frontier

hasPath :: [Position] -> Int -> Bool
hasPath byteDrops drops = isJust $ shortestPath valid (Set.fromList [(0, 0)]) Set.empty 0
  where valid = traceShow drops (Set.difference allLocations ((Set.fromList . take drops) byteDrops))

main :: String -> IO ()
main contents = print $ byteDrops !! (fromJust firstNoPath - 1)
  where
    byteDrops = parse contents
    hasPaths = map (hasPath byteDrops) [0..]
    firstNoPath = elemIndex False hasPaths
