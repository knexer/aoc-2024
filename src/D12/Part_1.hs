module D12.Part_1 where

import Data.List (partition, transpose)
import qualified Data.Map as Map
import Debug.Trace (traceShowId)

-- strats
-- merge the plots
-- get perimiter for each plant, and area for each plant, and merge those
-- how to figure out what to merge? and what does merging mean?
-- suppose a plot is a set of plants; merge is union
-- then to detect whether to merge, they must be: a) the same type of plant, and b) have two adjacent plants

-- get perim for each plant, and area for each plant, then walk over each plot
-- and remove it from the map

type Plant = ((Int, Int), Char, Int)
type Plot = [Plant]

isAdjacent :: Plant -> Plant -> Bool
isAdjacent ((x1, y1), _, _) ((x2, y2), _, _)
    = (x1 == x2 && abs (y1 - y2) <= 1)
    || (abs (x1 - x2) <= 1 && y1 == y2)

canMerge :: Plot -> Plot -> Bool
canMerge plot1 plot2 = or (liftA2 isAdjacent plot1 plot2)

-- so, to merge
-- imperative style:
-- start with a list of unmerged plants (plots of area 1)

-- grab the first
-- try to merge it with each other plot
-- repeat this until no merges happen
-- recurse with the new second plot
mergeIntoFirst :: [Plot] -> [Plot]
mergeIntoFirst [] = []
mergeIntoFirst (first:rest)
  | length mergeable > 0 = mergeIntoFirst (biggerFirst:unmergeable)
  | otherwise = first : mergeIntoFirst rest
  where
    (mergeable, unmergeable) = partition (canMerge first) rest
    biggerFirst = concat (first:mergeable)

-- big pre-optimization: split the plants out by type
splitByType :: [Plant] -> [[Plant]]
splitByType plants = Map.elems (Map.fromListWith (++) keyedByType)
  where
    second (_, x, _) = x
    keyedByType :: [(Char, [Plant])] = map (\plant -> (second plant, [plant])) plants

makePlots :: [Plant] -> [Plot]
makePlots plants = concat $ map (mergeIntoFirst . map (\x -> [x])) (splitByType plants)

scorePlot :: Plot -> Int
scorePlot plot = area * perimeter
  where
    area = traceShowId (length plot)
    perimeter = traceShowId (sum $ map (\plant -> third plant) plot)
    third (_, _, x) = x

---- parsing ----

withCoordinates :: [[a]] -> [[((Int, Int), a)]]
withCoordinates = zipWith zip ids
  where
    ids = map rowNumberToIds [0..]
    rowNumberToIds rowNumber = map (rowNumber,) [0..]

addFencesToRowRec :: [(Char, Int)] -> [(Char, Int)]
addFencesToRowRec [] = []
addFencesToRowRec [x] = [(fst x, snd x + 1)]
addFencesToRowRec (x:y:ys)
  | fst x == fst y = x : addFencesToRowRec (y : ys)
  | otherwise      = (fst x, snd x + 1) : addFencesToRowRec ((fst y, snd y + 1) : ys)

addFencesToRow :: [(Char, Int)] -> [(Char, Int)]
addFencesToRow [] = []
addFencesToRow (x:xs) = addFencesToRowRec ((fst x, snd x + 1):xs)

parse :: String -> [Plant]
parse input = map flattenPlants withCoords
  where
    rows = lines input
    withRowFences = (map (addFencesToRow . map (,0)) rows)
    withFences = transpose (map addFencesToRow (transpose withRowFences))
    withCoords = concat (withCoordinates withFences)
    flattenPlants notPlant = (fst notPlant, fst (snd notPlant), snd (snd notPlant))

main :: String -> IO ()
main contents = print $ sum scores
  where
    plants = parse contents
    plots = makePlots plants
    scores = map scorePlot plots
