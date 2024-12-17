module D12.Part_2 where

import Data.List (partition, transpose)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Location = (Int, Int)
type Plant = (Location, Char)
type Plot = [Plant]

isAdjacent :: Plant -> Plant -> Bool
isAdjacent ((x1, y1), _) ((x2, y2), _)
    = (x1 == x2 && abs (y1 - y2) <= 1)
    || (abs (x1 - x2) <= 1 && y1 == y2)

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
    canMerge plot1 plot2 = or (liftA2 isAdjacent plot1 plot2)

-- big pre-optimization: split the plants out by type
splitByType :: [Plant] -> [[Plant]]
splitByType plants = Map.elems (Map.fromListWith (++) keyedByType)
  where
    keyedByType :: [(Char, [Plant])] = map (\plant -> (snd plant, [plant])) plants

makePlots :: [Plant] -> [Plot]
makePlots plants = concat $ map (mergeIntoFirst . map (\x -> [x])) (splitByType plants)

pricePlot :: Int -> Plot -> Int
pricePlot size plot = area * numSides
  where
    area = length plot
    numSides = countSides size plot

-- To count fences for a plot:
-- then, find the top and bottom segments
--   first, take two adjacent rows
--   go through the adjacent rows and classify each pair as a top, bottom, or not-an edge
--   count how many times we switch types
--   do that for all the rest of the rows (recurse)
-- then, transpose the grid and do the same thing to find the left and right segments

countSides :: Int -> Plot -> Int
countSides size plot = topBottom + leftRight
  where
    asGrid = reformGrid size plot
    topBottom = (countTopBottomSides . padWithFalses) asGrid
    leftRight = (countTopBottomSides . padWithFalses . transpose) asGrid

isFirstRow :: Plant -> Bool
isFirstRow ((0, _), _) = True
isFirstRow ((_, _), _) = False

reformRow :: Int -> [Plant] -> [Bool]
reformRow size plants = map (`elem` ys) [0..size]
  where
    ys = Set.fromList (map (\((_, y), _) -> y) plants)

-- re-form a plot into a 2d grid of bools (is the plot there or not)
reformGrid :: Int -> Plot -> [[Bool]]
reformGrid _ [] = []
reformGrid size plot = (reformRow size firstRow):(reformGrid size shifted)
  where
    (firstRow, rest) = partition isFirstRow plot
    shifted = map (\((x, y), c) -> ((x - 1, y), c)) rest

data EdgeType = Top | Bottom | NotAn deriving Eq;

edgeType :: (Bool, Bool) -> EdgeType
edgeType (True, False) = Top
edgeType (False, True) = Bottom
edgeType _             = NotAn

padWithFalses :: [[Bool]] -> [[Bool]]
padWithFalses prev = ((repeat False) : prev ++ [(repeat False)])

-- then, find the top and bottom segments
--   first, take two adjacent rows
--   go through the adjacent rows and classify each pair as a top, bottom, or not-an edge
--   count how many times we switch types
--   do that for all the rest of the rows (recurse)
countTopBottomSides :: [[Bool]] -> Int
countTopBottomSides (firstRow:secondRow:rows) = numEdges + countTopBottomSides (secondRow:rows)
  where
    edgeTypes = map edgeType (zip firstRow secondRow)
    numEdges = countEdgeSpans edgeTypes
countTopBottomSides _ = 0

countEdgeSpans :: [EdgeType] -> Int
countEdgeSpans [] = 0
countEdgeSpans (NotAn:xs) = countEdgeSpans (dropWhile (== NotAn) xs)
countEdgeSpans (x:xs) = 1 + countEdgeSpans (dropWhile (== x) xs)

---- parsing ----

withCoordinates :: [[a]] -> [[((Int, Int), a)]]
withCoordinates = zipWith zip ids
  where
    ids = map rowNumberToIds [0..]
    rowNumberToIds rowNumber = map (rowNumber,) [0..]

parse :: String -> [Plant]
parse = concat . withCoordinates . lines

main :: String -> IO ()
main contents = print $ sum prices
  where
    size = (length . lines) contents
    plants = parse contents
    plots = makePlots plants
    prices = map (pricePlot size) plots
