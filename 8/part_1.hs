{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
import System.Environment (getArgs)
import qualified Data.Set as Set
import qualified Data.List as List

getAntinodes :: [(Int, Int)] -> Set.Set (Int, Int)
getAntinodes [node] = Set.empty
getAntinodes (node:nodes) =  Set.union withCurrent rest
   where withCurrent = Set.fromList $ concatMap (getAntinodesPair node) nodes
         rest = getAntinodes nodes

getAntinodesPair :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAntinodesPair (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = []
    | otherwise = [(x1 + fst difference, y1 + snd difference), (x2 - fst difference, y2 - snd difference)]
    where difference = (x1 - x2, y1 - y2)

getNodes :: [String] -> Char -> [(Int, Int)]
getNodes [] _ = []
getNodes (row:rows) frequency = map (0,) indices ++ incrementedIndices
    where indices = List.elemIndices frequency row
          incrementedIndices = map (\(x, y) -> (x + 1, y)) (getNodes rows frequency)

inBounds :: Int -> (Int, Int) -> Bool
inBounds max (x, y) = x >= 0 && y >= 0 && x < max && y < max

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)

    let frequencies = Set.toList $ Set.fromList $ concatMap (filter ('.' /=)) contents
    print frequencies
    let nodes = map (getNodes contents) frequencies
    print nodes
    let antinodes = foldr (Set.union . getAntinodes) Set.empty nodes
    print $ length $ filter (inBounds (length contents)) $ Set.toList antinodes
