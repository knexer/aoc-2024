{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
import System.Environment (getArgs)
import qualified Data.Set as Set
import qualified Data.List as List

getAntinodeGens :: [(Int, Int)] -> [Int -> (Int, Int)]
getAntinodeGens [node] = []
getAntinodeGens (node:nodes) = withCurrent ++ rest
   where withCurrent = map (enumerateAntinodes node) nodes
         rest = getAntinodeGens nodes

enumerateAntinodes :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
enumerateAntinodes (x1, y1) (x2, y2) i = (x1 + i * fst from1to2, y1 + i * snd from1to2)
    where from1to2 = (x2 - x1, y2 - y1)

getNodes :: [String] -> Char -> [(Int, Int)]
getNodes [] _ = []
getNodes (row:rows) frequency = map (0,) indices ++ incrementedIndices
    where indices = List.elemIndices frequency row
          incrementedIndices = map (\(x, y) -> (x + 1, y)) (getNodes rows frequency)

generateAntinodes :: Int -> (Int -> (Int, Int)) -> [(Int, Int)]
generateAntinodes size generator = positive ++ negative
    where positive = takeWhile (inBounds size) (map generator [0,1..])
          negative = takeWhile (inBounds size) (map generator [0,-1..])

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
    let antinodeGens = concatMap getAntinodeGens nodes
    let antinodes = Set.fromList $ concatMap (generateAntinodes (length contents)) antinodeGens
    print $ length antinodes
