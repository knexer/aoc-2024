import System.Environment (getArgs)
import Data.List
import qualified Data.Set as Set

-- input: a map
-- output: the sum of the scores of the trailheads

-- cellular automata kind of solution? Like start from the 9s, and propagate out
-- each spot in the map has a height and a number of peaks reachable from it
-- the height are always the same, but the reachable peaks spread out

data Location = Location {
    height :: Int,
    peaks :: Set.Set (Int, Int)
} deriving Show

locFromChar :: ((Int, Int), Char) -> Location
locFromChar (id, '9') = Location { height = 9, peaks = Set.fromList [id] }
locFromChar (id, x) = Location { height = read [x], peaks = Set.empty }

visitLoc :: Location -> Location -> Location
visitLoc from to = Location {height = height to, peaks = Set.union (peaks from) (peaks to)}

rotate90 :: [[a]] -> [[a]]
rotate90 = transpose . reverse

spread :: Int -> [[Location]] -> [[Location]]
spread targetHeight startingMap = iterate rotateAndSpread startingMap !! 4
  where
    rotateAndSpread = rotate90 . map (spreadRow targetHeight)

spreadRow :: Int -> [Location] -> [Location]
spreadRow _ [] = []
spreadRow _ [lastLoc] = [lastLoc]
spreadRow targetHeight (from:to:rest)
  | height from == targetHeight && height to == targetHeight - 1 = from:visitLoc from to:spreadRow targetHeight rest
  | otherwise = from:spreadRow targetHeight (to:rest)

rotations =
    [id
    ,rotate90
    ,rotate90.rotate90
    ,rotate90.rotate90.rotate90
    ]

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)
    let rowNumberToIds rowNumber = map (rowNumber,) [0..]
    let ids :: [[(Int, Int)]] = map rowNumberToIds [0..]
    let contentsWithIndex :: [[((Int, Int), Char)]] = zipWith zip ids contents

    let startingMap = (locFromChar <$>) <$> contentsWithIndex -- TODO should be unique integers for each
    -- zip with indices? or... randomly generate? what do

    -- print startingMap
    let spreaders = map spread [1,2..9]
    let spreader = foldr (.) id spreaders
    let spreadMap = spreader startingMap

    -- print spreadMap
    let trailheads = concatMap (filter (\loc -> height loc == 0)) spreadMap
    print trailheads
    let scores = map (Set.size . peaks) trailheads
    print scores
    print $ sum scores
