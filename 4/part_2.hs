import System.Environment
import Data.Text (count, pack, unpack)
import Data.List (transpose)
import Data.Sequence (mapWithIndex)

hasXMas :: [String] -> Int -> Int -> Bool
hasXMas puzzle row col = hasA && isMas topLeft botRight && isMas topRight botLeft
    where
        hasA = puzzle !! row !! col == 'A'
        topLeft = puzzle !! (row - 1) !! (col - 1)
        topRight = puzzle !! (row - 1) !! (col + 1)
        botLeft = puzzle !! (row + 1) !! (col - 1)
        botRight = puzzle !! (row + 1) !! (col + 1)
        isMas first second = (first == 'M' && second == 'S') || (first == 'S' && second == 'M')

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)
    let middleElements = [1..length contents - 2]
    let found = hasXMas contents <$> middleElements <*> middleElements
    print $ (length . filter id) found
