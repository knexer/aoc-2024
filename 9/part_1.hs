import System.Environment (getArgs)
import Debug.Trace (trace)
import Data.Maybe (isJust, fromJust, fromMaybe)

-- alternating values and whitespaces
-- could just go with their approach, and inflate, then solve, then checksum.
-- So, inflate - produce a list of Maybe Ints from the string

inflate :: Int -> String -> [Maybe Int]
inflate id (file:rest) = replicate count (Just id) ++ inflateGap (id + 1) rest
   where count :: Int = read [file]

inflateGap :: Int -> String -> [Maybe Int]
inflateGap _ [] = []
inflateGap id (gap:rest) = replicate count Nothing ++ inflate id rest
    where count :: Int = read [gap]

fillGaps :: [Maybe Int] -> [Int] -> [Int]
fillGaps [] _ = []
fillGaps (orig:origs) (filler:fillers) = if isJust orig then fromJust orig:fillGaps origs (filler:fillers) else filler:fillGaps origs fillers

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let origMap = inflate 0 contents
    let fillers = map fromJust $ reverse (filter isJust origMap)
    let midMap = fillGaps origMap fillers

    let usedSpace = length $ filter isJust origMap
    let finalMap = take usedSpace midMap

    -- print contents
    -- print $ map (fromMaybe (-1)) origMap
    -- print fillers
    -- print midMap
    -- print finalMap
    print $ sum $ zipWith (*) [0..] finalMap
