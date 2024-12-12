import System.Environment (getArgs)
import Debug.Trace (trace, traceShow, traceShowId)
import Data.Maybe (isJust, fromJust, fromMaybe)

-- alternating values and whitespaces
-- could just go with their approach, and inflate, then solve, then checksum.
-- So, inflate - produce a list of Maybe Ints from the string

-- this version might actually be easier to solve in the file-gap-file formulation
-- but, we need to keep track of IDs too.
-- so what if we didn't then?
-- the biggest tricky part is that we don't know the final size...
-- well that just means we have to solve smarter, that's OK, we can do that.

solveSmarter :: Int -> [Maybe Int] -> [Maybe Int] -> [Maybe Int]
solveSmarter _ [] back = back -- empty front, everything has been compacted into back
solveSmarter (-1) front back = front ++ back -- found the last id, done
solveSmarter id front back = solveSmarter nextId (fromMaybe seekedFront replacedFront) finalBack
  where
    (!seekedFront, found, skipped) = seekId id front
    !replacedFront = replaceGap (length found) seekedFront id
    replacedFound = if isJust replacedFront then replicate (length found) Nothing else found
    !finalBack = replacedFound ++ skipped ++ back
    nextId = traceShowId id - 1

-- for each id, scanning from the right
-- find a gap, scanning from the left, that fits it, or find the id
-- move it into that gap

-- Search for id in list, and split list in three:
-- the values before the id (skipped)
-- the length of the span that is the id (count)
-- the values after the id (remainder)
seekId :: Int -> [Maybe Int] -> ([Maybe Int], [Maybe Int], [Maybe Int])
seekId id front = (before, during, after)
  where
    (before, remainder) = span (/= Just id) front
    (during, after) = span (== Just id) remainder

-- returns a new list, with the first gap of size size replaced with replacement,
-- or Nothing if there is no big enough gap
replaceGap :: Int -> [Maybe Int] -> Int -> Maybe [Maybe Int]
replaceGap size [] _ = Nothing
replaceGap size list replacement
  | gapSize < size = (take dropForRecurse list ++) <$> replaceGap size (drop dropForRecurse list) replacement
  | otherwise = Just (replicate size (Just replacement) ++ drop size list)
  where gapSize = length $ takeWhile (== Nothing) list
        dropForRecurse = length $ takeWhile (== head list) list

-- old outdated big picture thoughts:
-- so, the list could be split into two - the already processed right side, and the yet to be processed left
-- BUT the left keeps getting mutated, it's not just a scanning thing
-- so, in the recursive case, you have a front, a file (with id and length), and a back
-- search the front for a place to put the file
-- if you find one, put it there, now you have a new front
-- if not, put it in front of back, now you have a new back
-- now decrement to get the next ID
-- move things from front to back until you find said ID; this is final back for next recursion
-- remove things from front until you don't find said ID; this is final front for next recursion
-- you also know the new file's length
-- then recurse with the new front, file, and back

-- base case: empty front. Then at that point, back is your final answer.

inflate :: Int -> String -> [Maybe Int]
inflate id (file:rest) = replicate count (Just id) ++ inflateGap (id + 1) rest
   where count :: Int = read [file]

inflateGap :: Int -> String -> [Maybe Int]
inflateGap _ [] = []
inflateGap id (gap:rest) = replicate count Nothing ++ inflate id rest
    where count :: Int = read [gap]

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let origMap = inflate 0 contents
    let maxId = fromJust $ foldr max Nothing origMap
    print maxId

    -- print $ replaceGap 5 [Just 1, Nothing, Nothing, Just 0, Nothing, Nothing, Nothing, Nothing, Just 4] 2

    -- print $ seekId 1 (reverse [Nothing, Nothing, Just 0, Just 1, Just 1, Just 2])

    -- print $ solveSmarter 1 [Nothing, Nothing, Just 0, Just 1, Just 1, Just 2] []

    let finalMap = solveSmarter maxId origMap []
    -- print origMap
    -- print ""
    -- print $ seekId 9 (reverse origMap)
    -- print ""
    print finalMap

    -- print contents
    -- print $ map (fromMaybe (-1)) origMap
    -- print fillers
    -- print midMap
    -- print finalMap
    print ""
    print $ sum $ zipWith (*) [0..] (map (fromMaybe 0) finalMap)
