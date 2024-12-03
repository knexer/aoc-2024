import System.Environment

parseInput :: String -> [[Integer]]
parseInput contents = do
    let splitContents = lines contents
    let moreSplit = map words splitContents
    map (map read) moreSplit

toDiffs :: [Integer] -> [Integer]
toDiffs [x] = []
toDiffs (x:y:ys) = y - x : toDiffs (y:ys)

isSafeWithDamper :: (Integer -> Bool) -> [Integer] -> Bool
isSafeWithDamper pred [x] = True
isSafeWithDamper pred (x:y:ys) =
    (pred x && isSafeWithDamper pred (y:ys))
      || (pred (x + y) && isSafePred pred ys)

isSafePred :: (Integer -> Bool) -> [Integer] -> Bool
isSafePred pred = foldr ((&&) . pred) True

isSafeIncreasingInterval :: Integer -> Bool
isSafeIncreasingInterval x = x >= 1 && x <= 3

isSafeDecreasingInterval :: Integer -> Bool
isSafeDecreasingInterval x = x <= -1 && x >= -3

isSafeDumb :: [Integer] -> Bool
isSafeDumb list = do
    let removeIndex index = take index list ++ drop (index + 1) list
    let lists = map removeIndex [0..length list - 1]
    let safenesses = map (isSafePred isSafeIncreasingInterval . toDiffs) lists ++ map (isSafePred isSafeDecreasingInterval . toDiffs) lists
    or safenesses

isSafeSmrt :: [Integer] -> Bool
isSafeSmrt list = isSafeWithDamper isSafeIncreasingInterval list || isSafePred isSafeIncreasingInterval (tail list)
  || isSafeWithDamper isSafeDecreasingInterval list || isSafePred isSafeDecreasingInterval (tail list)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let dumb = isSafeDumb
    let smrt = isSafeSmrt . toDiffs
    print "Dumb and smart disagree on:"
    print $ filter (\x -> dumb x /= smrt x) $ parseInput contents
    print $ length $ filter dumb $ parseInput contents
    print $ length $ filter smrt $ parseInput contents

    -- let example = [65,62,65,61]
    let example = [50,51,54,53,58]
    print $ dumb example
    print $ toDiffs example
    print $ smrt example
