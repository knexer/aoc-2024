import System.Environment

parseInput :: String -> [[Integer]]
parseInput contents = do
    let splitContents = lines contents
    let moreSplit = map words splitContents
    map (map read) moreSplit

toDiffs :: [Integer] -> [Integer]
toDiffs [x] = []
toDiffs (x:y:ys) = y - x : toDiffs (y:ys)

isSafeIncreasing :: [Integer] -> Bool
isSafeIncreasing [] = True
isSafeIncreasing (x:xs) = x >= 1 && x <= 3 && isSafeIncreasing xs

isSafeDecreasing :: [Integer] -> Bool
isSafeDecreasing [] = True
isSafeDecreasing (x:xs) = x <= -1 && x >= -3 && isSafeDecreasing xs

isSafe :: [Integer] -> Bool
isSafe list = isSafeIncreasing list || isSafeDecreasing list

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    print $ length $ filter isSafe $ map toDiffs $ parseInput contents