import System.Environment (getArgs)
import Data.List (transpose)

hasExited :: [String] -> Bool
hasExited = not . any ('^' `elem`)

walkGuardRow :: Bool -> String -> String
walkGuardRow _ [] = []
walkGuardRow False ('^':remainder) = 'X' : walkGuardRow True remainder
walkGuardRow False (spot:remainder) = spot : walkGuardRow False remainder
walkGuardRow True (_:'#':remainder) = '^' : '#' : remainder
walkGuardRow True (_:remainder) = 'X' : walkGuardRow True remainder

walkGuard :: [String] -> [String]
walkGuard = map (walkGuardRow False)

rotate90 :: [String] -> [String]
rotate90 = reverse . transpose

walkGuard4x :: [[String]] -> [String] -> Bool
walkGuard4x mapLibrary currentMap
  | done = False
  | repeat = True
  | otherwise = walkGuard4x (nextMap:mapLibrary) nextMap
  where
      nextMap
        = (rotate90
             . walkGuard
                 . rotate90
                     . walkGuard . rotate90 . walkGuard . rotate90 . walkGuard)
            currentMap
      done = hasExited nextMap
      repeat = nextMap `elem` mapLibrary


patrolGuard :: [String] -> [String]
patrolGuard map = if done then map else patrolGuard nextMap
    where nextMap = (rotate90 . walkGuard) map
          done = hasExited map

isCycle :: [String] -> Int -> Int -> Bool
isCycle origMap x y = walkGuard4x [] modifiedMap
    where modifiedMap = take x origMap ++ [take y origRow ++ "#" ++ drop (y + 1) origRow] ++ drop (x + 1) origMap
          origRow = origMap !! x

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)
    let origMap = (rotate90 . rotate90 . rotate90) contents
    let s = rotate90 . walkGuard
    -- print origMap
    -- print $ patrolGuard origMap
    print $ sum $ map (length . filter ('X' ==)) (patrolGuard origMap)
    print $ walkGuard4x [] origMap
    print $ isCycle origMap 0 0
    print $ isCycle origMap 0 1
    print $ length origMap
    print $ (length . filter id) $ (<*>) (map (isCycle origMap) [0..length origMap - 1]) [0..length origMap - 1]

