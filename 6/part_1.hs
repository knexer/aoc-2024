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

patrolGuard :: [String] -> [String]
patrolGuard map = if done then map else patrolGuard nextMap
    where nextMap = (rotate90 . walkGuard) map
          done = hasExited map

main :: IO ()
main = do
    args <- getArgs
    contents <- lines <$> readFile (head args)
    let origMap = (rotate90 . rotate90 . rotate90) contents
    let s = rotate90 . walkGuard
    -- print origMap
    -- print $ patrolGuard origMap
    print $ sum $ map (length . filter ('X' ==)) (patrolGuard origMap)

