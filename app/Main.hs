module Main where

import System.Environment (getArgs)
import System.FilePath ((</>))

import D11.Part_1
import D11.Part_2
import D12.Part_1

exes :: Int -> Int -> String -> IO ()
exes day part
  | day == 11 && part == 1 = D11.Part_1.main
  | day == 11 && part == 2 = D11.Part_2.main
  | day == 12 && part == 1 = D12.Part_1.main
  | otherwise = undefined

main :: IO ()
main = do
    day:part:maybeFile <- getArgs
    let filename = if maybeFile == [] then "input.txt" else maybeFile !! 0
    let qualifiedFilename :: String = "src" </> "D" ++ day </> filename;
    contents <- readFile qualifiedFilename
    exes (read day) (read part) contents
