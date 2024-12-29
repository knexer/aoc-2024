module Main where

import System.Environment (getArgs)
import System.FilePath ((</>))

import D11.Part_1 (main)
import D11.Part_2 (main)
import D12.Part_1 (main)
import D12.Part_2 (main)
import D13.Part_1 (main)
import D13.Part_2 (main)
import D14.Part_1 (main)
import D14.Part_2 (main)
import D15.Part_1 (main)
import D15.Part_2 (main)
import D17.Part_1 (main)
import D17.Part_2 (main)
import D18.Part_1 (main)
import D18.Part_2 (main)
import D19.Part_1 (main)
import D19.Part_2 (main)
import D20.Part_1 (main)

exes :: Int -> Int -> String -> IO ()
exes day part
  | day == 11 && part == 1 = D11.Part_1.main
  | day == 11 && part == 2 = D11.Part_2.main
  | day == 12 && part == 1 = D12.Part_1.main
  | day == 12 && part == 2 = D12.Part_2.main
  | day == 13 && part == 1 = D13.Part_1.main
  | day == 13 && part == 2 = D13.Part_2.main
  | day == 14 && part == 1 = D14.Part_1.main
  | day == 14 && part == 2 = D14.Part_2.main
  | day == 15 && part == 1 = D15.Part_1.main
  | day == 15 && part == 2 = D15.Part_2.main
  | day == 17 && part == 1 = D17.Part_1.main
  | day == 17 && part == 2 = D17.Part_2.main
  | day == 18 && part == 1 = D18.Part_1.main
  | day == 18 && part == 2 = D18.Part_2.main
  | day == 19 && part == 1 = D19.Part_1.main
  | day == 19 && part == 2 = D19.Part_2.main
  | day == 20 && part == 1 = D20.Part_1.main
  | otherwise = undefined

main :: IO ()
main = do
    day:part:maybeFile <- getArgs
    let filename = if maybeFile == [] then "input.txt" else maybeFile !! 0
    let qualifiedFilename :: String = "src" </> "D" ++ day </> filename;
    contents <- readFile qualifiedFilename
    exes (read day) (read part) contents
