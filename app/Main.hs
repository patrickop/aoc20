module Main where

import Analysing
import Parsing
import System.Environment
import Types

day1 :: IO ()
day1 = do
  numbers <- parseIntsFile "data/day1.txt"
  let t1 = duos numbers
  let t2 = triples numbers
  let combinationT1 = findCombosThatAddTo t1 2020
  let combinationT2 = findCombosThatAddTo t2 2020
  let resultT1 = multiply' combinationT1
  let resultT2 = multiply' combinationT2
  putStrLn ("duos: " ++ (show resultT1))
  putStrLn ("triples: " ++ (show resultT2))

day2 :: IO ()
day2 = do
  policies <- parseFileLines "data/day2.txt"
  let validOld = countIf verifyPolicy policies
  let validNew = countIf verifyPolicyNew policies
  putStrLn ("Valid policies(old): " ++ (show validOld))
  putStrLn ("Valid policies(new): " ++ (show validNew))

day3 :: IO ()
day3 = do
  tmap <- parseTreeMap "data/day3.txt"
  putStrLn
    ("Encountered trees 3/1 slope " ++
     (show (countSledLocsOnTree tmap (Slope 3 1))))
  let slopes = [(Slope 1 1), (Slope 3 1), (Slope 5 1), (Slope 7 1), (Slope 1 2)]
  let counts = map (\slope -> countSledLocsOnTree tmap slope) slopes
  let p = product counts
  putStrLn ("Combined treecount: " ++ (show p))

solve :: String -> IO ()
solve "day1" = day1
solve "day2" = day2
solve "day3" = day3
solve s = putStrLn (s ++ " Not solved")

main :: IO ()
main = do
  args <- getArgs
  solve (args !! 0)
