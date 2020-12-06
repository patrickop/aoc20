module Main where

import Solver
import System.Environment

solve :: String -> IO ()
solve "day1A" = do
  result <- day1A "data/day1.txt"
  putStrLn $ show $ result
solve "day1B" = do
  result <- day1B "data/day1.txt"
  putStrLn $ show $ result
solve "day2A" = do
  result <- day2A "data/day2.txt"
  putStrLn $ show $ result
solve "day2B" = do
  result <- day2B "data/day2.txt"
  putStrLn $ show $ result
solve "day3A" = do
  result <- day3A "data/day3.txt"
  putStrLn $ show $ result
solve "day3B" = do
  result <- day3B "data/day3.txt"
  putStrLn $ show $ result
solve "day4A" = do
  result <- day4A "data/day4.txt"
  putStrLn $ show $ result
solve "day4B" = do
  result <- day4B "data/day4.txt"
  putStrLn $ show $ result
solve "day5A" = do
  result <- day5A "data/day5.txt"
  putStrLn $ show $ result
solve "day5B" = do
  result <- day5B "data/day5.txt"
  putStrLn $ show $ result
solve "day6A" = do
  result <- day6A "data/day6.txt"
  putStrLn $ show $ result
solve "day6B" = do
  result <- day6B "data/day6.txt"
  putStrLn $ show $ result
solve s = putStrLn (s ++ " Not solved")

main :: IO ()
main = do
  args <- getArgs
  solve (args !! 0)
