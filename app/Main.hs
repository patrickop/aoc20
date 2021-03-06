module Main where

import qualified Day1 as D1
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12
import qualified Day13 as D13
import qualified Day14 as D14
import qualified Day15 as D15
import qualified Day16 as D16
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import qualified Day9 as D9
import System.Environment

solve :: String -> IO ()
solve "day1A" = do
  result <- D1.a "data/day1.txt"
  putStrLn $ show $ result
solve "day1B" = do
  result <- D1.b "data/day1.txt"
  putStrLn $ show $ result
solve "day2A" = do
  result <- D2.a "data/day2.txt"
  putStrLn $ show $ result
solve "day2B" = do
  result <- D2.b "data/day2.txt"
  putStrLn $ show $ result
solve "day3A" = do
  result <- D3.a "data/day3.txt"
  putStrLn $ show $ result
solve "day3B" = do
  result <- D3.b "data/day3.txt"
  putStrLn $ show $ result
solve "day4A" = do
  result <- D4.a "data/day4.txt"
  putStrLn $ show $ result
solve "day4B" = do
  result <- D4.b "data/day4.txt"
  putStrLn $ show $ result
solve "day5A" = do
  result <- D5.a "data/day5.txt"
  putStrLn $ show $ result
solve "day5B" = do
  result <- D5.b "data/day5.txt"
  putStrLn $ show $ result
solve "day6A" = do
  result <- D6.a "data/day6.txt"
  putStrLn $ show $ result
solve "day6B" = do
  result <- D6.b "data/day6.txt"
  putStrLn $ show $ result
solve "day7A" = do
  result <- D7.a "data/day7.txt"
  putStrLn $ show $ result
solve "day7B" = do
  result <- D7.b "data/day7.txt"
  putStrLn $ show $ result
solve "day8A" = do
  result <- D8.a "data/day8.txt"
  putStrLn $ show $ result
solve "day8B" = do
  result <- D8.b "data/day8.txt"
  putStrLn $ show $ result
solve "day9A" = do
  result <- D9.a "data/day9.txt"
  putStrLn $ show $ result
solve "day9B" = do
  result <- D9.b "data/day9.txt"
  putStrLn $ show $ result
solve "day10A" = do
  result <- D10.a "data/day10.txt"
  putStrLn $ show $ result
solve "day10B" = do
  result <- D10.b "data/day10.txt"
  putStrLn $ show $ result
solve "day11A" = do
  result <- D11.a "data/day11.txt"
  putStrLn $ show $ result
solve "day11B" = do
  result <- D11.b "data/day11.txt"
  putStrLn $ show $ result
solve "day12A" = do
  result <- D12.a "data/day12.txt"
  putStrLn $ show $ result
solve "day12B" = do
  result <- D12.b "data/day12.txt"
  putStrLn $ show $ result
solve "day13A" = do
  result <- D13.a "data/day13.txt"
  putStrLn $ show $ result
solve "day13B" = do
  result <- D13.b "data/day13.txt"
  putStrLn $ show $ result
solve "day14A" = do
  result <- D14.a "data/day14.txt"
  putStrLn $ show $ result
solve "day14B" = do
  result <- D14.b "data/day14.txt"
  putStrLn $ show $ result
solve "day15A" = do
  result <- D15.a "data/day15.txt"
  putStrLn $ show $ result
solve "day15B" = do
  result <- D15.b "data/day15.txt"
  putStrLn $ show $ result
solve "day16A" = do
  result <- D16.a "data/day16.txt"
  putStrLn $ show $ result
solve "day16B" = do
  result <- D16.b "data/day16.txt"
  putStrLn $ show $ result
solve s = putStrLn (s ++ " Not solved")

main :: IO ()
main = do
  args <- getArgs
  solve (args !! 0)
