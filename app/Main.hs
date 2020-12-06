module Main where
import Data.List

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

day4 :: IO ()
day4 = do
  passports <- parsePassportDB "data/day4.txt"
  let result = countIf hasRequiredFields passports
  putStrLn ("passport with valid fields " ++ (show result))
  let validCt = countIf (\x -> (hasRequiredFields x) && (validatePassport x)) passports
  putStrLn ("Valid passports " ++ (show validCt))

day5 :: IO ()
day5 = do
  nrs <- parseFileLines "data/day5.txt"
  let seatnrs = map parseSeatNumber nrs
  let max = maximum $ seatnrs
  let min = minimum $ seatnrs
  let mynr = find (isMySeat seatnrs) [min..max]
  putStrLn ("Max seat number " ++ (show max))
  putStrLn ("Min seat number " ++ (show min))
  putStrLn ("My seat number " ++ (show mynr))


solve :: String -> IO ()
solve "day1" = day1
solve "day2" = day2
solve "day3" = day3
solve "day4" = day4
solve "day5" = day5
solve s = putStrLn (s ++ " Not solved")

main :: IO ()
main = do
  args <- getArgs
  solve (args !! 0)
