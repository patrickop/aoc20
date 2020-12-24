module Day15 where

import Common
import Data.List
import Data.List.Split
import qualified Data.HashMap.Strict as M

parseIntRow = (map read) . (splitOn ",")

nextHead :: ((Int, Int), (M.HashMap Int Int)) -> ((Int, Int), (M.HashMap Int Int))
nextHead (( i, v), lastOccs) 
  | Just lastOcc <- (M.lookup v lastOccs) = 
    ((i+1, (i - lastOcc)), (M.insert v i lastOccs))
  | otherwise = ((i+1, 0), (M.insert v i lastOccs))

makeFirstHead :: [Int] -> ((Int, Int), (M.HashMap Int Int))
makeFirstHead xs = (((length xs) - 1, last xs), M.fromList (zip (init xs) [0..]))

getNumberAtIteration :: Int -> String -> Int
getNumberAtIteration iterations line =
  let xs = parseIntRow $ line
      h = makeFirstHead xs
      all = iterate nextHead h
      allHeads = map (snd . fst) all
      targetIndex = iterations-(length xs)
  in  allHeads !! targetIndex

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  return $ getNumberAtIteration 2020 (lines !! 0)

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  return $ getNumberAtIteration 30000000 (lines !! 0)
