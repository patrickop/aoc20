module Day15 where

import Common
import Data.List
import Data.List.Split

parseIntRow = (map read) . (splitOn ",")

addNext :: [Int] -> [Int]
addNext (x:xs)
  | Just i <- (elemIndex x xs) = (i+1):x:xs
  | otherwise = 0:x:xs

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  let xs = reverse $ parseIntRow $ lines !! 0
  let all = iterate addNext xs
  let final = last $ takeWhile (\x -> (length x) <= 2020) all
  return $ head final

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  let xs = reverse $ parseIntRow $ lines !! 0
  let all = iterate addNext xs
  let final = last $ take 300000 all
  return $ head final
