module Common where

import Data.List.Split

parseFileLines :: String -> IO [String]
parseFileLines name = do
  content <- readFile name
  return (lines content)

countIf :: (a -> Bool) -> [a] -> Int
countIf op (x:xs)
  | op x = 1 + (countIf op xs)
  | otherwise = (countIf op xs)
countIf op [] = 0

parseIntsFile :: String -> IO [Int]
parseIntsFile name = do
  lineList <- parseFileLines name
  return (map (read :: String -> Int) lineList)

duos :: [Int] -> [[Int]]
duos values = [[i, j] | i <- values, j <- values, i < j]

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

addUpTo :: Int -> [Int] -> Bool
addUpTo goal xs = (sum xs) == goal

findCombosThatAddTo :: [[Int]] -> Int -> Maybe [Int]
findCombosThatAddTo list goal = head' (filter (addUpTo goal) list)

takeWhileList :: ([a] -> Bool) -> [a] -> [a] -> [a]
takeWhileList _ initial [] = initial
takeWhileList condition initial (x:xs)
  | not (condition newInitial) = initial
  | otherwise = takeWhileList condition newInitial xs
  where
    newInitial = initial ++ [x]

parseIntRow :: String -> [Int]
parseIntRow = (map read) . (splitOn ",")
