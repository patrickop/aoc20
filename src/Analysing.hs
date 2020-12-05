module Analysing where

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

duos :: [Int] -> [[Int]]
duos values = [[i, j] | i <- values, j <- values, i < j]

triples :: [Int] -> [[Int]]
triples values =
  [[i, j, k] | i <- values, j <- values, k <- values, i < j, j < k]

addUpTo :: Int -> [Int] -> Bool
addUpTo goal xs = (sum xs) == goal

findCombosThatAddTo :: [[Int]] -> Int -> Maybe [Int]
findCombosThatAddTo list goal = head' (filter (addUpTo goal) list)

multiply' :: Maybe [Int] -> Int
multiply' (Just xs) = product xs
