module Analysing where

import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Types

-- day1
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

-- day2
passwordPolicyPattern :: String
passwordPolicyPattern = "(\\d+)\\-(\\d+)\\s(\\w)\\:\\s*(.+)"

matchPasswordPolicyPattern :: String -> [[String]]
matchPasswordPolicyPattern line = line =~ passwordPolicyPattern

policyCharacter :: String -> Char
policyCharacter line = (matchPasswordPolicyPattern line) !! 0 !! 3 !! 0

policyMin :: String -> Int
policyMin line = read $ (matchPasswordPolicyPattern line) !! 0 !! 1

policyMax :: String -> Int
policyMax line = read $ (matchPasswordPolicyPattern line) !! 0 !! 2

policyPassword :: String -> String
policyPassword line = (matchPasswordPolicyPattern line) !! 0 !! 4

countIf :: (a -> Bool) -> [a] -> Int
countIf op (x:xs)
  | op x = 1 + (countIf op xs)
  | otherwise = (countIf op xs)
countIf op [] = 0

policyMatchingChars :: String -> Int
policyMatchingChars line =
  countIf (== (policyCharacter line)) (policyPassword line)

verifyPolicy :: String -> Bool
verifyPolicy policy =
  let matchingChars = policyMatchingChars policy
      min = policyMin policy
      max = policyMax policy
   in matchingChars >= min && matchingChars <= max

verifyPolicyNew :: String -> Bool
verifyPolicyNew policy =
  let password = policyPassword policy
      char = policyCharacter policy
      firstPosChar = password !! ((policyMin policy) - 1)
      secondPosChar = password !! ((policyMax policy) - 1)
   in (firstPosChar == char) /= (secondPosChar == char)

-- day 3
sledLocs :: Slope -> [Location]
sledLocs (Slope right down) = [Location (x * right) (x * down) | x <- [0 ..]]

sledsToBottom :: Slope -> Int -> [Location]
sledsToBottom slope depth =
  takeWhile (\(Location _ down) -> down < depth) (sledLocs slope)

mapLocToWidth :: Location -> Int -> Location
mapLocToWidth (Location right down) width = Location (right `mod` width) down

countSledLocsOnTree :: TreeMap -> Slope -> Int
countSledLocsOnTree map slope =
  let sleds = sledsToBottom slope (depth map)
      w = width map
      t = trees map
   in countIf (\x -> elem (mapLocToWidth x w) t) sleds
