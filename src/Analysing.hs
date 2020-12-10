module Analysing where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
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

-- day 4
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasField :: Eq a => a -> [(a, a)] -> Bool
hasField ex ((k, v):xs) = k == ex || hasField ex xs
hasField _ _ = False

hasRequiredFields :: [(String, String)] -> Bool
hasRequiredFields passport = all (\x -> hasField x passport) requiredFields

validatePassportField :: (String, String) -> Bool
validatePassportField ("byr", v) =
  let i = read v :: Int
   in (i >= 1920) && (i <= 2002)
validatePassportField ("iyr", v) =
  let i = read v :: Int
   in (i >= 2010) && (i <= 2020)
validatePassportField ("eyr", v) =
  let i = read v :: Int
   in (i >= 2020) && (i <= 2030)
validatePassportField ("hgt", v)
  | (drop 2 v) == "in" =
    let i = (read (take 2 v)) :: Int
     in (i >= 59) && (i <= 76)
  | (drop 3 v) == "cm" =
    let i = (read (take 3 v)) :: Int
     in (i >= 150) && (i <= 193)
  | otherwise = False
validatePassportField ("hcl", v) = v =~ "#[a-f0-9]{6}"
validatePassportField ("ecl", v) =
  elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validatePassportField ("pid", v) = v =~ "^\\d{9}$"
validatePassportField ("cid", _) = True

validatePassport :: [(String, String)] -> Bool
validatePassport passport = all (== True) $ map validatePassportField passport

valueOrDefault :: (Maybe a) -> a -> a
valueOrDefault (Just x) _ = x
valueOrDefault Nothing d = d

isMySeat :: [Int] -> Int -> Bool
isMySeat others candidate =
  let l = candidate - 1
      r = candidate + 1
   in (elem l others) && (elem r others) && (not (elem candidate others))

-- day 7
lookupOrEmpty :: Ord a => a -> Map a [b] -> [b]
lookupOrEmpty key m
  | Just val <- M.lookup key m = val
  | otherwise = []

containsBagOfColor :: String -> Map String [String] -> String -> Bool
containsBagOfColor target rules original =
  let contents = lookupOrEmpty original rules
   in (elem target contents) ||
      (any (== True) $ map (containsBagOfColor target rules) contents)

countAllBags :: Map String [(Int, String)] -> String -> Int
countAllBags rules bag =
  let contents = lookupOrEmpty bag rules
      direct = foldr (+) 0 $ map fst contents
      children =
        foldr (+) 0 $
        map (\x -> ((fst x) * (countAllBags rules (snd x)))) contents
   in direct + children
