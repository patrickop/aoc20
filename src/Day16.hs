module Day16 where

import Common
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Regex.PCRE

type Range = (Int, Int)

data Rule =
  Rule
    { name :: String
    , ranges :: [Range]
    }
  deriving (Eq, Show)

type Ticket = [Int]

upper = snd

lower = fst

ruleRegex = "([\\s\\w]+): (\\d+)\\-(\\d+) or (\\d+)\\-(\\d+)"

parseRule :: String -> Maybe Rule
parseRule s
  | s =~ ruleRegex =
    let m = (s =~ ruleRegex) !! 0
     in Just $
        Rule
          (m !! 1)
          [ ((read $ m !! 2), (read $ m !! 3))
          , ((read $ m !! 4), (read $ m !! 5))
          ]
  | otherwise = Nothing

parseTicket = parseIntRow

getRanges :: [Rule] -> [Range]
getRanges (x:xs) = [(ranges x) !! 0, (ranges x) !! 1] ++ (getRanges xs)
getRanges [] = []

matchesRange :: Int -> Range -> Bool
matchesRange v r = (v >= (lower r)) && (v <= (upper r))

matchesAnyRange :: [Range] -> Int -> Bool
matchesAnyRange rs v = any (matchesRange v) rs

parseRules =
  catMaybes .
  map parseRule . head . splitWhen (== "your ticket:") . filter (/= "")

parseMyTicket =
  parseTicket . head . last . splitWhen (== "your ticket:") . filter (/= "")

parseOtherTickets =
  map parseTicket . last . splitWhen (== "nearby tickets:") . filter (/= "")

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  let rules = parseRules lines
  let myTicket = parseMyTicket lines
  let otherTickets = parseOtherTickets lines
  let ranges = getRanges rules
  let allNumbers = foldl (++) [] (myTicket : otherTickets)
  let badNumbers = filter (not . (matchesAnyRange ranges)) allNumbers
  return $ sum badNumbers

discardInvalid :: [Rule] -> [Ticket] -> [Ticket]
discardInvalid rs ts = filter (all (matchesAnyRange $ getRanges rs)) ts

getPossibleIndicesFor :: [Ticket] -> Rule -> [Int]
getPossibleIndicesFor ts (Rule n rs) =
  let fieldCount = length $ head ts
      fieldValues = map (\x -> map (!! x) ts) [0 .. fieldCount - 1]
   in findIndices (all (matchesAnyRange rs)) fieldValues

removeValueFromAll v = map (filter (/= v))

getValidIndexCombination :: [[Int]] -> Maybe [Int]
getValidIndexCombination ((x:xs):ys)
  | any null valueRemoved = getValidIndexCombination $ xs : ys
  | Just tailValidCombination <- getValidIndexCombination valueRemoved =
    Just $ x : tailValidCombination
  | otherwise = getValidIndexCombination $ xs : ys
  where
    valueRemoved = removeValueFromAll x ys
getValidIndexCombination ([]:ys) = Nothing
getValidIndexCombination [] = Just []

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  let rules = parseRules lines
  let myTicket = parseMyTicket lines
  let otherTickets = parseOtherTickets lines
  let allTickets = discardInvalid rules $ myTicket : otherTickets
  return $
    product $
    map (\x -> myTicket !! x) $
    map snd $
    filter (\x -> isPrefixOf "departure" (fst x)) $
    zip (map name rules) $
    fromJust $
    getValidIndexCombination $ map (getPossibleIndicesFor allTickets) rules
