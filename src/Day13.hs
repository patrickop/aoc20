module Day13 where

import Common
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

parseBusId :: String -> Maybe Int
parseBusId = readMaybe

parseBusIdsMaybe = map parseBusId . splitOn ","

parseBusIds = catMaybes . map parseBusId . splitOn ","

pairMaybe :: Int -> Maybe Int -> Maybe (Int, Int)
pairMaybe _ Nothing = Nothing
pairMaybe x (Just y) = Just (x, y)

parseBusIdsEnumerated = catMaybes . zipWith pairMaybe [0 ..] . parseBusIdsMaybe

busDepartures id = [0,id ..]

busDeparturesWithId id = zip (repeat id) $ busDepartures id

mergeSorted :: (b -> b -> Bool) -> [(a, b)] -> [(a, b)] -> [(a, b)]
mergeSorted smaller (x:xs) (y:ys)
  | smaller (snd x) (snd y) = [x] ++ (mergeSorted smaller xs (y : ys))
  | otherwise = [y] ++ (mergeSorted smaller (x : xs) ys)
mergeSorted _ (x:xs) [] = (x : xs)
mergeSorted _ [] (y:ys) = (y : ys)
mergeSorted _ [] [] = []

generateDepartures :: [Int] -> [(Int, Int)]
generateDepartures = foldl (mergeSorted (<)) [] . map busDeparturesWithId

-- Gives an arithmetic progression (base, step) that describes
-- all candidates that arise from the input progression
-- and the other bus schedule oa a given offset
-- (offset, schedule)
commonCandidates :: (Int, Int) -> (Int, Int) -> (Int, Int)
commonCandidates (base, step) (offset, other) =
  ( head $
    filter (\x -> (mod (x + offset) other) == 0) $ [base,(base + step) ..]
  , step * other)

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  let myTime = read $ lines !! 0 :: Int
  let busIds = parseBusIds $ lines !! 1
  let deps = generateDepartures busIds
  let maybeDep = find ((> myTime) . snd) deps
  let dep = maybe (-2) snd $ maybeDep
  let bus = maybe (-2) fst $ maybeDep
  return $ bus * (dep - myTime)

solveB :: String -> Int
solveB input =
  let busIds = parseBusIdsEnumerated input
      h = snd $ head busIds
      t = tail busIds
   in fst $ foldl commonCandidates (0, h) t

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  return $ solveB $ lines !! 1
