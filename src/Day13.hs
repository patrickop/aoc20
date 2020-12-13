module Day13 where

import Common
import Text.Read
import Data.Maybe
import Data.List.Split
import Data.List

parseBusId :: String -> Maybe Int
parseBusId = readMaybe

parseBusIdsMaybe = map parseBusId . splitOn ","

parseBusIds = catMaybes . map parseBusId . splitOn ","

pairMaybe :: Int -> Maybe Int -> Maybe (Int, Int)
pairMaybe _ Nothing = Nothing
pairMaybe x (Just y) = Just (x,y)

parseBusIdsEnumerated = catMaybes . zipWith pairMaybe [0..] . parseBusIdsMaybe

busDepartures id = [0,id..]
busDeparturesWithId id = zip (repeat id) $ busDepartures id

mergeSorted :: (b -> b -> Bool) -> [(a,b)] -> [(a,b)] -> [(a,b)]
mergeSorted smaller (x:xs) (y:ys)
  | smaller (snd x) (snd y) = [x] ++ (mergeSorted smaller xs (y:ys))
  | otherwise = [y] ++ (mergeSorted smaller (x:xs) ys)
mergeSorted _ (x:xs) [] = (x:xs)
mergeSorted _ [] (y:ys) = (y:ys)
mergeSorted _ [] [] = []

generateDepartures :: [Int] -> [(Int,Int)]
generateDepartures = foldl (mergeSorted (<)) [] . map busDeparturesWithId

combinedCandidates :: Int -> [Int] -> [Int] -> [Int]
combinedCandidates neededDiff (x:xs) (y:ys) 
  | diff == neededDiff = [x] ++ (combinedCandidates neededDiff xs ys)
  | diff < neededDiff = combinedCandidates neededDiff (x:xs) ys
  | diff > neededDiff = combinedCandidates neededDiff xs (y:ys)
  where diff = y - x

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  let myTime = read $ lines !! 0 :: Int
  let busIds = parseBusIds $ lines !! 1
  let deps = generateDepartures busIds
  let maybeDep = find ((>myTime) . snd) deps
  let dep = maybe (-2) snd $ maybeDep
  let bus = maybe (-2) fst $ maybeDep
  return $ bus * (dep-myTime)

solveB :: String -> Int
solveB input =
  let busIds = parseBusIdsEnumerated input
      enumWithCandidates = map (\(x,y) -> (x, busDepartures y)) busIds
      com = foldl (\acc (x,y) -> combinedCandidates x acc y) (snd $ enumWithCandidates !! 0) (tail enumWithCandidates)
   in head com
  

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  return $ solveB $ lines !! 1
