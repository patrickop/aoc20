module Day11 where

import Common
import Control.Monad (join)
import Data.List
import qualified Data.Map as M
import Data.Maybe

data Field
  = Empty
  | Floor
  | Taken
  deriving (Eq, Show)

data Coordinate =
  Coordinate
    { right :: Int
    , down :: Int
    }
  deriving (Eq, Show, Ord)

type Area = M.Map Coordinate Field

type InfluencersFn = Area -> Coordinate -> Int

parseField :: Char -> Field
parseField 'L' = Empty
parseField '#' = Taken
parseField '.' = Floor

parseRow = map parseField

parseRows = map parseRow

width :: [[Field]] -> Int
width (x:xs) = length x

height :: [[Field]] -> Int
height = length

allCoords :: [[Field]] -> [Coordinate]
allCoords rows = [Coordinate r d | r <- [0 .. w - 1], d <- [0 .. h - 1]]
  where
    w = width rows
    h = height rows

getField :: [[Field]] -> Coordinate -> Field
getField area coord = area !! (down coord) !! (right coord)

toArea :: [[Field]] -> Area
toArea rows = M.fromList $ map (\x -> (x, getField rows x)) (allCoords rows)

getNeighbours :: Area -> Coordinate -> [Field]
getNeighbours area (Coordinate r d) =
  catMaybes $
  [M.lookup (Coordinate (r - 1) (d - 1)) area] ++
  [M.lookup (Coordinate (r - 1) (d)) area] ++
  [M.lookup (Coordinate (r - 1) (d + 1)) area] ++
  [M.lookup (Coordinate (r) (d - 1)) area] ++
  [M.lookup (Coordinate (r) (d + 1)) area] ++
  [M.lookup (Coordinate (r + 1) (d - 1)) area] ++
  [M.lookup (Coordinate (r + 1) (d)) area] ++
  [M.lookup (Coordinate (r + 1) (d + 1)) area]

nextValue :: Int -> Field -> Int -> Field
nextValue _ Empty takenNeighbours
  | takenNeighbours == 0 = Taken
  | otherwise = Empty
nextValue tolerance Taken takenNeighbours
  | takenNeighbours >= tolerance = Empty
  | otherwise = Taken
nextValue tolerance Floor _ = Floor

tickCoord :: Int -> InfluencersFn -> Area -> Coordinate -> Field
tickCoord tolerance influencers area coord
  | Just f <- (M.lookup coord area) =
    nextValue tolerance (f) (influencers area coord)
  | otherwise = error "not implemented"

tick :: Int -> InfluencersFn -> Area -> Area
tick tolerance influencers area =
  M.mapWithKey (\k _ -> (tickCoord tolerance influencers area k)) area

allFutureStates tolerance influencers = iterate (tick tolerance influencers)

lastTwoElemsDiffer :: Eq (a) => [a] -> Bool
lastTwoElemsDiffer [x0, x1] = x0 /= x1
lastTwoElemsDiffer [x0] = True
lastTwoElemsDiffer [] = True
lastTwoElemsDiffer (x:xs) = lastTwoElemsDiffer xs

countOccupied = length . filter (== Taken) . map snd . M.toList

coordsInDirection :: Int -> Int -> Coordinate -> [Coordinate]
coordsInDirection r d (Coordinate r0 d0) =
  [Coordinate (r0 + (r * i)) (d0 + (d * i)) | i <- [1 ..]]

isFloor :: Maybe Field -> Bool
isFloor (Just field) = field == Floor
isFloor _ = False

isNotFloor = not . isFloor

getFirstNonFloor :: Area -> [Coordinate] -> Maybe Field
getFirstNonFloor area coords =
  join $ find isNotFloor $ map (\x -> M.lookup x area) coords

getVisible :: Area -> Coordinate -> [Field]
getVisible area coord =
  catMaybes $
  [getFirstNonFloor area $ coordsInDirection (-1) (-1) coord] ++
  [getFirstNonFloor area $ coordsInDirection (-1) (0) coord] ++
  [getFirstNonFloor area $ coordsInDirection (-1) (1) coord] ++
  [getFirstNonFloor area $ coordsInDirection (0) (-1) coord] ++
  [getFirstNonFloor area $ coordsInDirection (0) (1) coord] ++
  [getFirstNonFloor area $ coordsInDirection (1) (-1) coord] ++
  [getFirstNonFloor area $ coordsInDirection (1) (0) coord] ++
  [getFirstNonFloor area $ coordsInDirection (1) (1) coord]

occupiedNeighbours area = countIf (== Taken) . getNeighbours area

occupiedVisible area = countIf (== Taken) . getVisible area

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  let area = toArea $ parseRows lines
  let allStates = allFutureStates 4 occupiedNeighbours area
  let allNonrepeatingStates = takeWhileList lastTwoElemsDiffer [] allStates
  let stableState = last allNonrepeatingStates
  return $ countOccupied stableState

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  let area = toArea $ parseRows lines
  let allStates = allFutureStates 5 occupiedVisible area
  let allNonrepeatingStates = takeWhileList lastTwoElemsDiffer [] allStates
  let stableState = last allNonrepeatingStates
  return $ countOccupied stableState
