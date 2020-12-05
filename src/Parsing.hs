module Parsing where

import Types

addone :: Int -> Int
addone x = x + 1

parseFileLines :: String -> IO [String]
parseFileLines name = do
  content <- readFile name
  return (lines content)

parseIntsFile :: String -> IO [Int]
parseIntsFile name = do
  lineList <- parseFileLines name
  return (map (read :: String -> Int) lineList)

parseTreeLocs :: Location -> [String] -> [Location]
parseTreeLocs (Location right down) (('#':cs):xs) =
  (Location right down) : (parseTreeLocs (Location (right + 1) down) (cs : xs))
parseTreeLocs (Location right down) ((_:cs):xs) =
  parseTreeLocs (Location (right + 1) down) (cs : xs)
parseTreeLocs (Location right down) ([]:xs) =
  parseTreeLocs (Location 0 (down + 1)) xs
parseTreeLocs _ [] = []

parseTreeMap :: String -> IO TreeMap
parseTreeMap name = do
  lineList <- parseFileLines name
  let depth = length lineList
  let width = length (lineList !! 0)
  let trees = parseTreeLocs (Location 0 0) lineList
  return (TreeMap width depth trees)
