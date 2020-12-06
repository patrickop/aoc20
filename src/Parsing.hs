module Parsing where

import Types
import Data.List.Split
import Data.List

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

combineUnseperatedLines :: [String] -> [String]
combineUnseperatedLines ls = 
  let entries = splitWhen (=="") ls
  in  map (intercalate " ") entries

parseKeyValue :: String -> (String, String)
parseKeyValue s =
  let elems = splitOn ":" s
   in (elems !! 0, elems !! 1)

parsePassport :: String -> [(String, String)]
parsePassport s = map parseKeyValue $ filter (/= "") $ splitOn " " s

parsePassportDB :: String -> IO [[(String, String)]]
parsePassportDB filename = do
  ls <- parseFileLines filename
  return $ map parsePassport $ combineUnseperatedLines ls

parseSeatNumberDigit :: Char -> Int
parseSeatNumberDigit 'F' = 0
parseSeatNumberDigit 'B' = 1
parseSeatNumberDigit 'R' = 1
parseSeatNumberDigit 'L' = 0

parseSeatNumber :: String -> Int
parseSeatNumber (c:s) = (parseSeatNumberDigit c) * (2 ^ (length s))  + (parseSeatNumber s)
parseSeatNumber _ = 0

parseGroupChoices :: String -> IO [[Char]]
parseGroupChoices filename = (do
    ls <- parseFileLines filename
    let entries = splitWhen (=="") ls
    return $ map nub $ map concat entries
  )

isChosenByAll :: [String] -> Char -> Bool
isChosenByAll group question = all (elem question) group

getGroupChoicesByAll :: [String] -> [Char]
getGroupChoicesByAll group = 
  let allQuestions = nub $ concat group
  in  filter (isChosenByAll group) allQuestions

parseGroupChoicesByAll :: String -> IO [[Char]]
parseGroupChoicesByAll filename = (do
    ls <- parseFileLines filename
    let groups = splitWhen (=="") ls
    return $ map getGroupChoicesByAll groups
  )

