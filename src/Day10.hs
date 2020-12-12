module Day10 where

import Common
import Data.List
import Data.List.Split

getDiffs :: [Int] -> [Int]
getDiffs (x0:x1:xs) = [x1-x0] ++ (getDiffs (x1:xs))
getDiffs xs = []

getCompleteArrangement :: [Int] -> [Int]
getCompleteArrangement xs = sort (0:(3 + (maximum xs)):xs)

-- groups of differences of 1 can be rearranged with permutations
-- according to a(x) = 2*a(x-1) - a(x-4)
-- while preserving the law that no difference becomes larger than 3
possibleDeletionsOfOnes :: Int -> Int
possibleDeletionsOfOnes 0 = 0
possibleDeletionsOfOnes 1 = 0
possibleDeletionsOfOnes 2 = 1
possibleDeletionsOfOnes 3 = 3
possibleDeletionsOfOnes x = (2 * (possibleDeletionsOfOnes (x-1))) - (possibleDeletionsOfOnes (x-4))

-- all possible arrangements include the one without deletions
possibleArrangementsOfOnes = (+1) . possibleDeletionsOfOnes 

-- groups of threes cannot be rearranged
-- groups of ones can be rearranged by the law above
-- the dataset only contains differences of 1 or three
-- thus we can split the diffs of the dataset into groups of one
-- calculate their rearrangements and then multiply the numbers
-- to get the total amount of arrangements possible
countPossibleDeletions :: [Int] -> Int
countPossibleDeletions xs 
  | all (\x -> ((x==1) || (x==3))) xs =
    product $ (map possibleArrangementsOfOnes) $ (map length) $ splitWhen (==3) xs
  | otherwise = error "Not implemented"

a :: String -> IO Int
a filename = do
  lines <- parseIntsFile filename
  let arrangement = getCompleteArrangement lines
  let diffs = getDiffs arrangement
  let ones = countIf (==1) diffs
  let threes = countIf (==3) diffs
  return (threes * ones)

b :: String -> IO Int
b filename = do
  lines <- parseIntsFile filename
  let arrangement = getCompleteArrangement lines
  let diffs = getDiffs arrangement
  return $ countPossibleDeletions diffs
