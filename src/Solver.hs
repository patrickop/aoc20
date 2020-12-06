module Solver where
import Parsing
import Analysing
import Types
import Data.List

day1A :: String -> IO Int
day1A filename = do
  numbers <- parseIntsFile filename
  let t = duos numbers
  let combination = findCombosThatAddTo t 2020
  let result = multiply' combination
  return result

day1B :: String -> IO Int
day1B filename = do
  numbers <- parseIntsFile filename
  let t = triples numbers
  let combination = findCombosThatAddTo t 2020
  let result = multiply' combination
  return result

day2A :: String -> IO Int
day2A filename = do
  policies <- parseFileLines filename
  let validOld = countIf verifyPolicy policies
  return validOld

day2B :: String -> IO Int
day2B filename = do
  policies <- parseFileLines filename
  let validOld = countIf verifyPolicyNew policies
  return validOld

day3A :: String -> IO Int
day3A filename = do
  tmap <- parseTreeMap filename
  return $ countSledLocsOnTree tmap (Slope 3 1)

day3B :: String -> IO Int
day3B filename = do
  tmap <- parseTreeMap filename
  let slopes = [(Slope 1 1), (Slope 3 1), (Slope 5 1), (Slope 7 1), (Slope 1 2)]
  let counts = map (\slope -> countSledLocsOnTree tmap slope) slopes
  return $ product counts

day4A :: String -> IO Int
day4A filename = do
  passports <- parsePassportDB filename
  let result = countIf hasRequiredFields passports
  return result

day4B :: String -> IO Int
day4B filename = do
  passports <- parsePassportDB filename
  let validCt = countIf (\x -> (hasRequiredFields x) && (validatePassport x)) passports
  return validCt

day5A :: String -> IO Int
day5A filename = do
  nrs <- parseFileLines filename
  let seatnrs = map parseSeatNumber nrs
  let max = maximum $ seatnrs
  return max

day5B :: String -> IO Int
day5B filename = do
  nrs <- parseFileLines filename
  let seatnrs = map parseSeatNumber nrs
  let max = maximum $ seatnrs
  let min = minimum $ seatnrs
  let mynr = find (isMySeat seatnrs) [min..max]
  return (valueOrDefault mynr (-1))

day6A :: String -> IO Int
day6A  filename =  do
  ls <- parseFileLines filename
  return 0

day6B :: String -> IO Int
day6B  filename =  do
  ls <- parseFileLines filename
  return 0

