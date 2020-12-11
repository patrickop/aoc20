module Day5 where
import Common
import Data.List

parseSeatNumberDigit :: Char -> Int
parseSeatNumberDigit 'F' = 0
parseSeatNumberDigit 'B' = 1
parseSeatNumberDigit 'R' = 1
parseSeatNumberDigit 'L' = 0

parseSeatNumber :: String -> Int
parseSeatNumber (c:s) =
  (parseSeatNumberDigit c) * (2 ^ (length s)) + (parseSeatNumber s)
parseSeatNumber _ = 0

valueOrDefault :: (Maybe a) -> a -> a
valueOrDefault (Just x) _ = x
valueOrDefault Nothing d = d

isMySeat :: [Int] -> Int -> Bool
isMySeat others candidate =
  let l = candidate - 1
      r = candidate + 1
   in (elem l others) && (elem r others) && (not (elem candidate others))

a :: String -> IO Int
a filename = do
  nrs <- parseFileLines filename
  let seatnrs = map parseSeatNumber nrs
  let max = maximum $ seatnrs
  return max

b :: String -> IO Int
b filename = do
  nrs <- parseFileLines filename
  let seatnrs = map parseSeatNumber nrs
  let max = maximum $ seatnrs
  let min = minimum $ seatnrs
  let mynr = find (isMySeat seatnrs) [min .. max]
  return (valueOrDefault mynr (-1))

