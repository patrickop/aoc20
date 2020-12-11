module Common where

parseFileLines :: String -> IO [String]
parseFileLines name = do
  content <- readFile name
  return (lines content)

countIf :: (a -> Bool) -> [a] -> Int
countIf op (x:xs)
  | op x = 1 + (countIf op xs)
  | otherwise = (countIf op xs)
countIf op [] = 0
