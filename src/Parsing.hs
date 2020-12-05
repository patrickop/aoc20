module Parsing where 

addone :: Int -> Int
addone x = x + 1

parseIntsFile :: String -> IO [Int]
parseIntsFile name = do
                    content <- readFile name
                    return ( map ( read :: String->Int) (lines content) )
