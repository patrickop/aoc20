module Day3 where
import Common

data Location =
  Location
    { right :: Int
    , down :: Int
    }
  deriving (Eq, Show, Ord)

data TreeMap =
  TreeMap
    { width :: Int
    , depth :: Int
    , trees :: [Location]
    }

data Slope =
  Slope
    { rightStep :: Int
    , downStep :: Int
    }

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

sledLocs :: Slope -> [Location]
sledLocs (Slope right down) = [Location (x * right) (x * down) | x <- [0 ..]]

sledsToBottom :: Slope -> Int -> [Location]
sledsToBottom slope depth =
  takeWhile (\(Location _ down) -> down < depth) (sledLocs slope)

mapLocToWidth :: Location -> Int -> Location
mapLocToWidth (Location right down) width = Location (right `mod` width) down

countSledLocsOnTree :: TreeMap -> Slope -> Int
countSledLocsOnTree map slope =
  let sleds = sledsToBottom slope (depth map)
      w = width map
      t = trees map
   in countIf (\x -> elem (mapLocToWidth x w) t) sleds

a :: String -> IO Int
a filename = do
  tmap <- parseTreeMap filename
  return $ countSledLocsOnTree tmap (Slope 3 1)

b :: String -> IO Int
b filename = do
  tmap <- parseTreeMap filename
  let slopes = [(Slope 1 1), (Slope 3 1), (Slope 5 1), (Slope 7 1), (Slope 1 2)]
  let counts = map (\slope -> countSledLocsOnTree tmap slope) slopes
  return $ product counts

