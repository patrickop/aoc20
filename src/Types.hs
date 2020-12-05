module Types where

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
