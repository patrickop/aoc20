module Day7 where
import qualified Data.Map as M
import Common
import Data.List.Split

parseBagContents :: [String] -> [(Int, String)]
parseBagContents (count:adj:color:measure:xs) =
  ((read count), (concat [adj, color])) : parseBagContents xs
parseBagContents ["no", "other", "bags."] = []
parseBagContents [] = []

parseBagRule :: String -> (String, [(Int, String)])
parseBagRule rule =
  let label = concat $ take 2 $ splitOn " " rule
      contents = parseBagContents $ drop 4 $ splitOn " " rule
   in (label, contents)

parseAllBagRules :: String -> IO (M.Map String [(Int, String)])
parseAllBagRules filename =
  (do ls <- parseFileLines filename
      return $ M.fromList $ map parseBagRule ls)
lookupOrEmpty :: Ord a => a -> M.Map a [b] -> [b]
lookupOrEmpty key m
  | Just val <- M.lookup key m = val
  | otherwise = []

containsBagOfColor :: String -> M.Map String [String] -> String -> Bool
containsBagOfColor target rules original =
  let contents = lookupOrEmpty original rules
   in (elem target contents) ||
      (any (== True) $ map (containsBagOfColor target rules) contents)

countAllBags :: M.Map String [(Int, String)] -> String -> Int
countAllBags rules bag =
  let contents = lookupOrEmpty bag rules
      direct = foldr (+) 0 $ map fst contents
      children =
        foldr (+) 0 $
        map (\x -> ((fst x) * (countAllBags rules (snd x)))) contents
   in direct + children
a :: String -> IO Int
a filename = do
  rules <- parseAllBagRules filename
  let simpleRules = M.map (map snd) rules
  let colors = M.keys simpleRules
  let goldContained = map (containsBagOfColor "shinygold" simpleRules) colors
  return $ countIf (== True) goldContained

b :: String -> IO Int
b filename = do
  rules <- parseAllBagRules filename
  return $ countAllBags rules "shinygold"
