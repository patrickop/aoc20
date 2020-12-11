module Day6 where
import Common
import Data.List
import Data.List.Split

parseGroupChoices :: String -> IO [[Char]]
parseGroupChoices filename =
  (do ls <- parseFileLines filename
      let entries = splitWhen (== "") ls
      return $ map nub $ map concat entries)

isChosenByAll :: [String] -> Char -> Bool
isChosenByAll group question = all (elem question) group

getGroupChoicesByAll :: [String] -> [Char]
getGroupChoicesByAll group =
  let allQuestions = nub $ concat group
   in filter (isChosenByAll group) allQuestions

parseGroupChoicesByAll :: String -> IO [[Char]]
parseGroupChoicesByAll filename =
  (do ls <- parseFileLines filename
      let groups = splitWhen (== "") ls
      return $ map getGroupChoicesByAll groups)

a :: String -> IO Int
a filename = do
  choices <- parseGroupChoices filename
  return $ foldl (+) 0 $ map length choices

b :: String -> IO Int
b filename = do
  choices <- parseGroupChoicesByAll filename
  return $ foldl (+) 0 $ map length choices

