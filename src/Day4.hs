module Day4 where
import Common
import Data.List
import Data.List.Split
import Text.Regex.PCRE

combineUnseperatedLines :: [String] -> [String]
combineUnseperatedLines ls =
  let entries = splitWhen (== "") ls
   in map (intercalate " ") entries

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

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasField :: Eq a => a -> [(a, a)] -> Bool
hasField ex ((k, v):xs) = k == ex || hasField ex xs
hasField _ _ = False

hasRequiredFields :: [(String, String)] -> Bool
hasRequiredFields passport = all (\x -> hasField x passport) requiredFields

validatePassportField :: (String, String) -> Bool
validatePassportField ("byr", v) =
  let i = read v :: Int
   in (i >= 1920) && (i <= 2002)
validatePassportField ("iyr", v) =
  let i = read v :: Int
   in (i >= 2010) && (i <= 2020)
validatePassportField ("eyr", v) =
  let i = read v :: Int
   in (i >= 2020) && (i <= 2030)
validatePassportField ("hgt", v)
  | (drop 2 v) == "in" =
    let i = (read (take 2 v)) :: Int
     in (i >= 59) && (i <= 76)
  | (drop 3 v) == "cm" =
    let i = (read (take 3 v)) :: Int
     in (i >= 150) && (i <= 193)
  | otherwise = False
validatePassportField ("hcl", v) = v =~ "#[a-f0-9]{6}"
validatePassportField ("ecl", v) =
  elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validatePassportField ("pid", v) = v =~ "^\\d{9}$"
validatePassportField ("cid", _) = True

validatePassport :: [(String, String)] -> Bool
validatePassport passport = all (== True) $ map validatePassportField passport

a :: String -> IO Int
a filename = do
  passports <- parsePassportDB filename
  let result = countIf hasRequiredFields passports
  return result

b :: String -> IO Int
b filename = do
  passports <- parsePassportDB filename
  let validCt =
        countIf (\x -> (hasRequiredFields x) && (validatePassport x)) passports
  return validCt

