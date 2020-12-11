module Day2 where

import Common
import Text.Regex.PCRE

passwordPolicyPattern :: String
passwordPolicyPattern = "(\\d+)\\-(\\d+)\\s(\\w)\\:\\s*(.+)"

matchPasswordPolicyPattern :: String -> [[String]]
matchPasswordPolicyPattern line = line =~ passwordPolicyPattern

policyCharacter :: String -> Char
policyCharacter line = (matchPasswordPolicyPattern line) !! 0 !! 3 !! 0

policyMin :: String -> Int
policyMin line = read $ (matchPasswordPolicyPattern line) !! 0 !! 1

policyMax :: String -> Int
policyMax line = read $ (matchPasswordPolicyPattern line) !! 0 !! 2

policyPassword :: String -> String
policyPassword line = (matchPasswordPolicyPattern line) !! 0 !! 4

policyMatchingChars :: String -> Int
policyMatchingChars line =
  countIf (== (policyCharacter line)) (policyPassword line)

verifyPolicy :: String -> Bool
verifyPolicy policy =
  let matchingChars = policyMatchingChars policy
      min = policyMin policy
      max = policyMax policy
   in matchingChars >= min && matchingChars <= max

verifyPolicyNew :: String -> Bool
verifyPolicyNew policy =
  let password = policyPassword policy
      char = policyCharacter policy
      firstPosChar = password !! ((policyMin policy) - 1)
      secondPosChar = password !! ((policyMax policy) - 1)
   in (firstPosChar == char) /= (secondPosChar == char)

a :: String -> IO Int
a filename = do
  policies <- parseFileLines filename
  let validOld = countIf verifyPolicy policies
  return validOld

b :: String -> IO Int
b filename = do
  policies <- parseFileLines filename
  let validOld = countIf verifyPolicyNew policies
  return validOld
