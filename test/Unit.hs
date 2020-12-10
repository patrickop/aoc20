module Unit where

import Analysing
import Data.Sort
import Parsing
import Test.HUnit
import Types

-- Test parsing
testParseInts :: Test
testParseInts =
  TestCase
    (do result <- parseIntsFile "data/test/ints.txt"
        assertEqual
          "parses file with one int per row"
          [1721, 979, 366, 299, 675, 1456]
          result)

testParseLines :: Test
testParseLines =
  TestCase
    (do result <- parseFileLines "data/test/ints.txt"
        assertEqual
          "parses file lines"
          ["1721", "979", "366", "299", "675", "1456"]
          result)

-- Test analysing
testDuos :: Test
testDuos =
  TestCase
    (assertEqual
       "finds all duos"
       (sort [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]])
       (sort (duos [1, 2, 3, 4])))

testTriples :: Test
testTriples =
  TestCase
    (assertEqual
       "finds all triples"
       (sort [[1, 2, 3], [1, 2, 4], [2, 3, 4], [1, 3, 4]])
       (sort (triples [1, 2, 3, 4])))

testFindNumbersThatAddTo :: Test
testFindNumbersThatAddTo =
  TestCase
    (assertEqual
       "finds number pair that adds to 6"
       (Just [2, 4])
       (findCombosThatAddTo [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]] 6))

testPolicyCharacter :: Test
testPolicyCharacter =
  TestCase
    (assertEqual
       "finds policy relevant character"
       ('a')
       (policyCharacter "1-3 a: abcde"))

testPolicyMin :: Test
testPolicyMin =
  TestCase (assertEqual "finds policy min" (1) (policyMin "1-3 a: abcde"))

testPolicyMax :: Test
testPolicyMax =
  TestCase (assertEqual "finds policy max" (3) (policyMax "1-3 a: abcde"))

testPolicyPassword :: Test
testPolicyPassword =
  TestCase
    (assertEqual
       "finds policy password"
       ("abcde")
       (policyPassword "1-3 a: abcde"))

testPolicyMatchingChars :: Test
testPolicyMatchingChars =
  TestCase
    (assertEqual
       "counts matching chars"
       (2)
       (policyMatchingChars "1-3 a: abcdea"))

testParseTreeMap :: Test
testParseTreeMap =
  TestCase
    (do tmap <- parseTreeMap "data/test/treemap.txt"
        let ts = sort $ trees tmap
        let ts_expected =
              sort
                [ (Location 1 0)
                , (Location 4 0)
                , (Location 5 0)
                , (Location 0 1)
                , (Location 4 1)
                , (Location 0 3)
                ]
        assertEqual "parses tree locations" ts_expected ts
        assertEqual "parses width" 6 (width tmap)
        assertEqual "parses depth" 4 (depth tmap))

testParsePassportDB :: Test
testParsePassportDB =
  TestCase
    (do ps <- parsePassportDB "data/test/passports.txt"
        let ex_ps =
              [ [ ("ecl", "gry")
                , ("pid", "860033327")
                , ("eyr", "2020")
                , ("hcl", "#fffffd")
                , ("byr", "1937")
                , ("iyr", "2017")
                , ("cid", "147")
                , ("hgt", "183cm")
                ]
              , [ ("iyr", "2013")
                , ("ecl", "amb")
                , ("cid", "350")
                , ("eyr", "2023")
                , ("pid", "028048884")
                , ("hcl", "#cfa07d")
                , ("byr", "1929")
                ]
              , [ ("hcl", "#ae17e1")
                , ("iyr", "2013")
                , ("eyr", "2024")
                , ("ecl", "brn")
                , ("pid", "760753108")
                , ("byr", "1931")
                , ("hgt", "179cm")
                ]
              , [ ("hcl", "#cfa07d")
                , ("eyr", "2025")
                , ("pid", "166559648")
                , ("iyr", "2011")
                , ("ecl", "brn")
                , ("hgt", "59in")
                ]
              ]
        assertEqual "parses passport db" ex_ps ps)

testValidatePassportField :: Test
testValidatePassportField =
  TestCase
    (do assertEqual "parses byr" True (validatePassportField ("byr", "2002"))
        assertEqual "parses byr" False (validatePassportField ("byr", "2003"))
        assertEqual "parses hgt" True (validatePassportField ("hgt", "60in"))
        assertEqual "parses hgt" True (validatePassportField ("hgt", "190cm"))
        assertEqual "parses hgt" False (validatePassportField ("hgt", "190in"))
        assertEqual "parses hgt" False (validatePassportField ("hgt", "190"))
        assertEqual "parses hcl" True (validatePassportField ("hcl", "#123abc"))
        assertEqual
          "parses hcl"
          False
          (validatePassportField ("hcl", "#123abz"))
        assertEqual "parses hcl" False (validatePassportField ("hcl", "123abc"))
        assertEqual "parses ecl" True (validatePassportField ("ecl", "brn"))
        assertEqual "parses ecl" False (validatePassportField ("ecl", "wat"))
        assertEqual
          "parses pid"
          True
          (validatePassportField ("pid", "000000001"))
        assertEqual
          "parses pid"
          False
          (validatePassportField ("pid", "0123456789")))

testParseSeatNumber :: Test
testParseSeatNumber =
  TestCase
    (do assertEqual "parses seat number" 567 (parseSeatNumber "BFFFBBFRRR")
        assertEqual "parses seat number" 119 (parseSeatNumber "FFFBBBFRRR")
        assertEqual "parses seat number" 820 (parseSeatNumber "BBFFBBFRLL"))

testParseGroupChoices :: Test
testParseGroupChoices =
  TestCase
    (do choices <- parseGroupChoices "data/test/day6A.txt"
        let ex_choices = ["abc", "abc", "abc", "a", "b"]
        assertEqual "parses group choices" ex_choices choices)

testParseGroupChoicesByAll :: Test
testParseGroupChoicesByAll =
  TestCase
    (do choices <- parseGroupChoicesByAll "data/test/day6A.txt"
        let ex_choices = ["abc", "", "a", "a", "b"]
        assertEqual "parses group choices by all" ex_choices choices)

testParsesBagRule :: Test
testParsesBagRule =
  TestCase
    (do let contents =
              parseBagRule
                "light red bags contain 1 bright white bag, 2 muted yellow bags."
        let ex_contents = ("lightred", [(1, "brightwhite"), (2, "mutedyellow")])
        assertEqual "parses contents" ex_contents contents
        let contents1 = parseBagRule "light red bags contain no other bags."
        let ex_contents1 = ("lightred", [])
        assertEqual "parses contents" ex_contents1 contents1)

unitTests :: Test
unitTests =
  TestList
    [ TestLabel "testParseInts" testParseInts
    , TestLabel "testParseLines" testParseLines
    , TestLabel "testDuos" testDuos
    , TestLabel "testTriples" testTriples
    , TestLabel "testPolicyCharacter" testPolicyCharacter
    , TestLabel "testPolicyMin" testPolicyMin
    , TestLabel "testPolicyMax" testPolicyMax
    , TestLabel "testPolicyPassword" testPolicyPassword
    , TestLabel "testPolicyMatchingChars" testPolicyMatchingChars
    , TestLabel "testParseTreeMap" testParseTreeMap
    , TestLabel "testParsePassportDB" testParsePassportDB
    , TestLabel "testValidatePassportField" testValidatePassportField
    , TestLabel "testParseSeatNumber" testParseSeatNumber
    , TestLabel "testParseGroupChoices" testParseGroupChoices
    , TestLabel "testParseGroupChoicesByAll" testParseGroupChoicesByAll
    , TestLabel "testFindNumbersThatAddTo" testFindNumbersThatAddTo
    , TestLabel "testParsesBagRule" testParsesBagRule
    ]
