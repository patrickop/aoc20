module TestDay4
  ( tests
  ) where

import Day4
import Test.HUnit

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

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day4A.txt"
        assertEqual "Day 4A EndToEnd failed" 2 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day4B.txt"
        assertEqual "Day 4B EndToEnd failed" 4 r)

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testParsePassportDB" testParsePassportDB
    , TestLabel "testValidatePassportField" testValidatePassportField
    ]
