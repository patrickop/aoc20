module TestCommon
  ( tests
  ) where

import Common
import Data.List
import Test.HUnit

testParseLines :: Test
testParseLines =
  TestCase
    (do result <- parseFileLines "data/test/ints.txt"
        assertEqual
          "parses file lines"
          ["1721", "979", "366", "299", "675", "1456"]
          result)

testParseInts :: Test
testParseInts =
  TestCase
    (do result <- parseIntsFile "data/test/ints.txt"
        assertEqual
          "parses file with one int per row"
          [1721, 979, 366, 299, 675, 1456]
          result)

testFindNumbersThatAddTo :: Test
testFindNumbersThatAddTo =
  TestCase
    (assertEqual
       "finds number pair that adds to 6"
       (Just [2, 4])
       (findCombosThatAddTo [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]] 6))

testDuos :: Test
testDuos =
  TestCase
    (assertEqual
       "finds all duos"
       (sort [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]])
       (sort (duos [1, 2, 3, 4])))

tests :: Test
tests =
  TestList
    [ TestLabel "parseLines" testParseLines
    , TestLabel "testParseInts" testParseInts
    , TestLabel "testDuos" testDuos
    , TestLabel "testFindNumbersThatAddTo" testFindNumbersThatAddTo
    ]
