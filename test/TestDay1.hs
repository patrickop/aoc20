module TestDay1
  ( tests
  ) where

import Data.List
import Day1
import Test.HUnit

testParseInts :: Test
testParseInts =
  TestCase
    (do result <- parseIntsFile "data/test/ints.txt"
        assertEqual
          "parses file with one int per row"
          [1721, 979, 366, 299, 675, 1456]
          result)

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

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day1.txt"
        assertEqual "Day 1A EndToEnd failed" 514579 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day1.txt"
        assertEqual "Day 1B EndToEnd failed" 241861950 r)

tests :: Test
tests =
  TestList
    [ TestLabel "testParseInts" testParseInts
    , TestLabel "testDuos" testDuos
    , TestLabel "testTriples" testTriples
    , TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testFindNumbersThatAddTo" testFindNumbersThatAddTo
    ]
