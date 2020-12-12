module TestDay1
  ( tests
  ) where

import Data.List
import Day1
import Test.HUnit

testTriples :: Test
testTriples =
  TestCase
    (assertEqual
       "finds all triples"
       (sort [[1, 2, 3], [1, 2, 4], [2, 3, 4], [1, 3, 4]])
       (sort (triples [1, 2, 3, 4])))

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
    [ TestLabel "testTriples" testTriples
    , TestLabel "testA" testA
    , TestLabel "testB" testB
    ]
