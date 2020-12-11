module TestDay5 (tests) where

import Test.HUnit
import Day5

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day5A.txt"
        assertEqual "Day 5A EndToEnd failed" 820 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day5B.txt"
        assertEqual "Day 5B EndToEnd failed" 821 r)

testParseSeatNumber :: Test
testParseSeatNumber =
  TestCase
    (do assertEqual "parses seat number" 567 (parseSeatNumber "BFFFBBFRRR")
        assertEqual "parses seat number" 119 (parseSeatNumber "FFFBBBFRRR")
        assertEqual "parses seat number" 820 (parseSeatNumber "BBFFBBFRLL"))


tests :: Test
tests =
  TestList
    [ 
      TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testParseSeatNumber" testParseSeatNumber
    ]
