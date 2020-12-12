module TestDay9
  ( tests
  ) where

import Day9
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day9.txt"
        assertEqual "Day 9 A EndToEnd failed" (-1) r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day9.txt"
        assertEqual "Day 9 B EndToEnd failed" (-1) r)

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    ]
