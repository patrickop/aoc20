module TestDay15
  ( tests
  ) where

import Day15
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day15.txt"
        assertEqual "Day 15 A EndToEnd failed" (1836) r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day15.txt"
        assertEqual "Day 15 B EndToEnd failed" (362) r)

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    ]
