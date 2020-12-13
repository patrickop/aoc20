module TestDay12
  ( tests
  ) where

import Day12
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day12.txt"
        assertEqual "Day 12 A EndToEnd failed" (25) r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day12.txt"
        assertEqual "Day 12 B EndToEnd failed" 286 r)

tests :: Test
tests = TestList [TestLabel "testA" testA, TestLabel "testB" testB]
