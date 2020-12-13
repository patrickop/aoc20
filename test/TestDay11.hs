module TestDay11
  ( tests
  ) where

import Day11
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day11.txt"
        assertEqual "Day 11 A EndToEnd failed" 37 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day11.txt"
        assertEqual "Day 11 B EndToEnd failed" 26 r)

tests :: Test
tests = TestList [TestLabel "testA" testA, TestLabel "testB" testB]
