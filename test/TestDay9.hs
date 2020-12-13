module TestDay9
  ( tests
  ) where

import Day9
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a' 5 "data/test/day9.txt"
        assertEqual "Day 9 A EndToEnd failed" 127 r)

testB :: Test
testB =
  TestCase
    (do r <- b' 5 "data/test/day9.txt"
        assertEqual "Day 9 B EndToEnd failed" 62 r)

tests :: Test
tests = TestList [TestLabel "testA" testA, TestLabel "testB" testB]
