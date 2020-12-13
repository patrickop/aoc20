module TestDay10
  ( tests
  ) where

import Day10
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day10.txt"
        assertEqual "Day 10 A EndToEnd failed" 220 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day10.txt"
        assertEqual "Day 10 B EndToEnd failed" 19208 r)

tests :: Test
tests = TestList [TestLabel "testA" testA, TestLabel "testB" testB]
