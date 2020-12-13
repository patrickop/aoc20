module TestDay13
  ( tests
  ) where

import Day13
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day13.txt"
        assertEqual "Day 13 A EndToEnd failed" 295 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day13.txt"
        assertEqual "Day 13 B EndToEnd failed" 1068781 r)

testBSubcases :: Test
testBSubcases =
  TestCase
    (do 
        assertEqual "Day 13 B Subcase failed" 3417 (solveB "17,x,13,19")
        assertEqual "Day 13 B Subcase failed" 754018 (solveB "67,7,59,61")
        assertEqual "Day 13 B Subcase failed" 779210 (solveB "67,x,7,59,61")
        assertEqual "Day 13 B Subcase failed" 1261476 (solveB "67,7,x,59,61")
        assertEqual "Day 13 B Subcase failed" 1202161486 (solveB "1789,37,47,1889")

        )

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testBSubcases" testBSubcases
    ]
