module EndToEnd where

import Solver
import Test.HUnit

test1A :: Test
test1A =
  TestCase
    (do r <- day1A "data/test/day1.txt"
        assertEqual "Day 1A EndToEnd failed" 514579 r)

test1B :: Test
test1B =
  TestCase
    (do r <- day1B "data/test/day1.txt"
        assertEqual "Day 1B EndToEnd failed" 241861950 r)

test2A :: Test
test2A =
  TestCase
    (do r <- day2A "data/test/day2.txt"
        assertEqual "Day 2A EndToEnd failed" 2 r)

test2B :: Test
test2B =
  TestCase
    (do r <- day2B "data/test/day2.txt"
        assertEqual "Day 2B EndToEnd failed" 1 r)

test3A :: Test
test3A =
  TestCase
    (do r <- day3A "data/test/day3.txt"
        assertEqual "Day 3A EndToEnd failed" 7 r)

test3B :: Test
test3B =
  TestCase
    (do r <- day3B "data/test/day3.txt"
        assertEqual "Day 3B EndToEnd failed" 336 r)

test4A :: Test
test4A =
  TestCase
    (do r <- day4A "data/test/day4A.txt"
        assertEqual "Day 4A EndToEnd failed" 2 r)

test4B :: Test
test4B =
  TestCase
    (do r <- day4B "data/test/day4B.txt"
        assertEqual "Day 4B EndToEnd failed" 4 r)

test5A :: Test
test5A =
  TestCase
    (do r <- day5A "data/test/day5A.txt"
        assertEqual "Day 5A EndToEnd failed" 820 r)

test5B :: Test
test5B =
  TestCase
    (do r <- day5B "data/test/day5B.txt"
        assertEqual "Day 5B EndToEnd failed" 821 r)

test6A :: Test
test6A =
  TestCase
    (do r <- day6A "data/test/day6A.txt"
        assertEqual "Day 6A EndToEnd failed" 11 r)

test6B :: Test
test6B =
  TestCase
    (do r <- day6B "data/test/day6A.txt"
        assertEqual "Day 6B EndToEnd failed" 6 r)

test7A :: Test
test7A =
  TestCase
    (do r <- day7A "data/test/day7.txt"
        assertEqual "Day 7A EndToEnd failed" 4 r)

test7B :: Test
test7B =
  TestCase
    (do r <- day7B "data/test/day7.txt"
        assertEqual "Day 7B EndToEnd failed" 32 r)

endToEndTests :: Test
endToEndTests =
  TestList
    [ TestLabel "test1A" test1A
    , TestLabel "test1B" test1B
    , TestLabel "test2A" test2A
    , TestLabel "test2B" test2B
    , TestLabel "test3A" test3A
    , TestLabel "test3B" test3B
    , TestLabel "test4A" test4A
    , TestLabel "test4B" test4B
    , TestLabel "test5A" test5A
    , TestLabel "test5B" test5B
    , TestLabel "test6A" test6A
    , TestLabel "test6B" test6B
    , TestLabel "test7A" test7A
    , TestLabel "test7B" test7B
    ]
