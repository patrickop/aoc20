module TestDay7
  ( tests
  ) where

import Day7
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day7.txt"
        assertEqual "Day 7A EndToEnd failed" 4 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day7.txt"
        assertEqual "Day 7B EndToEnd failed" 32 r)

testParsesBagRule :: Test
testParsesBagRule =
  TestCase
    (do let contents =
              parseBagRule
                "light red bags contain 1 bright white bag, 2 muted yellow bags."
        let ex_contents = ("lightred", [(1, "brightwhite"), (2, "mutedyellow")])
        assertEqual "parses contents" ex_contents contents
        let contents1 = parseBagRule "light red bags contain no other bags."
        let ex_contents1 = ("lightred", [])
        assertEqual "parses contents" ex_contents1 contents1)

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testParsesBagRule" testParsesBagRule
    ]
