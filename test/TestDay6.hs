module TestDay6 (tests) where

import Test.HUnit
import Day6

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day6A.txt"
        assertEqual "Day 6A EndToEnd failed" 11 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day6A.txt"
        assertEqual "Day 6B EndToEnd failed" 6 r)

testParseGroupChoices :: Test
testParseGroupChoices =
  TestCase
    (do choices <- parseGroupChoices "data/test/day6A.txt"
        let ex_choices = ["abc", "abc", "abc", "a", "b"]
        assertEqual "parses group choices" ex_choices choices)

testParseGroupChoicesByAll :: Test
testParseGroupChoicesByAll =
  TestCase
    (do choices <- parseGroupChoicesByAll "data/test/day6A.txt"
        let ex_choices = ["abc", "", "a", "a", "b"]
        assertEqual "parses group choices by all" ex_choices choices)

tests :: Test
tests =
  TestList
    [ 
      TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testParseGroupChoices" testParseGroupChoices
    , TestLabel "testParseGroupChoicesByAll" testParseGroupChoicesByAll
    ]
