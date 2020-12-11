module TestDay2
  ( tests
  ) where

import Day2
import Test.HUnit

testPolicyCharacter :: Test
testPolicyCharacter =
  TestCase
    (assertEqual
       "finds policy relevant character"
       ('a')
       (policyCharacter "1-3 a: abcde"))

testPolicyMin :: Test
testPolicyMin =
  TestCase (assertEqual "finds policy min" (1) (policyMin "1-3 a: abcde"))

testPolicyMax :: Test
testPolicyMax =
  TestCase (assertEqual "finds policy max" (3) (policyMax "1-3 a: abcde"))

testPolicyPassword :: Test
testPolicyPassword =
  TestCase
    (assertEqual
       "finds policy password"
       ("abcde")
       (policyPassword "1-3 a: abcde"))

testPolicyMatchingChars :: Test
testPolicyMatchingChars =
  TestCase
    (assertEqual
       "counts matching chars"
       (2)
       (policyMatchingChars "1-3 a: abcdea"))

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day2.txt"
        assertEqual "Day 2A EndToEnd failed" 2 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day2.txt"
        assertEqual "Day 2B EndToEnd failed" 1 r)

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testPolicyCharacter" testPolicyCharacter
    , TestLabel "testPolicyMin" testPolicyMin
    , TestLabel "testPolicyMax" testPolicyMax
    , TestLabel "testPolicyPassword" testPolicyPassword
    , TestLabel "testPolicyMatchingChars" testPolicyMatchingChars
    ]
