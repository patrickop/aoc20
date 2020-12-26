module TestDay16
  ( tests
  ) where

import Common
import Day16
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day16.txt"
        assertEqual "Day 16 A EndToEnd failed" (71) r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day16.txt"
        assertEqual "Day 16 B EndToEnd failed" (1) r)

testParseRules :: Test
testParseRules =
  TestCase
    (do lines <- parseFileLines "data/test/day16.txt"
        let ex =
              [ Rule "class" [(1, 3), (5, 7)]
              , Rule "row" [(6, 11), (33, 44)]
              , Rule "seat" [(13, 40), (45, 50)]
              ]
        assertEqual "Rule failed to parse" (ex) (parseRules lines))

testParseMyTicket :: Test
testParseMyTicket =
  TestCase
    (do lines <- parseFileLines "data/test/day16.txt"
        let ex = [7, 1, 14]
        assertEqual "My ticket failed to parse" (ex) (parseMyTicket lines))

testParseOtherTickets :: Test
testParseOtherTickets =
  TestCase
    (do lines <- parseFileLines "data/test/day16.txt"
        let ex = [[7, 3, 47], [40, 4, 50], [55, 2, 20], [38, 6, 12]]
        assertEqual
          "other tickets failed to parse"
          (ex)
          (parseOtherTickets lines))

testDiscardInvalid :: Test
testDiscardInvalid =
  TestCase
    (do let rs =
              [ Rule "class" [(1, 3), (5, 7)]
              , Rule "row" [(6, 11), (33, 44)]
              , Rule "seat" [(13, 40), (45, 50)]
              ]
        let ts = [[7, 3, 47], [40, 4, 50], [55, 2, 20], [38, 6, 12]]
        let ex = [[7, 3, 47]]
        assertEqual "DiscardInvali failed" (ex) (discardInvalid rs ts))

testGetPossibleIndicesFor :: Test
testGetPossibleIndicesFor =
  TestCase
    (do let rClass = Rule "class" [(0, 1), (4, 19)]
        let rRow = Rule "row" [(0, 5), (8, 19)]
        let rSeat = Rule "seat" [(0, 13), (16, 19)]
        let ts = [[11, 12, 13], [3, 9, 18], [15, 1, 5], [5, 14, 9]]
        assertEqual
          "get class index failed"
          ([1, 2])
          (getPossibleIndicesFor ts rClass)
        assertEqual
          "get row index failed"
          ([0, 1, 2])
          (getPossibleIndicesFor ts rRow)
        assertEqual
          "get seat index failed"
          ([2])
          (getPossibleIndicesFor ts rSeat))

testGetValidIndexCombination :: Test
testGetValidIndexCombination =
  TestCase
    (do let i = [[1, 2], [0, 1, 2], [2]]
        assertEqual
          "get valid combo failed"
          (Just [1, 0, 2])
          (getValidIndexCombination i))

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testParseRules" testParseRules
    , TestLabel "testParseMyTicket" testParseMyTicket
    , TestLabel "testParseOtherTickets" testParseOtherTickets
    , TestLabel "testDiscardInvalid" testDiscardInvalid
    , TestLabel "testGetPossibleIndicesFor" testGetPossibleIndicesFor
    , TestLabel "testGetValidIndexCombination" testGetValidIndexCombination
    ]
