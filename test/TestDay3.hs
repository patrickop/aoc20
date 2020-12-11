module TestDay3 (tests) where

import Test.HUnit
import Day3
import Data.List

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day3.txt"
        assertEqual "Day 3A EndToEnd failed" 7 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day3.txt"
        assertEqual "Day 3B EndToEnd failed" 336 r)

testParseTreeMap :: Test
testParseTreeMap =
  TestCase
    (do tmap <- parseTreeMap "data/test/treemap.txt"
        let ts = sort $ trees tmap
        let ts_expected =
              sort
                [ (Location 1 0)
                , (Location 4 0)
                , (Location 5 0)
                , (Location 0 1)
                , (Location 4 1)
                , (Location 0 3)
                ]
        assertEqual "parses tree locations" ts_expected ts
        assertEqual "parses width" 6 (width tmap)
        assertEqual "parses depth" 4 (depth tmap))


tests :: Test
tests =
  TestList
    [ 
      TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testParseTreeMap" testParseTreeMap
    ]
