import qualified TestCommon
import qualified TestDay1
import qualified TestDay2
import qualified TestDay3
import qualified TestDay4
import qualified TestDay5
import qualified TestDay6
import qualified TestDay7
import qualified TestDay8

import Test.HUnit
import qualified TestDay10
import qualified TestDay11
import qualified TestDay12
import qualified TestDay9

tests :: Test
tests =
  TestList
    [ TestLabel "Common" TestCommon.tests
    , TestLabel "Day1" TestDay1.tests
    , TestLabel "Day2" TestDay2.tests
    , TestLabel "Day3" TestDay3.tests
    , TestLabel "Day4" TestDay4.tests
    , TestLabel "Day5" TestDay5.tests
    , TestLabel "Day6" TestDay6.tests
    , TestLabel "Day7" TestDay7.tests
    , TestLabel "Day8" TestDay8.tests
    , TestLabel "Day9" TestDay9.tests
    , TestLabel "Day10" TestDay10.tests
    , TestLabel "Day11" TestDay11.tests
    , TestLabel "Day12" TestDay12.tests
    ]

main :: IO Counts
main = runTestTT tests
