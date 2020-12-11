module TestCommon
  ( tests
  ) where

import Common
import Test.HUnit

testParseLines :: Test
testParseLines =
  TestCase
    (do result <- parseFileLines "data/test/ints.txt"
        assertEqual
          "parses file lines"
          ["1721", "979", "366", "299", "675", "1456"]
          result)

tests :: Test
tests = TestList [TestLabel "parseLines" testParseLines]
