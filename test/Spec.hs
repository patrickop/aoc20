import Analysing
import Data.Sort
import Parsing
import Test.HUnit
import Types

-- Test parsing
testParseInts :: Test
testParseInts =
  TestCase
    (do result <- parseIntsFile "data/test/ints.txt"
        assertEqual
          "parses file with one int per row"
          [1721, 979, 366, 299, 675, 1456]
          result)

testParseLines :: Test
testParseLines =
  TestCase
    (do result <- parseFileLines "data/test/ints.txt"
        assertEqual
          "parses file lines"
          ["1721", "979", "366", "299", "675", "1456"]
          result)

-- Test analysing
testDuos :: Test
testDuos =
  TestCase
    (assertEqual
       "finds all duos"
       (sort [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]])
       (sort (duos [1, 2, 3, 4])))

testTriples :: Test
testTriples =
  TestCase
    (assertEqual
       "finds all triples"
       (sort [[1, 2, 3], [1, 2, 4], [2, 3, 4], [1, 3, 4]])
       (sort (triples [1, 2, 3, 4])))

testFindNumbersThatAddTo :: Test
testFindNumbersThatAddTo =
  TestCase
    (assertEqual
       "finds number pair that adds to 6"
       (Just [2, 4])
       (findCombosThatAddTo [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]] 6))

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
    [ TestLabel "testParseInts" testParseInts
    , TestLabel "testParseLines" testParseLines
    , TestLabel "testDuos" testDuos
    , TestLabel "testTriples" testTriples
    , TestLabel "testPolicyCharacter" testPolicyCharacter
    , TestLabel "testPolicyMin" testPolicyMin
    , TestLabel "testPolicyMax" testPolicyMax
    , TestLabel "testPolicyPassword" testPolicyPassword
    , TestLabel "testPolicyMatchingChars" testPolicyMatchingChars
    , TestLabel "testParseTreeMap" testParseTreeMap
    , TestLabel "testFindNumbersThatAddTo" testFindNumbersThatAddTo
    ]

main :: IO Counts
main = runTestTT tests
