import Test.HUnit
import Parsing
import Analysing
import Data.Sort

-- Test parsing
testParseInts :: Test
testParseInts = TestCase (
            do 
              result <- parseIntsFile "data/test/ints.txt"
              assertEqual 
                "parses file with one int per row"
                [1721, 979, 366, 299, 675, 1456]
                result 
            )

-- Test analysing
testDuos :: Test
testDuos = TestCase (
              assertEqual
                "finds all duos"
                (sort [[1,2], [1,3], [1,4], [2,3], [2,4], [3,4]])
                (sort (duos[1, 2, 3, 4]))
            )
testTriples :: Test
testTriples = TestCase (
              assertEqual
                "finds all duos"
                (sort [[1,2,3], [1,2,4], [2,3,4], [1,3,4]])
                (sort (triples[1, 2, 3, 4]))
            )

testFindNumbersThatAddTo :: Test
testFindNumbersThatAddTo = TestCase (
              assertEqual
                "finds number pair that adds to 6"
                (Just [2,4])
                (findCombosThatAddTo [[1,2], [1,3], [1,4], [2,3], [2,4], [3,4]] 6)
            )

tests :: Test
tests = TestList [
          TestLabel "testParseInts" testParseInts,
          TestLabel "testDuos" testDuos,
          TestLabel "testTriples" testTriples,
          TestLabel "testFindNumbersThatAddTo" testFindNumbersThatAddTo
        ]

main :: IO Counts
main = runTestTT tests
