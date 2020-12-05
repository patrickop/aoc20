import Test.HUnit
import Parsing

test1 :: Test
test1 = TestCase (assertEqual "addone adds one" 4 (addone 3))

testParseInts :: Test
testParseInts = TestCase (
            do 
              result <- parseIntsFile "data/test/ints.txt"
              assertEqual 
                "parses file with one int per row"
                [1721, 979, 366, 299, 675, 1456]
                result 
            )

tests :: Test
tests = TestList [
          TestLabel "test1" test1,
          TestLabel "testParseInts" testParseInts
        ]

main :: IO Counts
main = runTestTT tests
