import Unit
import EndToEnd
import Test.HUnit

tests :: Test
tests =
  TestList
    [ TestLabel "unit tests" unitTests 
    , TestLabel "end to end tests" endToEndTests ]
main :: IO Counts
main = runTestTT tests
