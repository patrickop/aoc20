module EndToEnd where
import Test.HUnit
import Solver

test6A :: Test
test6A =
  TestCase
    (do 
      r <- day6A "data/test/day6A.txt"
      assertEqual "Day 6A EndToEnd failed" 11 r
    )

test6B :: Test
test6B =
  TestCase
    (do 
      r <- day6B "data/test/day6A.txt"
      assertEqual "Day 6B EndToEnd failed" 6 r
    )

endToEndTests :: Test
endToEndTests =
  TestList
    [ TestLabel "test6A" test6A 
    , TestLabel "test6B" test6B 
    ]
