module TestDay8
  ( tests
  ) where

import Day8
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day8.txt"
        assertEqual "Day 8A EndToEnd failed" 5 r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day8.txt"
        assertEqual "Day 8B EndToEnd failed" 8 r)

testSwapInstruction :: Test
testSwapInstruction =
  TestCase
    (do code <- parseProgram "data/test/day8.txt"
        let ex0 =
              Just
                [ (Jmp, 0)
                , (Acc, 1)
                , (Jmp, 4)
                , (Acc, 3)
                , (Jmp, -3)
                , (Acc, -99)
                , (Acc, 1)
                , (Jmp, -4)
                , (Acc, 6)
                ]
        let ex1 = Nothing
        let ex2 =
              Just
                [ (Nop, 0)
                , (Acc, 1)
                , (Nop, 4)
                , (Acc, 3)
                , (Jmp, -3)
                , (Acc, -99)
                , (Acc, 1)
                , (Jmp, -4)
                , (Acc, 6)
                ]
        assertEqual "Swap instruction 0" ex0 (swapInstruction code 0)
        assertEqual "Swap instruction 1" ex1 (swapInstruction code 1)
        assertEqual "Swap instruction 2" ex2 (swapInstruction code 2)
        assertEqual "Swap instruction 3" Nothing (swapInstruction code 99))

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testSwapInstruction" testSwapInstruction
    ]
