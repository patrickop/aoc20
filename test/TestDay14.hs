module TestDay14
  ( tests
  ) where

import Data.List
import Day14
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day14.txt"
        assertEqual "Day 14 A EndToEnd failed" (165) r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day14B.txt"
        assertEqual "Day 14 B EndToEnd failed" (208) r)

testParseInstruction :: Test
testParseInstruction =
  TestCase
    (do let bitmask = parseInstruction "mask = XX10X"
        let ex_bitmask = BitmaskInstruction [Keep, Keep, Set, Unset, Keep]
        assertEqual "Parse Bitmask failed" (ex_bitmask) bitmask
        let memory = parseInstruction "mem[5] = 33"
        let ex_memory = MemoryManipulationInstruction (MemoryManipulation 5 33)
        assertEqual "Parse memory failed" (ex_memory) memory)

testApplyBitmask :: Test
testApplyBitmask =
  TestCase
    (do let bitmask = [Keep, Keep, Set, Unset, Keep]
        let in0 = 0b11111
        let result0 = applyBitmask in0 bitmask
        assertEqual "Apply bitmask to 11111 failed" (0b11101) result0
        let in1 = 0b00000
        let result1 = applyBitmask in1 bitmask
        assertEqual "Apply bitmask to 00000 failed" (0b00100) result1)

testApplyBitmaskAddress :: Test
testApplyBitmaskAddress =
  TestCase
    (do let bitmask = [Keep, Keep, Set, Unset, Keep]
        let in0 = 0b11111
        let result0 = applyBitmaskAddress in0 bitmask
        let ex_result0 =
              sort $
              [ 0b00110
              , 0b00111
              , 0b01110
              , 0b01111
              , 0b10110
              , 0b10111
              , 0b11110
              , 0b11111
              ]
        assertEqual
          "Apply bitmask address to 11111 failed"
          (ex_result0)
          (sort result0))

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    , TestLabel "testParseInstruction" testParseInstruction
    , TestLabel "testApplyBitmask" testApplyBitmask
    , TestLabel "testApplyAddress" testApplyBitmaskAddress
    ]
