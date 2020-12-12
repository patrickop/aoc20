#!/usr/bin/bash
day=$@
# add the test
cat <<EOF > test/TestDay${day}.hs
module TestDay${day}
  ( tests
  ) where

import Day${day}
import Test.HUnit

testA :: Test
testA =
  TestCase
    (do r <- a "data/test/day${day}.txt"
        assertEqual "Day ${day} A EndToEnd failed" (-1) r)

testB :: Test
testB =
  TestCase
    (do r <- b "data/test/day${day}.txt"
        assertEqual "Day ${day} B EndToEnd failed" (-1) r)

tests :: Test
tests =
  TestList
    [ TestLabel "testA" testA
    , TestLabel "testB" testB
    ]
EOF

# add the lib code
cat <<EOF > src/Day${day}.hs
module Day${day} where

import Common

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  return (-2)

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  return (-2)
EOF

# Add test cases to spec
sed -i "s|import Test.HUnit|import qualified TestDay${day}\nimport Test.HUnit|" test/Spec.hs
sed -i "s|\]|   , TestLabel \"Day${day}\" TestDay${day}.tests\n    ]|" test/Spec.hs

# Add the solver to the app
sed -i "s|import System.Environment|\
import qualified Day${day} as D${day}\n\
import System.Environment\n|" app/Main.hs

sed -i "s|solve s = putStrLn (s ++ \" Not solved\")|\
solve \"day${day}A\" = do\n\
  result <- D${day}.a \"data/day${day}.txt\"\n\
  putStrLn $ show $ result\n\
solve \"day${day}B\" = do\n\
  result <- D${day}.b \"data/day${day}.txt\"\n\
  putStrLn $ show $ result\n\
solve s = putStrLn (s ++ \" Not solved\")|" app/Main.hs 

