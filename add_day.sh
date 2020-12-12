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
sed -i "s|import Test.HUnit|import qualified TestDay8\nimport Test.HUnit|"
sed -i "s|\]|   , TestLabel \"Day${day}\" TestDay${day}.tests\n    ]|"

# Add the solver to the app
sed -i "s|solve s = putStrLn (s ++ \" Not solved\")|
solve \"day8A\" = do\n
  result <- D8.a \"data/day8.txt\"\n
  putStrLn $ show $ result\n
solve \"day8B\" = do\n
  result <- D8.b \"data/day8.txt\"\n
  putStrLn $ show $ result\n
solve s = putStrLn (s ++ \" Not solved\")|"

