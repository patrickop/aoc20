module Main where

import Parsing
import Analysing

main :: IO ()
main = do
        numbers <- parseIntsFile "data/day1/input.txt"
        let t1 = duos numbers
        let t2 = triples numbers
        let combinationT1 = findCombosThatAddTo t1 2020
        let combinationT2 = findCombosThatAddTo t2 2020
        let resultT1 = multiply' combinationT1
        let resultT2 = multiply' combinationT2
        putStrLn ("duos: " ++ ( show resultT1 ))
        putStrLn ("triples: " ++ ( show resultT2 ))
