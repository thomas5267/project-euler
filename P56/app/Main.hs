module Main where

import Euler.Helper

candidates :: [Integer]
candidates = [ a^b | a <- [1..100], b <- ([1..100] :: [Int]) ]

results :: [Int]
results = map (sum . digits) candidates

main :: IO ()
main = print $ maximum results
