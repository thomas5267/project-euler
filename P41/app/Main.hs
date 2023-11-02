module Main (main) where

import Euler.Helper

import Data.List (permutations, sort)
import Data.Numbers.Primes

pandigitalNumbers :: [Int]
pandigitalNumbers = foldr go [] [1..9]
  where
    go :: Int -> [Int] -> [Int]
    go d acc = (map fromDigits . permutations $ [1..d]) ++ acc

main :: IO ()
main = print $ maximum . filter isPrime $ pandigitalNumbers
