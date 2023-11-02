module Main where

import Euler.Helper

import Data.Numbers.Primes

import Data.List (scanl')

truncations :: [Int] -> [Int]
-- Given the digits of a number, return all truncations of that number.
truncations ds = truncateFromLeft ds ++ truncateFromRight ds
  where
    truncateFromLeft :: [Int] -> [Int]
    truncateFromLeft = tail . map fromDigits . suffixes
      where
        suffixes []     = []
        suffixes xs = xs : suffixes (tail xs)

    truncateFromRight :: [Int] -> [Int]
    truncateFromRight = tail . scanl' (\ a d -> a*10 + d) 0

truncatablePrimes :: [Integer]
-- It is given that there are exactly 11 of them.
truncatablePrimes = go 11 (dropWhile (<= 10) primes)
  where
    go :: Int -> [Integer] -> [Integer]
    go 0 _      = []
    go _ []     = undefined
    go n (x:xs) =
        if isTruncatable x
        then x : go (n-1) xs
        else go n xs
      where
        isTruncatable p =
            (all odd (tail digitsP) && 5 `notElem` tail digitsP) && all isPrime (truncations digitsP)
          where
            digitsP = digits p

main :: IO ()
main = print $ sum truncatablePrimes
