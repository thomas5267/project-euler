module Main where

import Euler.Helper

import Data.Numbers.Primes

import Data.IntSet qualified as IS

import Control.Monad


globalBound :: Int
-- [8389, 6733, 5701, 5197, 13] is one set of primes with 5 elements
-- satisfying the prime pair property.
-- The sum of this set is 26033.
-- The set was found by brute-force to 10000 using the exact same algorithm
-- as below.
globalBound = 26033

testBound :: Int
testBound = 10^(5 :: Int)

bound :: Int
bound = globalBound

primesBelowBound :: [Int]
primesBelowBound = takeWhile (<= bound) primes

inputPrimes :: [Int]
-- 2 and 5 cannot be a part of prime pairs.
-- Any number with a least significant digit of 2 or 5
-- must be divisible by 2 or 5 respectively.
inputPrimes = deleteOrd primesBelowBound [2, 5]

isPrimeQuick :: Int -> Bool
isPrimeQuick = (`IS.member` primesSet)
  where
    primesSet = IS.fromAscList $ takeWhile (<= concatDigits bound bound) primes


-- Although it is given that [3, 7, 109, 673] is a set of four primes which
-- satisfies the prime pair property, it could very well be the case that
-- it cannot be extended to a set of five primes.
-- Thus, we cannot use this set as a starting point.

concatDigits :: Int -> Int -> Int
concatDigits a b = a*mul + b
  where
    mul = findMul 1
    findMul m = if m > b
                then m
                else findMul (10*m)

isPrimePair :: Int -> Int -> Bool
isPrimePair a b = isPrimeQuick (concatDigits a b) && isPrimeQuick (concatDigits b a)

addPrime :: [Int] -> [[Int]]
addPrime xs = if null addablePrimes
              then []
              else map (:xs) addablePrimes
  where
    possiblePrimes = dropWhile (<= maximum xs) inputPrimes
    addable :: Int -> Bool
    addable n = all (isPrimePair n) xs
    addablePrimes = filter addable possiblePrimes

extendPrimePairs :: Int -> [Int] -> [[Int]]
extendPrimePairs size = foldr1 (>=>) (replicate (size-1) addPrime)

primePairs :: Int -> [[Int]]
primePairs size = concatMap (\ p -> extendPrimePairs size [p]) inputPrimes


main :: IO ()
main = print $ primePairs 5
