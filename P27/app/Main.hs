module Main where

import Data.Numbers.Primes

import Data.List (maximumBy)
import Data.Ord (comparing)

-- n^2 + an + b
-- The question asks for the quadratic with the most number of
-- consecutive primes starting from n = 0.
-- As such, b must be a prime element.
--
-- b^2 + ab + b is divisible by b.
-- Hence, the number of consecutive primes cannot be greater than length [0..b] == b+1.
--
-- a cannot be even.
-- If a were to be even and b /= 2, then n^2 + an + b = n^2 + 1 mod 2.
-- 1^2 + 1 = 0 mod 2.
-- Hence, if a is even, then the quadratic has exactly one prime value, namely (0, b).
--
-- Consider n^2 + an + b = n^2 + b mod a
-- If (-b) is quadratic residue mod a,
-- then there exist an n such that
-- n^2 = -b mod a.
-- Hence, n^2 + an + b = -b + 0 + b = 0 mod a.
-- The quadratic is divisible by a.
--
-- Can we say anything about n^2 + an + b mod 3?
-- b is a prime and is necessarily congruent to 1 or 2 (mod 3).
-- a can be 0, 1, or 2 (mod 3)
-- n^2 is 0 or 1 (mod 3)
--
-- Case by case:
--
-- b = 1 mod 3, a = 0 mod 3
-- No restriction
--
-- b = 1 mod 3, a = 1 mod 3
-- When n = 1, n^2 + n + 1 = 0 mod 3
--
-- b = 1 mod 3, a = 2 mod 3
-- When n = 2, n^2 + 2n + 1 = 0 mod 3
--
-- b = 2 mod 3, a = 0 mod 3
-- When n = 1, n^2 + 1 = 0 mod 3
--
-- b = 2 mod 3, a = 1 mod 3
-- No restriction
--
-- b = 2 mod 3, a = 2 mod 3
-- No restriction
--
-- Doesn't seem to worth the time.

withNegative :: [Int] -> [Int]
-- withNegative takes a list of numbers and joints it with the negation of every number.
withNegative = concatMap (\ n -> [-n, n])

extIsPrime :: Int -> Bool
-- Extends isPrime to negative numbers.
-- A negative number is considered prime if its negation is prime.
extIsPrime = isPrime . abs

quadratic :: Int -> Int -> Int -> Int
-- Quadtratic functions of the form n^2 + an + b for integral n, a, b.
quadratic a b n = n^(2 :: Int) + a*n + b

goodness :: Int -> Int -> (Int, (Int, Int))
goodness a b = (score, (a, b))
  where
    score = (length . takeWhile extIsPrime . map (quadratic a b)) [0..]

results :: [(Int, (Int, Int))]
results = [ goodness a b | a <- withNegative [1, 3..1000],
                           b <- withNegative (takeWhile (<=1000) primes)
          ]

answer :: (Int, (Int, Int))
answer = maximumBy (comparing fst) results

main :: IO ()
main = print answer
