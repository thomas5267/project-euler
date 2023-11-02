module Main where

import Data.MemoTrie

import Data.Numbers.Primes

primeFactorsAList :: Int -> [(Int, Int)]
primeFactorsAList n = tail $ go (0, 0) (primeFactors n)
  where
    go :: (Int, Int) -> [Int] -> [(Int, Int)]
    go acc []     = [acc]
    go (a, c) (x:xs)
      | a == x    = go (a, c+1) xs
      | otherwise = (a, c) : go (x, 1) xs

sumDivisorsPP :: Int -> Int -> Int
-- Returns the sum of divisors of prime powers
sumDivisorsPP = memo2 slow_sumDivisorsPP
  where slow_sumDivisorsPP p k = (p^(k+1) - 1) `quot` (p-1)

sumDivisors :: Int -> Int
-- We note that sum of divisors is a multiplicative function, i.e.
-- sumDivisors (i*j) == sumDivisors i * sumDivisors j if i, j coprime.
-- Therefore, factorise n into prime powers,
-- then use the identity to compute sumDivisors n.
sumDivisors = memo $ product . map (uncurry sumDivisorsPP) . primeFactorsAList

sumProperDivisors :: Int -> Int
sumProperDivisors = memo slow_sumProperDivisors
    where slow_sumProperDivisors n = sumDivisors n - n

results :: [(Int, Int)]
results = (filter isAmicable . map go) [2..10000]
  where
    isAmicable (a, b) = a == b && a /= sumProperDivisors a
    go n = (n, sumProperDivisors (sumProperDivisors n))

answer :: Int
answer = (sum . map fst) results

main :: IO ()
main = print answer
