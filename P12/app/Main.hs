module Main where

import Data.Numbers.Primes
import Data.List (group, find, foldl')

triangleNumbers :: [Int]
triangleNumbers = map f [1..]
  where f n = n * (n+1) `div` 2

groupAndCount :: [Int] -> [(Int, Int)]
groupAndCount l = map (\i@(x:xs) -> (x, length i)) (group l)

-- Divisor function is multiplicative.
-- If a and b are coprime, then divisor (a*b) = divisor a * divisor b
-- For prime powers, divisor (p^n) = n + 1

numOfDivisors :: Int -> Int
numOfDivisors n = foldl' (\acc l -> acc * (length l + 1)) 1 (group (primeFactors n))

-- numOfDivisors :: Int -> Int
-- numOfDivisors n = numOfDivisors' 1 0 1 (primeFactors n)
--   where numOfDivisors' :: Int -> Int -> Int -> [Int] -> Int
--         -- c is the total count
--         -- e is the exponent of p in prime factorisation of n
--         -- p is the prime currently processing
--         -- q is the next prime factor
--         numOfDivisors' c e p [] = c*(e+1)
--         numOfDivisors' c e p (q:qs)
--           | p == q    = numOfDivisors' c         (e+1) p qs
--           | otherwise = numOfDivisors' (c*(e+1)) 1     q qs

answer :: Maybe (Int, Int)
answer = find (\(_, b) -> b > 500) $ map (\x -> (x, numOfDivisors x)) triangleNumbers

main :: IO ()
main = print answer
