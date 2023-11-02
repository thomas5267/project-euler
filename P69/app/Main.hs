module Main where

import Euler.Helper
import Data.Numbers.Primes


eulerPhiPrime :: Int -> Int -> Int
-- eulerPhiPrime p k returns phi(p^k)
eulerPhiPrime p k = p^(k-1) * (p-1)

primeFactorsAList :: Int -> [(Int, Int)]
-- primeFactorsAList n = map (\l@(x:_) -> (x, length l)) (group $ primeFactors n)
primeFactorsAList n = tail $ go (0, 0) (primeFactors n)
  where
    go :: (Int, Int) -> [Int] -> [(Int, Int)]
    go acc []     = [acc]
    go (a, c) (x:xs)
      | a == x    = go (a, c+1) xs
      | otherwise = (a, c) : go (x, 1) xs

eulerPhi :: Int -> Int
eulerPhi = product . map (uncurry eulerPhiPrime) . primeFactorsAList

weird :: Int -> (Double, Int)
weird n = (fromIntegral n / fromIntegral (eulerPhi n), n)

answer :: (Double, Int)
answer = (maximum . map weird) ([4..1000000] `deleteOrd` primes)

main :: IO ()
main = print answer
