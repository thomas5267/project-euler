module Main where

import Euler.Helper
import Data.Numbers.Primes
import Data.List (sort)


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

candidates :: [(Int, Int)]
candidates = (filter (\p -> (sort . digits . fst) p == (sort . digits . snd) p)
               . map (toFst eulerPhi))
               [2..10^(7 :: Int)]

answer :: (Double, Int)
answer = (minimum . map (\p@(_, n) -> (go p, n))) candidates
  where
    go (phi, n) = fromIntegral n / fromIntegral phi

main :: IO ()
main = print answer
