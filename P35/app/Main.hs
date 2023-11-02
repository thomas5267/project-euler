module Main where

import Euler.Helper
import Data.Numbers.Primes

import Data.Vector.Unboxed qualified as V
import Data.Bit.ThreadSafe


globalBound :: Int
globalBound = 10^(6 :: Int)


circularPermutations :: [a] -> [[a]]
circularPermutations xs = go lenXs (cycle xs)
  where
    lenXs = length xs
    go 0 _  = []
    go n ys = circular : go (n-1) (tail rest)
      where
        (circular, rest) = splitAt lenXs ys

primesVec :: Int -> V.Vector Bit
primesVec n = ((iv V.//) . map (toSnd (const (Bit True))) . takeWhile (< n)) primes
  where
    iv = V.replicate (n+1) (Bit False)


testP :: V.Vector Bit -> Int -> V.Vector Bit
-- testP tests whether p is a circular prime.
-- If p is a circular prime, do nothing.
-- If p is not a circular prime,
-- set v V.! p to Bit False,
-- and set the bits corresponding to circular permutations of p to False.
testP v p = v V.// updates
  where
    bound = V.length v

    isCircularPrimeQuick :: Int -> Bool
    isCircularPrimeQuick k =
        if k <= bound
        then unBit $ v V.! k
        else isPrime k

    updates :: [(Int, Bit)]
    updates =
        if all odd digitsP && all isCircularPrimeQuick candidates
        then []
        else [ (k, Bit False) | k <- filter (\ x -> x <= bound && x >= p) candidates ]
      where
        digitsP = digits p
        candidates = (map fromDigits . circularPermutations) digitsP

iterator :: [Int] -> V.Vector Bit -> V.Vector Bit
-- iterator finds the smallest prime that has not been processed
-- and pass it to testP for processing.
-- It does so by keeping track of the smallest prime that may have not yet been processed
-- and checks if it has been processed by consulting the vector.
iterator [] v = v
iterator (x:xs) !v =
    case V.elemIndex (Bit True) (V.drop x v) of
      Nothing -> v
      Just i  -> iterator nxs nv
        where
          nv = testP v (x+i)
          nxs = dropWhile (<= (x+i)) xs

results :: Int -> V.Vector Bit
results n = iterator primesInBound iv
  where
    primesInBound = takeWhile (<= n) primes
    iv = ((V.replicate (n+1) (Bit False) V.//) . map (toSnd (const (Bit True)))) primesInBound

answer :: Int -> Int
answer = countBits . results

test :: V.Vector Bit
test = primesVec 100

main :: IO ()
main = print $ answer globalBound
