module Main where

import Euler.Helper
import Data.Numbers.Primes

import Data.Word
import Data.Vector.Unboxed qualified as V


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


-- Encode Maybe Bool with the following
-- Nothing = 0
-- Just True = 1
-- Just False = 2

primesVec :: Int -> V.Vector Word8
primesVec n = ((iv V.//) . map (toSnd (const 0)) . takeWhile (< n)) primes
  where
    iv = V.replicate (n+1) 1


testP :: V.Vector Word8 -> Int -> V.Vector Word8
-- testP tests whether p is a circular prime.
-- If p is a circular prime, change all the circular permutations to Just True.
-- If p is not a circular prime, change all the circular permutations to Just False.
testP v p = v V.// updates
  where
    bound = V.length v

    isCircularPrimeQuick :: Int -> Bool
    isCircularPrimeQuick k =
        case v V.!? k of
          Nothing -> isPrime k
          Just 0  -> True
          Just 1  -> True
          Just 2  -> False
          _       -> errorWithoutStackTrace "wtf"

    updates :: [(Int, Word8)]
    updates =
        [ (k, encodedBool) | k <- filter (\ x -> x <= bound && x >= p) candidates ]
      where
        candidates = (map fromDigits . circularPermutations . digits) p
        isCircular = all isCircularPrimeQuick candidates
        encodedBool = if isCircular then 1 else 2

iterator :: [Int] -> V.Vector Word8 -> V.Vector Word8
-- iterator finds the smallest prime that has not been processed
-- and pass it to testP for processing.
-- It does so by keeping track of the smallest prime that may have not yet been processed
-- and checks if it has been processed by consulting the vector.
iterator [] v = v
iterator (x:xs) v =
    case V.elemIndex 0 (V.drop x v) of
      Nothing -> v
      Just i  -> iterator nxs nv
        where
          nv = testP v (x+i)
          nxs = dropWhile (<= (x+i)) xs

results :: Int -> V.Vector Word8
results n = iterator primesInBound iv
  where
    primesInBound = takeWhile (<= n) primes
    iv = ((V.replicate (n+1) 2 V.//) . map (toSnd (const 0))) primesInBound

answer :: Int -> Int
answer = V.length . V.elemIndices 1 . results

test :: V.Vector Word8
test = primesVec 100

main :: IO ()
main = print $ answer globalBound
