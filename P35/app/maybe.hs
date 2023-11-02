module Main where

import Euler.Helper
import Data.Numbers.Primes

import Control.Monad

import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV



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

primesVec :: Int -> V.Vector (Maybe Bool)
primesVec n = ((iv V.//) . map (toSnd (const Nothing)) . takeWhile (< n)) primes
  where
    iv = V.replicate (n+1) (Just False)


testP :: V.Vector (Maybe Bool) -> Int -> V.Vector (Maybe Bool)
-- testP tests whether p is a circular prime.
-- If p is a circular prime, change all the circular permutations to Just True.
-- If p is not a circular prime, change all the circular permutations to Just False.
testP !v p = destructiveUpdate v updates
  where
    bound = V.length v

    isCircularPrimeQuick :: Int -> Bool
    isCircularPrimeQuick k =
        case v V.!? k of
          Nothing           -> isPrime k
          Just Nothing      -> True
          Just (Just True)  -> True
          Just (Just False) -> False

    updates :: [(Int, Maybe Bool)]
    updates =
        [ (k, Just isCircular) | k <- filter (\ x -> x <= bound && x >= p) candidates ]
      where
        candidates = (map fromDigits . circularPermutations . digits) p
        isCircular = all isCircularPrimeQuick candidates

    destructiveUpdate :: V.Vector a -> [(Int, a)] -> V.Vector a
    destructiveUpdate w xs = V.modify (forM_ xs . uncurry . MV.write) w

iterator :: [Int] -> V.Vector (Maybe Bool) -> V.Vector (Maybe Bool)
-- iterator finds the smallest prime that has not been processed
-- and pass it to testP for processing.
-- It does so by keeping track of the smallest prime that may have not yet been processed
-- and checks if it has been processed by consulting the vector.
iterator [] v = v
iterator (x:xs) v =
    case V.elemIndex Nothing (V.drop x v) of
      Nothing -> v
      Just i  -> iterator nxs nv
        where
          nv = testP v (x+i)
          nxs = dropWhile (<= (x+i)) xs

results :: Int -> V.Vector (Maybe Bool)
results n = iterator primesInBound iv
  where
    primesInBound = takeWhile (<= n) primes
    iv = ((V.replicate (n+1) (Just False) V.//) . map (toSnd (const Nothing))) primesInBound

answer :: Int -> Int
answer = V.length . V.elemIndices (Just True) . results

test :: V.Vector (Maybe Bool)
test = primesVec 100

main :: IO ()
main = print $ answer 100000 -- globalBound
