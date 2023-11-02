{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Euler.Helper
import Data.Ratio

binarySearchF :: forall a b. (Integral a, Ord b) => b -> (a -> b) -> a
-- Binary search on monotonic functions.
-- Returns the largest x such that f x <= k.
binarySearchF k f = uncurry binarySearchF' bound
  where
    bound :: (a, a)
    bound = (0, genUpperBound 100)
      where
        genUpperBound b = if k <= f b then b else genUpperBound ((b+1)*2)
    binarySearchF' l r
      | l == r    = l
      | otherwise = case k `compare` y of
                      LT -> binarySearchF' l (m-1)
                      _  -> binarySearchF' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            y = f m
{-# INLINABLE binarySearchF #-}

biggestRational :: Rational -> Integer -> Rational
biggestRational k n = binarySearchF k (%n) % n

results :: [Rational]
results = map (biggestRational (3%7)) ([2..1000000] `deleteOrd` [7,14..])

answer :: Rational
answer = maximum $ filter (< (3%7)) results

main :: IO ()
main = print answer
