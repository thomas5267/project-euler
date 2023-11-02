module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.IntSet as IS

import Control.Monad

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)
{-# INLINE mapFst #-}

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
{-# INLINE mapSnd #-}

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
{-# INLINE toFst #-}

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

elemOrdV :: (Ord a, V.Unbox a) => a -> V.Vector a -> Bool
-- Binary search on vectors
elemOrdV k v
  | V.null v = False
  | otherwise = elemOrdV' 0 (V.length v - 1)
  where
    elemOrdV' l r
      | l == r = v V.! m == k
      | k <  y = elemOrdV' l (m-1)
      | k >= y = elemOrdV' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            y = v `V.unsafeIndex` m

elemOrdF :: (Integral a, Ord b) => b -> (a -> b) -> Bool
-- Binary search on monotonic functions
elemOrdF k f = uncurry elemOrdF' bound
  where
    bound = (1, genUpperBound 100)
      where
        genUpperBound b = if k <= f b then b else genUpperBound ((b+1)*2)
    elemOrdF' l r
      | l == r = f m == k
      | k <  y = elemOrdF' l (m-1)
      | k >= y = elemOrdF' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            y = f m

elemOrdLF :: (Integral a, Ord b) => b -> (a -> b) -> Bool
-- Linear search on monotonic functions
elemOrdLF k f = elemOrdLF' 1
  where
    elemOrdLF' n
      | f n >  k = False
      | f n == k = True
      | f n <  k = elemOrdLF' (n+1)

pentagonalNumber :: Int -> Int
pentagonalNumber n = (n * (3*n - 1)) `quot` 2
-- pentagonalNumber = (map slow [0..] !!)
--   where slow n = (n * (3*n - 1)) `quot` 2

diffPenta :: Int -> Int -> Int
diffPenta m n = pentagonalNumber m - pentagonalNumber n

findAllDiffPenta :: Int -> [(Int, Int)]
-- Finds all pairs of pentagonal numbers (P_n, P_{n+k}) such that
-- P_{n+k} - P_n is equal to some pentagonal number P_d, where d <= h.
-- Returns (n, k)

findAllDiffPenta h = result
  where
    bound = (pentagonalNumber h - 1) `quot` 3
    memo = V.generate (bound+2) pentagonalNumber
    isDiffPentagonal m n = (memo V.! m - memo V.! n) `elemOrdV` memo
    result = do
        n <- [1..bound]
        k <- [1..bound+1-n]
        guard (isDiffPentagonal (n+k) n)
        return (n, k)

findAllGoodPenta :: Int -> [(Int, Int)]
-- Finds all pairs of pentagonal numbers such that their difference
-- is equal to some pentagonal number P_k, where k <= h, and their sum
-- is also a pentagonal number not necessarily smaller than h.
findAllGoodPenta h = filter isGood candidates
  where
    isGood (n, k) = sumPenta `elemOrdF` pentagonalNumber
      where sumPenta = pentagonalNumber n + pentagonalNumber (n+k)
    candidates :: [(Int, Int)]
    -- Finds all pairs of pentagonal numbers (P_n, P_{n+k}) such that
    -- P_{n+k} - P_n is equal to some pentagonal number P_d, where d <= h.
    -- Returns (n, k)

    candidates = do
           --  n <- [1..bound]
           --  k <- [1..bound+1-n]
            n <- [ x | x <- [1..bound],
                       let r = x `rem` 5 in r `elem` [0..2]]
            k <- if n `rem` 5 == 1 then [5,10..bound+1-n] else [1..bound+1-n]
            guard (isDiffPentagonal (n+k) n)
            return (n, k)
      where
        bound = (pentagonalNumber h - 1) `quot` 3
        pentaMemo = map pentagonalNumber [0..bound+1]
        memoV = V.fromList pentaMemo
        memoS = IS.fromDistinctAscList pentaMemo
        isDiffPentagonal m n = (memoV V.! m - memoV V.! n) `IS.member` memoS


main :: IO ()
main = print $ findAllGoodPenta 100
