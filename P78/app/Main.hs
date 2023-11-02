{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Euler.Helper

import Data.List (elemIndex)
import Data.MemoTrie

p :: Integer -> Integer
-- p calculates how many partitions a set of n elements have.
p = memo p'
  where
    p' 0 = 1
    p' 1 = 1
    p' n = sum (map (q n) [1..(n-s-1)]) + sum (map p [2..s]) + 2 -- p 0 + p 1 = 2
      where s = n `quot` 2

q :: Integer -> Integer -> Integer
-- q calculates how many partitions a set of n elements have,
-- with the restriction that the largest subset contains exactly k elements.
q = memo2 q'
  where
    q' n k
      | k == 1    = 1
      | k == n    = 1
      | otherwise = sum (map (q (n-k)) [1..(min (n-k) k)])

mil :: Int
mil = 1000000

-- newtype SumMod = SumMod { getSumMod :: Int} deriving (Show, Eq)
-- 
-- instance Num SumMod where
--   (+) (SumMod x) (SumMod y) = SumMod $ x + y `mod` mil
--   (*) (SumMod x) (SumMod y) = SumMod $ x * y `mod` mil
--   abs (SumMod x) = SumMod $ abs x
--   signum (SumMod x) = SumMod $ signum x
--   fromInteger a = SumMod $ fromInteger (a `mod` fromIntegral mil)
--   negate (SumMod x) = SumMod $ negate x `mod` mil
-- 
-- instance Semigroup SumMod where
--   (<>) x y = x + y
-- 
-- instance Monoid SumMod where
--   mempty = SumMod 0

p_sum_mod :: Int -> Int
p_sum_mod = memo slow_p_sum_mod
  where
    slow_p_sum_mod 0 = 1
    slow_p_sum_mod n = (p_sum_mod (n-1) + p_mod n) `rem` mil

p_mod :: Int -> Int
-- p calculates how many partitions a set of n elements have mod 1000000.
p_mod = memo slow_p_mod
  where
    slow_p_mod 0 = 1
    slow_p_mod 1 = 1
    slow_p_mod n = (sum (map (q_mod n) [1..(n-s-1)]) + p_sum_mod s) `rem` mil
      where
        s = n `quot` 2

q_mod :: Int -> Int -> Int
-- q calculates how many partitions a set of n elements have,
-- with the restriction that the largest subset contains exactly k elements.
q_mod = memo2 slow_q_mod
  where
    slow_q_mod n k
      | k == 1    = 1
      | k == n    = 1
      | otherwise = sum (map (q_mod (n-k)) [1..(min (n-k) k)]) `rem` mil

pentagon :: Int -> Int
pentagon = memo slow_pentagon
  where
    slow_pentagon n = (n*(3*n-1)) `quot` 2

natToInt :: Int -> Int
-- A bijection between natural numbers and integers.
-- n    0  1  2  3  4  5
-- f n  0  1 -1  2 -2  3
natToInt = memo slow_natToInt
  where
    slow_natToInt n = if even n then -s else s
      where s = (n+1) `quot` 2

p_mod_fast :: Int -> Int
-- Recursion via pentagonal number theorem.
-- https://en.wikipedia.org/wiki/Pentagonal_number_theorem
p_mod_fast = memo slow_p_mod_fast
  where
    slow_p_mod_fast 0 = 1
    slow_p_mod_fast 1 = 1
    slow_p_mod_fast n =
        sum (map combiner . takeWhile ((<= n) . fst) $
              map (toFst pentagon . natToInt) [1..]) `mod` mil
      where
        combiner (g, k) = sign * p_mod_fast (n-g)
          where
            sign :: Int
            sign = if even k then -1 else 1
-- binarySearchF :: forall a b. (Integral a, Ord b) => (a -> b) -> b -> a
-- -- Binary search on monotonic functions.
-- -- Returns the largest x such that f x <= k.
-- binarySearchF f k = uncurry binarySearchF' bound
--   where
--     bound :: (a, a)
--     bound = (0, genUpperBound 100)
--       where
--         genUpperBound b = if k <= f b then b else genUpperBound ((b+1)*2)
--     binarySearchF' l r
--       | l == r    = l
--       | otherwise = case k `compare` y of
--                       LT -> binarySearchF' l (m-1)
--                       _  -> binarySearchF' m r
--       where (s, t) = (l + r) `quotRem` 2
--             m = s + t
--             y = f m
-- 
-- searchMaybe :: (Integral a, Ord b) => ((a -> b) -> b -> a) -> (a -> b) -> b -> Maybe a
-- searchMaybe search f k = if f (search f k) == k then Just searchResult else Nothing
--   where searchResult = search f k

main :: IO ()
main = print $ elemIndex 0 (map p_mod_fast [0..])
-- main = print $ p_mod_fast 10000
