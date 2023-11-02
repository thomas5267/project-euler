module Main where
import Data.Numbers.Primes
import Data.List (group)

import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Bit

import Data.Foldable (foldl')
import Data.Maybe (fromJust)

import Control.Applicative
import Control.Monad
import Control.Monad.ST

bound :: Int
bound = 28123

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

type PPSDMap = IntMap.IntMap (V.Vector Int)

ppsdMap :: PPSDMap
-- ppsd stands for prime power sum (of) divisors
-- ppsdMap returns a prime-indexed map with vectors containing sum of divisors of prime powers
ppsdMap = IntMap.fromList ppsdList
  where
    ppsdList :: [(Int, V.Vector Int)]
    ppsdList = map (toSnd ppsdVec) $ takeWhile (<= bound) primes
    ppsdVec :: Int -> V.Vector Int
    ppsdVec p = V.fromList $ map ppsd ppList
      where
        ppList = takeWhile (<= bound) (map (p^) [1..])
        ppsd pp = (pp*p - 1) `quot` (p-1)

-- primeFactorsAList :: Int -> [(Int, Int)]
-- primeFactorsAList n = map (\l@(x:_) -> (x, length l)) (group $ primeFactors n)

primeFactorsAList :: Int -> [(Int, Int)]
primeFactorsAList n = factors 2 0 n primes
  where
    factors 2 0 1 _ = []
    factors p k m (a:as)
      | m == 1                = [(p,k)]
      | (m < p*p) && (k == 0) = [(m,1)]
      | r == 0                = factors p (k+1) q (a:as)
      | k == 0                = factors a 0 m as
      | otherwise             = (p,k) : factors a 0 m as
      where (q,r) = m `quotRem` p

(#) :: PPSDMap -> (Int, Int) -> Int
(#) m (a,b) = m IntMap.! a V.! (b-1) -- zero-based indexing
infixl 9 #

sumDivisors :: Int -> Int
-- We note that sum of divisors is a multiplicative function, i.e.
-- sumDivisors (i*j) == sumDivisors i * sumDivisors j if i, j coprime.
-- Therefore, factorise n into prime powers,
-- then use the identity to compute sumDivisors n.
sumDivisors 1 = 1
sumDivisors n = product $ map (ppsdMap #) (primeFactorsAList n)

abundantNumbers :: [Int]
abundantNumbers = filter isAbundant [1..bound]
  where isAbundant n = sumDivisors n > 2*n

abundantNumbersVec :: V.Vector Int
abundantNumbersVec = V.fromList abundantNumbers

-- nonAbundantSums :: V.Vector Bit
-- nonAbundantSums = V.generate (bound+1) (Bit . not . isSum)
--   where
--     s = IntSet.fromAscList abundantNumbers
--     isSum :: Int -> Bool
--     isSum n = isSum' abundantNumbersVec
--       where
--         t = (n `quot` 2) + (n `rem` 2)
--         isSum' v
--           | V.null v         = False
--           | a > t            = False
--           | isAbundant (n-a) = True
--           | otherwise        = isSum' as
--           where (a, as) = fromJust $ V.uncons v
--                 isAbundant x = x `IntSet.member` s
--                 {-# SCC isAbundant #-}

-- nonAbundantSums :: V.Vector Bit
-- nonAbundantSums = V.generate (bound+1) isNotSum
--   where
--     s = V.generate (bound+1) (\x -> Bit $ x `elemOrd` abundantNumbersVec)
--     isNotSum :: Int -> Bit
--     isNotSum n = V.foldr isNotSum' (Bit True) abundantNumbersVec
--       where
--         t = (n `quot` 2) + (n `rem` 2)
--         isNotSum' a bit
--           | a > t            = Bit True
--           | isAbundant (n-a) = Bit False
--           | otherwise        = bit
--           where isAbundant x = unBit $ s V.! x
--                 {-# SCC isAbundant #-}

nonAbundantSums :: V.Vector Bit
nonAbundantSums = V.generate (bound+1) isNotSum
  where
    s = V.fromList (helper 0 abundantNumbers)
    helper :: Int -> [Int] -> [Bit]
    helper _ [] = []
    helper n l@(a:as)
      | n < a     = Bit False : helper (n+1) l
      | otherwise = Bit True : helper (n+1) as
    isNotSum :: Int -> Bit
    isNotSum n = V.foldr isNotSum' (Bit True) abundantNumbersVec
      where
        t = (n `quot` 2) + (n `rem` 2)
        isNotSum' a bit
          | a > t            = Bit True
          | isAbundant (n-a) = Bit False
          | otherwise        = bit
          where isAbundant x = unBit $ s V.! x
                {-# SCC isAbundant #-}


elemOrd :: (Ord a, Num a, V.Unbox a) => a -> V.Vector a -> Bool
-- Binary search on vectors
elemOrd k v
  | V.null v = False
  | otherwise = elemOrd' 0 (V.length v - 1) v
  where
    elemOrd' l r v
      | l == r = v V.! m == k
      | x >  k = elemOrd' l (m-1) v
      | x <= k = elemOrd' m r     v
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            x = v V.! m

-- isSum :: V.Vector Int -> Int -> Bool
-- -- isSum takes a sorted vector v and a number n.
-- -- Returns True if and only if there exists two elements of v which sum to n.
-- isSum v n
--   | V.null v  = False
--   | otherwise = V.any pred v
--   where
--     s = IntSet.fromAscList $ V.toList v
--     pred a = (n-a) `IntSet.member` s


-- isSum :: (Ord a, Num a, Integral a, V.Unbox a) => V.Vector a -> a -> Bool
-- -- isSum takes a sorted vector v and a number n.
-- -- Returns True if and only if there exists two elements of v which sum to n.
-- isSum v n
--   | V.null v = False
--   | otherwise     = isSum' 0 (V.length v - 1) v
--   where
--     isSum' l r v
--       | l > r         = False
--       | d < m         = False
--       | d `elemOrd` v = True
--       | otherwise     = isSum' (l+1) r v
--       where
--         a = v V.! l
--         (s, t) = a `quotRem` 2
--         m = s + t
--         d = n - a

dropContaining :: Ord a => [a] -> [a] -> [a]
-- Takes two sorted list a and b, drop every element of b from a
dropContaining []     _  = []
dropContaining l@(a:as) [] = l
dropContaining al@(a:as) bl@(b:bs)
  | a <  b = a : dropContaining as bl
  | a == b = dropContaining as bl
  | a >  b = dropContaining al bs

main :: IO ()
main = print $ sum $ listBits nonAbundantSums
