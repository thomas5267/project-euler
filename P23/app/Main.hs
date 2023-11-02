module Main where
import Data.Numbers.Primes
import Data.List (group)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as Vector
import qualified Data.IntSet as IntSet
import Data.Foldable (foldl')

bound :: Int
bound = 28123

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

type PPSDMap = IntMap.IntMap (Vector.Vector Int)

ppsdMap :: PPSDMap
-- ppsd stands for prime power sum (of) divisors
-- ppsdMap returns a prime-indexed map with vectors containing sum of divisors of prime powers
ppsdMap = IntMap.fromList ppsdList
  where
    ppsdList :: [(Int, Vector.Vector Int)]
    ppsdList = map (toSnd ppsdVec) $ takeWhile (<= bound) primes
    ppsdVec :: Int -> Vector.Vector Int
    ppsdVec p = Vector.fromList $ map ppsd ppList
      where
        ppList = takeWhile (<= bound) (map (p^) [1..])
        ppsd pp = (pp * p - 1) `div` (p-1)

primeFactorsAList :: Int -> [(Int, Int)]
primeFactorsAList n = map (\l@(x:_) -> (x, length l)) (group $ primeFactors n)

(#) :: PPSDMap -> (Int, Int) -> Int
(#) m (a,b) = m IntMap.! a Vector.! (b-1) -- zero-based indexing
infixl 9 #

sumDivisors :: Int -> Int
-- We note that sum of divisors is a multiplicative function, i.e.
-- sumDivisors (i*j) == sumDivisors i * sumDivisors j if i, j coprime.
-- Therefore, factorise n into prime powers,
-- then use the identity to compute sumDivisors n.
sumDivisors 1 = 1
sumDivisors n = product $ map (ppsdMap #) (primeFactorsAList n)

isAbundant :: Int -> Bool
isAbundant n = sumDivisors n - n > n

abundantNumbers :: [Int]
abundantNumbers = filter isAbundant [1..bound]

abundantSums :: [Int]
abundantSums = (IntSet.toAscList . IntSet.fromList) abundantSumsList
  where abundantSumsList = triangularSumWithBound bound abundantNumbers

-- elemOrd :: (Ord a, Num a, Vector.Unbox a) => a -> Vector.Vector a -> Bool
-- -- Binary search on vectors
-- elemOrd k v
--   | Vector.null v = False
--   | otherwise = elemOrd' k 0 (Vector.length v - 1) v
--   where
--     elemOrd' k l r v
--       | l == r = v Vector.! m == k
--       | x >  k = elemOrd' k l (m-1) v
--       | x <= k = elemOrd' k m r     v
--       where (s, t) = (l + r) `quotRem` 2
--             m = s + t
--             x = v Vector.! m
-- 
-- 
-- isSum :: (Ord a, Num a, Vector.Unbox a) => Vector.Vector a -> a -> Bool
-- -- isSum takes a sorted vector v and a number n.
-- -- Returns True if and only if there exists two elements of v which sum to n.
-- isSum v n
--   | Vector.null v = False
--   | otherwise     = isSum' 0 (Vector.length v - 1) v
--   where
--     isSum' l r v
--       | l > r = False
--       | (n-a) `elemOrd` v = True
--       | otherwise         = isSum' (l+1) r v
--       where a = v Vector.! l


triangularSumWithBound :: Int -> [Int] -> [Int]
-- Takes a sorted list and return all pairwise sum that are less than or equal to the bound
triangularSumWithBound n l = triangularSumWithBound' l l
  where
    triangularSumWithBound' []        _      = []
    triangularSumWithBound' (a:as)    []     = triangularSumWithBound' as as
    triangularSumWithBound' al@(a:as) (b:bs)
      | a >  n    = []
      | s <= n    = s : triangularSumWithBound' al bs
      | otherwise = triangularSumWithBound' as as
      where s = a + b

dropContaining :: Ord a => [a] -> [a] -> [a]
-- Takes two sorted list a and b, drop every element of b from a
dropContaining []     _  = []
dropContaining l@(a:as) [] = l
dropContaining al@(a:as) bl@(b:bs) =
    case a `compare` b of
      LT -> a : dropContaining as bl
      EQ -> dropContaining as bl
      GT -> dropContaining al bs

main :: IO ()
main = print $ sum $ dropContaining [1..bound] abundantSums
