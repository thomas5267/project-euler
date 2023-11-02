module Main where
import Data.Numbers.Primes
import Data.List (group)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as Vector

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


isSum :: (Ord a, Num a) => [a] -> a -> Bool
-- isSum takes a sorted list v and a number n.
-- Returns True if and only if there exists two elements of v which sum to n.
isSum [] n = False
isSum v@(a:as) n
  | (n-a) `elem` v = True
  | otherwise      = isSum as n

-- triangularSumWithBound :: Int -> [Int] -> [Int]
-- -- Takes a sorted list and return all pairwise sum that are less than or equal to the bound
-- triangularSumWithBound n l = triangularSumWithBound' l l
--   where
--     triangularSumWithBound' []        _      = []
--     triangularSumWithBound' (a:as)    []     = triangularSumWithBound' as as
--     triangularSumWithBound' al@(a:as) (b:bs)
--       | s <= n    = s : triangularSumWithBound' al bs
--       | otherwise = triangularSumWithBound' as as
--       where s = a + b
-- 
-- sumAbundantNumbers :: [Int]
-- sumAbundantNumbers = triangularSumWithBound bound abundantNumbers

main :: IO ()
main = print $ sum $ filter (not . isSum abundantNumbers) [1..bound]
