{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Main where

import Euler.Helper

import Data.Numbers.Primes

import Data.MemoTrie

import Data.List (sort)
import Data.Function (on)
import Data.Foldable
import Data.Maybe (fromJust)

import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
{-# INLINE fst3 #-}

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
{-# INLINE snd3 #-}

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
{-# INLINE thd3 #-}

both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)
{-# INLINE both #-}


-- Expr contains the length of the expression,
-- the valuation of the expression,
-- and the expression itself in an OccList.
type Expr = (Int, Int, IM.IntMap Int)

len :: Expr -> Int
len = fst3
{-# INLINE len #-}

fastFactor :: Int -> IM.IntMap Int
fastFactor = memo $ foldl' go IM.empty . primeFactors
  where
    go acc p = IM.insertWith (+) p 1 acc

composites :: [Int]
composites = deleteOrd [2..] primes

size :: IM.IntMap Int -> Int
size = IM.foldl' (+) 0

valuation :: IM.IntMap Int -> Int
valuation = IM.foldlWithKey' (\acc f c -> acc*f^c) 1

sumValue :: IM.IntMap Int -> Int
sumValue = IM.foldlWithKey' (\acc f c -> acc + f*c) 0

expr :: Int -> Expr
-- Returns a triple containing the length of the longest expression of n,
-- n itself, and the prime factorisation of n.
-- The longest expression is the prime factorisation of n then padded with enough 1's
-- to make it a product-sum number.
expr = memo expr'
  where
    expr' n = (p-s+l, n, factors)
      where
        factors = fastFactor n
        (p, s, l) = (valuation factors, sumValue factors, size factors)

largestDivisor :: Int -> (Int, Int)
largestDivisor n = head [ (q, k) | k <- [2..sqrtN],
                                   let (q, r) = n `quotRem` k,
                                   r == 0 ]
  where
    sqrtN = floor (sqrt @Float (fromIntegral n))


contractPrime :: Int -> Expr -> [Expr]
-- Assumes p is prime.
contractPrime p (l, z, fs) =
    (nubOrd . sort) (nx0 : otherContractions)
  where
    nx0 = (nl0, nz0, nf0)
    nf0 = IM.unionWith (+) (IM.singleton p 1) fs
    nz0 = p*z
    nl0 = l + (p-1)*z + 1 - p

    otherContractions =
        if IM.findWithDefault 0 p fs <= 1
        then temp
        else []

    temp = map (toExpr . ($ fs)) (IM.foldrWithKey go [] fs)
      where
        go :: IM.Key -> Int -> [IM.IntMap Int -> IM.IntMap Int]-> [IM.IntMap Int -> IM.IntMap Int]
        go k a acc = (IM.insertWith (+) (p*k) 1 . IM.update dec k) : acc
          where
            dec = const (if a >= 2 then Just (a-1) else Nothing)
        toExpr nf = (nz0 - ns + nl, nz0, nf)
          where
            ns = sumValue nf
            nl = size nf

contractions :: Expr -> [Expr]
-- We can reuse the computation for contractions in the following way.
-- Let x be the largest divisor of y.
-- Every expression of x can be "embedded" into an expression of y.
-- E.g.
-- (18,24,OccList [(2,1),(3,1),(4,1)]) can be embedded into
-- (41,48,OccList [(2,2),(3,1),(4,1)])
-- Partition the factors of y into two sets, factors of x and not factors of x.
-- Calculate the expressions for the two sets individually,
-- then combine the expressions together and consider
-- the cross-contractions from both set.
contractions x@(l, z, _)
  | l == 1    = [x]
  | otherwise = nubOrd . sort $ concatMap (contractPrime q) (contractExpr d)
  where
    (d, q) = largestDivisor z

contractExpr :: Int -> [Expr]
contractExpr = memo (contractions . expr)

backfillStrong :: [Expr] -> [Expr]
backfillStrong l = concat $ zipWith go l (tail l)
  where
    go x y = filter (\y' -> len y' > len x) (contractions y)


expressions :: Int -> [Expr]
-- Returns the all longest expressions of length smaller or equal to bound.
expressions bound = takeWhile ((<= bound) . len) $ map expr composites

filteredExpressions :: Int -> [Expr]
filteredExpressions = nubByOrd ((==) `on` fst3) . sort . expressions

strong :: [Expr] -> [Expr]
strong = strong' 0
  where
    strong' _ []     = []
    strong' m (x:xs) = if len x > m
                       then x : strong' (len x) xs
                       else strong' m xs

strongTestExpr :: [Expr]
strongTestExpr = strong $ expressions 100

strongExpr :: Int -> [Expr]
strongExpr = strong . expressions

strongContractions :: Int -> [Expr]
strongContractions bound = takeWhile ((<= bound) . len) $ go 1 (map expr composites)
  where
    go _ []     = []
    go m (x:xs) = if c > m
                  then filter ((> m) . len) (contractExpr (snd3 x)) ++ go c xs
                  else go c xs
      where c = len x

missingExpr :: Int -> [Int]
missingExpr n = [1..n] `deleteOrd` backfilled
  where
    backfilled = (map len . backfillStrong . strongExpr) n

shortestExpression :: Int -> Int
-- Returns the smallest valuation of any expressions of length n.
shortestExpression n =
    snd3 $
    fromJust $
    find ((== n) . len) $
    concatMap contractExpr [n..2*n]

allShortestExpression :: Int -> V.Vector Int
-- Returns the valuation of all shortest expressions of length equal to or below bound.
allShortestExpression bound =
    V.accum min withStrong updates
  where
    start = V.replicate (bound+1) (2*bound)
    process = map (\(l, z, _) -> (l, z))
    allStrong = strongContractions bound
    withStrong = V.accum min start (process allStrong)
    missing = [2..bound] `deleteOrd` map fst3 allStrong
    updates = map (toSnd shortestExpression) missing

sumShortestExpression :: Int -> Int
sumShortestExpression = IS.foldl' (+) 0 . IS.fromList . V.toList . V.drop 2 . allShortestExpression

-- sumShortestExpression :: Int -> Int
-- sumShortestExpression bound = IS.foldl' (+) 0 (strongBackfillIS `IS.union` rest)
--   where
--     toIntSet = IS.fromList . map snd3
--     strongBackfill = (backfillStrong . strongExpr) bound
--     strongBackfillIS = toIntSet strongBackfill
--     rest = IS.fromList $ map shortestExpression $ [2..bound] `deleteOrd` map len (sort strongBackfill)








main :: IO ()
main = print $ sumShortestExpression 12000
