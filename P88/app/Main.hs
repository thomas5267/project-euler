{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Main (main) where

import Euler.Helper

import Data.Numbers.Primes

import Data.MemoTrie

import Data.List (group, sort)
import Data.Function (on)
import Data.Foldable
import Data.Maybe (fromJust, fromMaybe)

import qualified Data.Vector.Unboxed as V
import qualified Data.IntSet as IS

import Control.Applicative


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


newtype OccList a = OccList { unwrapOccList :: [(a, Int)] } deriving (Eq, Ord)

instance (Show a) => Show (OccList a) where
  show (OccList l) = "OccList " ++ show l

toOccList :: (Ord a) => [a] -> OccList a
toOccList = OccList . map (\l -> (head l, length l)) . group . sort

wrapOccList :: (Ord a) => [(a, Int)] -> OccList a
wrapOccList = reduce . OccList

fromOccList :: OccList a -> [a]
fromOccList = toList

reduce :: (Ord a) => OccList a -> OccList a
reduce = OccList . go . sort . unwrapOccList
  where
    go []  = []
    go (x:xs) =
        if snd condensed /= 0
        then condensed : go havenot
        else go xs
      where
        (have, havenot) = span (((==) `on` fst) x) xs
        condensed = foldl' (\(f, c1) (_, c2) -> (f, c1 + c2)) x have
--     go xl  = go' x xs
--       where
--         (x, xs) = fromJust $ uncons xl
--         go' a [] = [ a | snd a /= 0 ]
--         go' a (b:bs)
--           | fst a == fst b = go' (addPairs a b) bs
--           | otherwise      = if snd a /= 0 then a : go' b bs else go' b bs
--           where addPairs (z, i) (_, j) = (z, i+j)

instance Functor OccList where
  fmap f = OccList . map (mapFst f) . unwrapOccList

instance Foldable OccList where
  foldr f acc (OccList xs) = foldr go acc xs
    where
      go (a, l) nacc = foldr f nacc (replicate l a)

  length = foldl' go 0 . unwrapOccList
    where
      go acc (_, c) = acc + c
  {-# INLINE length #-}

  product = foldl' go 1 . unwrapOccList
    where
      go acc (a, c) = acc * a^c
  {-# INLINE product #-}

  sum = foldl' go 0 . unwrapOccList
    where
      go acc (a, c) = acc + a * fromIntegral c
  {-# INLINE sum #-}


valuation :: OccList Int -> Int
valuation = product

instance Applicative OccList where
  pure a = OccList [(a, 1)]
  liftA2 f (OccList as) (OccList bs) =
      OccList $ [ (f a b, nl) | (a, al) <- as,
                                (b, bl) <- bs,
                                let nl = al*bl ]

instance Monad OccList where
  as >>= f =
      OccList $ [ x | (ol, l) <- unwrapOccList (fmap f as),
                      x <- map (mapSnd (l*)) (unwrapOccList ol) ]

instance Semigroup (OccList a) where
  (OccList a) <> (OccList b) = OccList (a <> b)

instance Monoid (OccList a) where
  mempty = OccList []


-- Expr contains the length of the expression,
-- the valuation of the expression,
-- and the expression itself in an OccList.
type Expr = (Int, Int, OccList Int)

len :: Expr -> Int
len = fst3
{-# INLINE len #-}

fastFactor :: Int -> OccList Int
fastFactor = memo $ toOccList . primeFactors

composites :: [Int]
composites = deleteOrd [2..] primes


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
        (p, s, l) = (product factors, sum factors, length factors)

wrapExpr :: OccList Int -> Expr
wrapExpr factors = (p-s+l, p, factors)
  where
    (p, s, l) = (product factors, sum factors, length factors)

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
    nf0 = reduce (pure p <> fs)
    nz0 = p*z
    nl0 = l + (p-1)*z + 1 - p

    otherContractions =
        if fromMaybe 0 (lookup p (unwrapOccList fs)) <= 1
        then temp
        else []

    temp = map (toExpr . wrapOccList) (help contractPairs (unwrapOccList fs))
      where
        toExpr nf = (nz0 - ns + nl, nz0, nf)
          where
            ns = sum nf
            nl = length nf

    -- help f [a, b, c]
    -- = [[f a, b, c], [a, f b, c], [a, b, f c]]
    -- but flattens (f a) to match the type.
    -- help is a fold?
    -- help :: (Eq a) => (a -> [a]) -> [a] -> [[a]]
    -- help g xs = go xs
    --   where
    --     go []     = []
    --     go (y:ys) = (before ++ processed) : go ys
    --       where
    --         (before, after) = span (/= y) xs
    --         processed = maybe [] (uncurry ((++) . g)) (uncons after)
    help g xs = go [] xs
      where
        go _   []     = []
        go acc (a:as) = (acc ++ g a ++ as) : go (a:acc) as

    contractPairs :: (Int, Int) -> [(Int, Int)]
    contractPairs (f, c) = if c > 1 then (f, c-1) : contracted else contracted
      where contracted = [(p*f, 1)]

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
contractions = nubOrd . sort . inner
  where
    inner :: Expr -> [Expr]
    inner x@(l, z, _)
      | l == 1    = [x]
      | otherwise = concatMap (contractPrime q) (contractExpr d)
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

possiblyMinimal :: [Expr] -> [Expr]
possiblyMinimal = possiblyMinimal' 0 . sort
  where
    possiblyMinimal' _ []     = []
    possiblyMinimal' c (x:xs) = if c /= len x
                                then x : possiblyMinimal' (len x) xs
                                else possiblyMinimal' c xs

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
missingExpr n = ([1..n] `deleteOrd` backfilled) `deleteOrd` longest
  where
    backfilled = (map len . backfillStrong . strongExpr) n
    longest = map len $ filteredExpressions n

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

main :: IO ()
main = print $ sumShortestExpression 12000
