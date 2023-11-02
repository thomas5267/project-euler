{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Main where

import Euler.Helper

import Data.Numbers.Primes

import Data.MemoTrie

import Data.List (group, sort, uncons)
import Data.Function (on)
import Data.Foldable
import Data.Maybe (fromJust)

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
    go xl  = go' x xs
      where
        (x, xs) = fromJust $ uncons xl
        go' a [] = [ a | snd a /= 0 ]
        go' a (b:bs)
          | fst a == fst b = go' (addPairs a b) bs
          | otherwise      = if snd a /= 0 then a : go' b bs else go' b bs
          where addPairs (z, i) (_, j) = (z, i+j)

instance Functor OccList where
  fmap f (OccList l) = OccList $ map (mapFst f) l

instance Foldable OccList where
  foldr f acc (OccList xs) = foldr go acc xs
    where
      go (a, l) nacc = foldr f nacc (replicate l a)

size :: OccList a -> Int
size = foldl' go 0 . unwrapOccList
  where
    go acc (_, c) = acc + c

valuation :: OccList Int -> Int
valuation = foldl' go 1 . unwrapOccList
  where
    go acc (a, c) = acc*a^c

instance Applicative OccList where
  pure a = OccList [(a, 1)]
  liftA2 f (OccList as) (OccList bs) =
      OccList $ [ (f a b, nl) | (a, al) <- as,
                                  (b, bl) <- bs,
                                  let nl = al*bl ]

instance Monad OccList where
  as >>= f =
      OccList $ [ x | (ol, l) <- unwrapOccList (fmap f as),
                          x <- map (mapSnd (*l)) (unwrapOccList ol) ]

instance Semigroup (OccList a) where
  (OccList a) <> (OccList b) = OccList (a <> b)

instance Monoid (OccList a) where
  mempty = OccList []

-- filterOL :: (a -> Bool) -> OccList a -> OccList a
-- filterOL p (OccList l) = OccList $ filterOL' l
--   where
--     filterOL' [] = []
--     filterOL' (x@(a, _):xs) = if p a
--                               then x : filterOL' xs
--                               else filterOL' xs

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
    expr' n = (l, n, factors)
      where
        factors = fastFactor n
        l = longestExpression factors

longestExpression :: OccList Int -> Int
-- Returns the longest expression of the product of the list of primes.
-- Useful in contractions to prevent unnecessary prime factorisation.
longestExpression (OccList factors) = process $ foldl' go (1, 0, 0) factors
  where
    go (p, s, l) (f, c) = (p*f^c, s+f*c, l+c)
    process (p, s, l) = p - s + l


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


powerset :: (Ord a) => [a] -> [[a]]
powerset [] = [[]]
powerset l = (map fromOccList . powerOccList . toOccList) l

powerOccList :: (Ord a) => OccList a -> [OccList a]
powerOccList (OccList l) = map wrapOccList $ foldr go [[]] l
  where
    go :: (a, Int) -> [[(a, Int)]] -> [[(a, Int)]]
    go (a, k) xs = map (\t -> ((a, t) :)) [0..k] <*> xs

(#) :: (a, b) -> ([a], [b]) -> ([a], [b])
(#) (a, b) (as, bs) = (a:as, b:bs)

powerOccListComple :: (Ord a) => OccList a -> [(OccList a, OccList a)]
powerOccListComple (OccList l) = map (both wrapOccList) $ powerOccListComple' l
  where
    powerOccListComple' [] = [([],[])]
    powerOccListComple' (x:xs) = liftA2 (#) (varyPairs x) (powerOccListComple' xs)
      where varyPairs (a, c) = map (\k -> ((a, k), (a, c-k))) [0..c]

powerlistComple :: (Ord a) => [a] -> [([a], [a])]
-- Returns the powerlist with complements of l, but considers
-- identical sets identical with no consideration of their provinence.
-- E.g. powersetComple [2,2] = [([], [2,2]), ([2], [2]), ([2, 2], [])],
-- not [([], [2,2]), ([2], [2]), ([2], [2]), ([2, 2], [])]
-- Powerlist is like a powerset, but on list instead of set.
powerlistComple = map (both fromOccList) . powerOccListComple . toOccList


divisors :: Int -> [(Int, Int)]
divisors n = go [2..floor (sqrt @Float (fromIntegral n))]
  where
    go [] = []
    go (d:ds) = if r == 0
                then (d, q) : (q, d) : go ds
                else go ds
      where (q, r) = n `quotRem` d

contractions :: Int -> Expr -> [Expr]
-- Returns all contractions of <d> with length above bound.
-- Used for strong numbers mostly.
contractions bound di@(_, z, _) =
    map postprocess . nubByOrd ((==) `on` fst) . sort . concat $ iterateNonEmpty go [preprocess di]
    -- map postprocess . sort . concat $ iterateNonEmpty go [preprocess di]
  where
    iterateNonEmpty :: ([a] -> [a]) -> [a] -> [[a]]
    iterateNonEmpty f l = if null l
                          then []
                          else l : iterateNonEmpty f (f l)

    preprocess d = (fst3 d, thd3 d)
    postprocess s = (fst s, z, snd s)

    go :: [(Int, OccList Int)] -> [(Int, OccList Int)]
    go ds = concatMap unnamed ds

    chosen = fst
    rejected = snd
    -- fc is the occlist of factors chosen
    -- fr is the occlist of factors rejected
    -- pf is the pair of factors, i.e. (fc, fr)

    -- Contractions preserve valuation
    -- we do not need to pass the valuation around.
    unnamed :: (Int, OccList Int) -> [(Int, OccList Int)]
    unnamed (l, f) =
        [ (nl, nf) | pf <- powerOccListComple f,
                     chosenSizeGood pf,
                     let nl = l - longestExpression (fst pf) + 1,
                     nl > bound,
                     let nf = wrapOccList $ (products (chosen pf), 1) : unwrapOccList (rejected pf) ]
      where
        chosenSizeGood y = size (chosen y) >= 2
        products :: OccList Int -> Int
        products = foldl' go2 1 . unwrapOccList
          where go2 acc (a, c) = acc*a^c


backfillStrong :: [Expr] -> [Expr]
backfillStrong l = concat $ zipWith (contractions . len) l (tail l)


-- biggestGap :: [Expr] -> (Int, Int)
-- biggestGap = biggestGap' (0, 0)
--   where
--     biggestGap' m []  = m
--     biggestGap' m [_] = m
--     biggestGap' m (x1:xs@(x2:_)) = if snd m >= len x2 - len x1
--                                    then biggestGap' m xs
--                                    else biggestGap' (len x1, len x2 - len x1) xs
--
-- deficiency :: Int -> Int
-- -- Deficiency is the number of 1's in the longest expression of n.
-- deficiency n = n - sum (fastFactor n)
--
expressions :: Int -> [Expr]
expressions bound = takeWhile ((<= bound) . len) $ map expr composites

filteredExpressions :: Int -> [Expr]
filteredExpressions = nubByOrd ((==) `on` fst3) . sort . expressions

strongTestExpr :: [Expr]
strongTestExpr = strong $ expressions 100

strongExpr :: Int -> [Expr]
strongExpr = strong . expressions

-- missingExpr :: Int -> [Int]
-- missingExpr bound = go [1..bound] backfilled
--   where
--     backfilled = backfillStrong $ strong $ takeWhile ((<= bound) . len) $ map expr composites
--     go [] _  = []
--     go al [] = al
--     go al@(a:as) bl@(b:bs) =
--         case a `compare` len b of
--           LT -> a : go as bl
--           EQ -> go as bs
--           GT -> go al bs

missingExpr :: Int -> [Int]
missingExpr n = ([1..n] `deleteOrd` backfilled) `deleteOrd` longest
  where
    backfilled = (map len . backfillStrong . strongExpr) n
    longest = map len $ filteredExpressions n


main :: IO ()
main = print $ missingExpr 200
