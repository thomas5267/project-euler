{-# LANGUAGE ScopedTypeVariables #-}

module Euler.Helper where

import Data.Int

import Data.List (foldl', sort)
import qualified Data.Vector.Unboxed as V

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

-- data Loop a = Cont a
--             | Exit a
--             deriving (Show, Eq, Ord)
--
-- instance Functor Loop where
--   fmap f (Cont a) = Cont (f a)
--   fmap f (Exit a) = Exit (f a)
--
-- instance Applicative Loop where
--   pure a = Cont a
--
-- -- Not sure about this
--   Cont f <*> Cont a = Cont (f a)
--   Cont f <*> Exit a = Exit (f a)
--   Exit f <*> Cont a = Exit (f a)
--   Exit f <*> Exit a = Exit (f a)
--
-- instance Monad Loop where
--   Cont a >>= k = k a
--   Exit a >>= k = k a

whenEQ :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
-- Combines two comparision functions.
-- When the first comparision function returns EQ,
-- use the second comparision function to tiebreak.
whenEQ fstCmp sndCmp a b =
  case fstCmp a b of
    EQ -> sndCmp a b
    x -> x
infixr 9 `whenEQ`
{-# INLINE whenEQ #-}

digits :: forall a b. (Integral a, Integral b) => a -> [b]
-- Returns the digits of an integer in a list.
digits = go []
  where
    go cs k
      | k < 10 = fromIntegral k : cs
      | otherwise = go (fromIntegral r : cs) q
      where
        (q, r) = k `quotRem` 10
{-# INLINEABLE digits #-}
{-# SPECIALIZE digits :: Int -> [Int8] #-}
{-# SPECIALIZE digits :: Int -> [Int] #-}
{-# SPECIALIZE digits :: Integer -> [Int8] #-}
{-# SPECIALIZE digits :: Integer -> [Int] #-}

reverseDigits :: (Integral a, Integral b) => a -> [b]
-- Returns the digits of an integer in a list, least significant digit first.
reverseDigits n
  | n < 10 = [fromIntegral n]
  | otherwise = fromIntegral r : reverseDigits q
  where
    (q, r) = n `quotRem` 10
{-# INLINEABLE reverseDigits #-}
{-# SPECIALIZE reverseDigits :: Int -> [Int8] #-}
{-# SPECIALIZE reverseDigits :: Int -> [Int] #-}
{-# SPECIALIZE reverseDigits :: Integer -> [Int8] #-}
{-# SPECIALIZE reverseDigits :: Integer -> [Int] #-}

fromDigits :: (Integral a, Integral b) => [a] -> b
fromDigits = foldl' (\k d -> 10 * k + fromIntegral d) 0
{-# INLINEABLE fromDigits #-}
{-# SPECIALIZE fromDigits :: [Int8] -> Int #-}
{-# SPECIALIZE fromDigits :: [Int] -> Int #-}
{-# SPECIALIZE fromDigits :: [Int8] -> Integer #-}
{-# SPECIALIZE fromDigits :: [Int] -> Integer #-}

{-# DEPRECATED digitsToInt "Use fromDigits instead" #-}
digitsToInt :: [Int] -> Integer
digitsToInt = fromDigits
{-# INLINEABLE digitsToInt #-}

compareLength :: [a] -> [b] -> Ordering
compareLength [] [] = EQ
compareLength _ [] = GT
compareLength [] _ = LT
compareLength (_ : as) (_ : bs) = compareLength as bs
{-# INLINEABLE compareLength #-}

compareDigits :: forall a. Integral a => a -> a -> Ordering
compareDigits a b = compareLength (reverseDigits a :: [a]) (reverseDigits b :: [a])
{-# INLINEABLE compareDigits #-}
{-# SPECIALIZE compareDigits :: Int -> Int -> Ordering #-}
{-# SPECIALIZE compareDigits :: Integer -> Integer -> Ordering #-}

fromAssocList :: [(a, Int)] -> [a]
fromAssocList = foldr go []
  where
    go (a, c) acc = replicate c a ++ acc
{-# INLINEABLE fromAssocList #-}

toAssocList :: (Ord a) => [a] -> [(a, Int)]
-- The following code is also fine:
-- toAssocList' = maybe [] (uncurry (go 1)) . uncons . sort
-- However, it is not as lazy as the following version.
-- null $ toAssocList (repeat 1) == False,
-- but null $ toAssocList' (repeat 1) == _|_
toAssocList as =
  case as of
    [] -> []
    -- I cannot prove to the compiler that sort and go transform
    -- a non-empty list to another non-empty list.
    xs ->
      let y : ys = sort xs
          z : zs = go 1 y ys
       in z : zs
      where
        -- c is count of e
        -- e is the currently processing element
        go c e [] = [(e, c)]
        go c e (y : ys) =
          if e == y
            then go (c + 1) e ys
            else (e, c) : go 1 y ys
{-# INLINEABLE toAssocList #-}

nubOrd :: Ord a => [a] -> [a]
-- nub for when the list is ordered.
nubOrd = nubByOrd (==)
{-# INLINEABLE nubOrd #-}

nubByOrd :: Ord a => (a -> a -> Bool) -> [a] -> [a]
-- nubBy for when the list is ordered.
-- Takes the leftmost element in the case of repetition.
nubByOrd _ [] = []
nubByOrd _ [x] = [x]
nubByOrd p (x1 : xs) = x1 : nubByOrd p (dropWhile (p x1) xs)
{-# INLINEABLE nubByOrd #-}

deleteOrd :: Ord a => [a] -> [a] -> [a]
-- delete for when the lists are sorted.
deleteOrd [] _ = []
deleteOrd xl [] = xl
deleteOrd xl@(x : xs) yl@(y : ys) =
  case x `compare` y of
    LT -> x : deleteOrd xs yl
    EQ -> deleteOrd xs yl
    GT -> deleteOrd xl ys
{-# INLINEABLE deleteOrd #-}

deleteOrdR :: Ord a => [a] -> [a] -> [a]
-- delete for when the list is sorted in reverse order.
deleteOrdR [] _ = []
deleteOrdR xl [] = xl
deleteOrdR xl@(x : xs) yl@(y : ys) =
  case x `compare` y of
    GT -> x : deleteOrdR xs yl
    EQ -> deleteOrdR xs yl
    LT -> deleteOrdR xl ys
{-# INLINEABLE deleteOrdR #-}

elemOrd :: (Ord a) => a -> [a] -> Bool
-- Linear search on sorted list.
elemOrd _ [] = False
elemOrd k (x : xs) = case k `compare` x of
  LT -> elemOrd k xs
  EQ -> True
  GT -> False
{-# INLINEABLE elemOrd #-}

elemOrdF :: forall a b. (Integral a, Ord b) => b -> (a -> b) -> Bool
-- Binary search on monotonic functions
elemOrdF k f = uncurry elemOrdF' bound
  where
    bound :: (a, a)
    bound = (0, genUpperBound 100)
      where
        genUpperBound b = if k <= f b then b else genUpperBound ((b + 1) * 2)
    elemOrdF' l r
      | l == r = f m == k
      | otherwise = case k `compare` y of
          LT -> elemOrdF' l (m - 1)
          _ -> elemOrdF' m r
      where
        (s, t) = (l + r) `quotRem` 2
        m = s + t
        y = f m
{-# INLINEABLE elemOrdF #-}

elemOrdV :: (Ord a, V.Unbox a) => a -> V.Vector a -> Bool
-- Binary search on vectors
elemOrdV k v
  | V.null v = False
  | otherwise = elemOrdV' 0 (V.length v - 1)
  where
    elemOrdV' l r
      | l == r = v V.! m == k
      | otherwise = case k `compare` y of
          LT -> elemOrdV' l (m - 1)
          _ -> elemOrdV' m r
      where
        (s, t) = (l + r) `quotRem` 2
        m = s + t
        y = v `V.unsafeIndex` m
{-# INLINEABLE elemOrdV #-}

binarySearchF :: forall a b. (Integral a, Ord b) => b -> (a -> b) -> a
-- Binary search on monotonic functions
-- Returning the largest a such that f a <= b
binarySearchF k f = uncurry binarySearchF' bound
  where
    bound :: (a, a)
    bound = (0, genUpperBound 100)
      where
        genUpperBound b = if k <= f b then b else genUpperBound ((b + 1) * 2)
    binarySearchF' l r
      | l == r = m
      | otherwise = case k `compare` y of
          LT -> binarySearchF' l (m - 1)
          _ -> binarySearchF' m r
      where
        (s, t) = (l + r) `quotRem` 2
        m = s + t
        y = f m
{-# INLINEABLE binarySearchF #-}
