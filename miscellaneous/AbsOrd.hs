{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Ord (comparing)
import GHC.Enum (predError)


newtype AbsOrd a = AbsOrd { unAbsOrd :: a }
    deriving (Show, Eq, Num, Real, Integral)
-- A wrapper around numbers with the absolute value ordering.
-- If |a| < |b| then a < b.
-- Otherwise compare a and b with the normal ordering.

whenEQ :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
-- Combines two comparision functions.
-- When the first comparision function returns EQ,
-- use the second comparision function to tiebreak.
whenEQ fstCmp sndCmp a b =
    case fstCmp a b of
      EQ -> sndCmp a b
      x  -> x
{-# INLINE whenEQ #-}

instance (Eq a, Ord a, Num a) => Ord (AbsOrd a) where
  compare = comparing (abs . unAbsOrd) `whenEQ` comparing unAbsOrd

instance (Integral a) => Enum (AbsOrd a) where
  -- toEnum :: (Integral a) => Int -> AbsOrd a
  -- a is expected to be Int or Integer.
  -- n `quotRem` 2 returns (Int, Int)
  -- Therefore, type conversion using fromIntegral is needed.
  -- (fromIntegral n) `quotRem` 2 also works,
  -- but then quotRem will be operating on Integer instead of Int.
  toEnum z = if r == 0 then AbsOrd (fromIntegral q) else AbsOrd (fromIntegral (-q-1))
    where
      (q, r) = z `quotRem` 2

  fromEnum a = if unAbsOrd a >= 0 then fromIntegral (2*z) else fromIntegral (-2*z - 1)
    where
      z = unAbsOrd a

  succ a = if z >= 0 then AbsOrd (-(succ z)) else AbsOrd (-z)
    where
      z = unAbsOrd a

  pred (AbsOrd 0) = predError "AbsOrd a"
  pred a = if z >= 0 then AbsOrd (-z) else AbsOrd (-z-1)
    where
      z = unAbsOrd a

absBelow :: Int -> [Int]
absBelow n = map unAbsOrd [AbsOrd 0 .. AbsOrd n]

instance Bounded (AbsOrd Int) where
  minBound = AbsOrd 0
  maxBound = AbsOrd maxBound

