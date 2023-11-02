module Euler.DigitSet where

import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.Bit.ThreadSafe
import Euler.Helper (digits)

newtype DigitSet = DigitSet { toBitVec :: V.Vector Bit } deriving (Show, Eq)

empty :: DigitSet
empty = DigitSet $ V.replicate 10 (Bit False)

fromList :: [Int] -> DigitSet
fromList l =
    DigitSet $ V.replicate 10 (Bit False) V.// map (\x -> (x, Bit True)) l

fromInt :: (Integral a) => a -> DigitSet
fromInt = fromList . digits

toList :: DigitSet -> [Int]
toList s = listBits (toBitVec s)

disjoint :: DigitSet -> DigitSet -> Bool
disjoint a b = not $ Bit True `V.elem` zipBits (.&.) va vb
  where
    va = toBitVec a
    vb = toBitVec b

union :: DigitSet -> DigitSet -> DigitSet
union (DigitSet a) (DigitSet b) = DigitSet $ V.zipWith (.|.) a b

elem :: Int -> DigitSet -> Bool
elem k s = toBitVec s V.! k == Bit True
{-# INLINE elem #-}
