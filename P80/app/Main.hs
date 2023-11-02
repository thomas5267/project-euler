{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.Decimal
import GHC.TypeNats (KnownNat)

import Data.Char (digitToInt)
import Data.List (delete)

import Control.Monad

type E100 = Decimal RoundHalfEven 101 Integer

divR
    :: (MonadThrow m, KnownNat s, Round r Integer)
    => Decimal r s Integer
    -> Decimal r s Integer
    -> m (Decimal r s Integer)
divR = divideDecimalWithRounding

sqrtApprox :: Integer -> E100 -> Arith E100
sqrtApprox n a = bindM2 divR (pure a + fromIntegral n `divR` a) 2

sqrtApprox20 :: Integer -> E100 -> Arith E100
-- This method has quadratic convergence.
-- 20 iterations should be more than enough.
sqrtApprox20 n = foldr1 (>=>) (replicate 20 (sqrtApprox n))

digitSum :: Arith E100 -> Int
-- show (x :: Arith E100) returns "Arith xxxxxxx".
-- drop 6 removes the "Arith " part.
digitSum = sum . map digitToInt . take 100 . delete '.' . drop 6 . show

input :: [(Integer, E100)]
input = go 1 [2..100] squares
  where
    squares :: [(Integer, Integer)]
    squares = [ (x^2, x) | x <- [2..] ]

    go :: Integer -> [Integer] -> [(Integer, Integer)] -> [(Integer, E100)]
    go _ [] _  = []
    go c as [] = map (\a -> (a, fromIntegral c)) as
    go c al@(a:as) bl@((sqb, b):bs) =
        case a `compare` sqb of
          LT -> (a, fromIntegral c) : go c as bl
          EQ -> go b as bs
          GT -> go b al bs

main :: IO ()
main = print $ sum $ map (digitSum . uncurry sqrtApprox20) input
