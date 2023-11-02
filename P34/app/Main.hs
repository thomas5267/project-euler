module Main where

import Euler.Helper

import Data.Vector.Unboxed qualified as V
import Data.List (foldl')

fac :: Int -> Int
fac n = (product . take n) [1..]

bound :: Int
-- digitFactorial 9999999 = fac 9 * 7 = 2540160
bound = fac 9 * 7

digitFactorial :: Int -> Int
digitFactorial = go . digits
  where
    go :: [Int] -> Int
    go = foldl' (\ s d -> s + (smallFac V.! d)) 0
      where
        smallFac = V.generate 10 fac

results :: [Int]
results = filter (\ x -> digitFactorial x == x) [3..bound]

answer :: Int
answer = sum results


main :: IO ()
main = print answer
