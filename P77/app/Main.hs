module Main where

import Euler.Helper

import Data.Numbers.Primes

import Data.List (foldl')

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Monad

-- See notes.txt

initialVector :: Int -> V.Vector Int
initialVector n = V.generate (n+1) (\k -> if even k then 1 else 0)

step :: V.Vector Int -> Int -> V.Vector Int
step v p = V.create (
  do nv <- V.thaw v
     let len = V.length v
     forM_ [p..(len-1)] (\t -> do
         x <- MV.read nv (t-p)
         MV.modify nv (+x) t)
     return nv )

count :: Int -> Int
-- initialVector generates the base case for the first prime = 2
-- Therefore drop the first prime from the list of primes.
count n = foldl' step (initialVector n) (takeWhile (<n) (tail primes)) V.! n

nSteps :: Int -> V.Vector Int
nSteps n = foldl' step (initialVector n) (takeWhile (<n) (tail primes))

answer :: Int
answer = binarySearchF 5000 count

main :: IO ()
main = print answer
