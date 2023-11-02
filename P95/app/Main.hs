{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Euler.Helper

import Data.Numbers.Primes

import Data.List (maximumBy)

import Data.MemoTrie

import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU

import Data.Functor

import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST


primeFactorsAList :: Int -> [(Int, Int)]
primeFactorsAList n = toAssocList $ primeFactors n

sumDivisorsPP :: Int -> Int -> Int
-- Returns the sum of divisors of prime powers
sumDivisorsPP = memo2 slow_sumDivisorsPP
  where slow_sumDivisorsPP p k = (p^(k+1) - 1) `quot` (p-1)

sumDivisors :: Int -> Int
-- We note that sum of divisors is a multiplicative function, i.e.
-- sumDivisors (i*j) == sumDivisors i * sumDivisors j if i, j coprime.
-- Therefore, factorise n into prime powers,
-- then use the identity to compute sumDivisors n.
sumDivisors = product . map (uncurry sumDivisorsPP) . primeFactorsAList

amicableStep :: Int -> Int
amicableStep = memo slow_sumProperDivisors
  where
    slow_sumProperDivisors n = sumDivisors n - n

amicableChain :: Int -> [Int]
amicableChain = takeWhile (/= 0) . tail . iterate amicableStep


bound :: Int
bound = globalBound

globalBound :: Int
globalBound = 10^(6 :: Int)

testBound :: Int
testBound = 10^(5 :: Int)


type StateMT s ms = ReaderT ms (ST s)
-- A mutable state monad in ST carrying the mutable reference via Reader

safeRead :: (U.Unbox a) => MU.MVector s a -> Int -> ST s (Maybe a)
safeRead mv i = if i < MU.length mv
                then Just <$> MU.read mv i
                else return Nothing

-- We use Int to encode the following.
-- l >=  1, l = length of cycle
-- l ==  0, length of cycle waiting to be determined
-- l == -1, chain ends on 1 and there is no cycle
-- l == -2, chain enters a cycle but not immediately thus ineligible
-- -3 is currently inconsistenly used
-- l == -3, cycle/chain has element larger than 10^6 thus ineligible
-- otherwise l is undefined

amicableChainLength :: Int -> StateMT s (MU.MVector s Int) (Int, Int)
amicableChainLength n = do
    mv <- ask
    let next = amicableStep n
    result <- if next < MU.length mv
              then do
                cache <- MU.read mv next
                return $ case compare cache 0 of
                           GT -> if n `elem` take cache (amicableChainFinite n)
                                 then cache
                                 else (-2)
                           EQ -> let chain = amicableChainFinite n
                                  in if | n `elem` chain -> length chain
                                        | 1 `elem` chain -> (-1)
                                        | otherwise -> (-2)
                           LT -> cache
              else return (-3)
    MU.write mv n result
    return (result, n)

amicableChainFinite :: Int -> [Int]
amicableChainFinite n = postprocess $ foldM go [] (amicableChain n)
  where
    go :: [Int] -> Int -> Either [Int] [Int]
    go as x
      | x >= bound = Left []
      | n == x = Left (x:as)
      | x `notElem` as = Right (x:as)
      | otherwise = Left []

    postprocess :: Either [Int] [Int] -> [Int]
    postprocess (Right _) = []
    postprocess (Left as) = as


initVector :: ST s (MU.MVector s Int)
initVector = do
    v <- MU.replicate (bound + 1) 0
    MU.write v 0 (-1)
    MU.write v 1 (-1)
    return v

input :: [Int]
input = deleteOrd [2..globalBound] primes

order :: (Int, Int) -> (Int, Int) -> Ordering
order (lx, x) (ly, y) = case compare lx ly of
  GT -> GT
  LT -> LT
  EQ -> case compare x y of
    GT -> LT
    LT -> GT
    EQ -> EQ


main :: IO ()
main = print $ runST $
    initVector
    >>= runReaderT (mapM amicableChainLength input)
    <&> maximumBy order



