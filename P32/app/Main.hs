module Main where
import Data.Char (digitToInt)
import qualified Data.IntSet as IS
import Data.List (delete, foldl', nub, (\\))

import qualified Euler.DigitSet as DS
import Control.Applicative
import Control.Monad

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

-- We have to consider two cases

-- First case:
-- The largest two digit number is 99,
-- 99 * 99 = 9801, a four digit number.
--
-- The smallest three digit number is 100,
-- 100 * 100 = 10000, a five digit number.
--
-- In both cases the identity does not have the right number of digits, namely 9.
-- Hence we only need to consider the products of two digit numbers
-- with three digit numbers.

-- Second case:
-- A one digit number times a four digit number.

twoDigits :: [Int]
twoDigits = filter good [10..100]
  where good n = (n `rem` 11 /= 0) && (n `rem` 10 /= 0)

digits :: Int -> [Int]
digits n = map digitToInt (show n)

digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\k d -> 10*k + d) 0

remDigits :: Int -> (Int, [Int])
remDigits = toSnd (\n -> [1..9] \\ digits n)

choose :: Int -> [Int] -> [[Int]]
-- Return all possible choices of choosing k elements without replacement from l.
choose 0 _ = [[]]
choose k l = do
    x <- l
    map (x:) (choose (k-1) (delete x l))

products :: [(Int, [Int])] -> [(Int, [(Int, Int)])]
products l = map helper l
  where helper (n, l) = (n, map (toSnd (n*)) l)

candidates1 :: [(Int,[Int])]
candidates1 = map (\(a, l) -> (a, filter (<= 9999 `quot` a) l)) unfiltered
-- We only want numbers such that the product with a is a four digit numbers
  where unfiltered = map (mapSnd (map digitsToInt . choose 3) . remDigits) twoDigits

resultGen :: [(Int, [Int])] -> [(Int, Int, Int)]
resultGen l = do
    (a, bs) <- products l
    let da = DS.fromList (digits a)
    do
        (b, c) <- bs
        let lc = digits c
        guard (nub lc == lc)
        let db = DS.fromList (digits b)
            dc = DS.fromList (digits c)
        guard ((da `DS.disjoint` dc) && (db `DS.disjoint` dc) && not (0 `DS.elem` dc))
        return (a, b, c)

result1 :: [Int]
result1 = nub $ map (\(_, _, c) -> c) (resultGen candidates1)

fourDigits :: [Int]
fourDigits = map digitsToInt (choose 4 [1..9])

candidates2 :: [(Int, [Int])]
candidates2 = map (\(a, l) -> (a, filter (<= 9999 `quot` a) l)) unfiltered
  where unfiltered = map (mapSnd (map digitsToInt . choose 1) . remDigits) fourDigits

result2 :: [Int]
result2 = nub $ map (\(_, _, c) -> c) (resultGen candidates2)



main :: IO ()
main = print $ sum (nub (result1 ++ result2))
