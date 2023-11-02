module Main where

import Euler.Helper

import Data.Vector.Unboxed qualified as V
import Data.List (elemIndex, foldl', sort, uncons)

bound :: Int
bound = 10^(6 :: Int)

fac :: Int -> Int
fac n = (product . take n) [1..]

digitFactorial :: Int -> Int
digitFactorial = foldl' (\ acc d -> acc + fac d) 0 . digits

dropEveryOther :: [a] -> [a]
-- Drops every other element of the list.
dropEveryOther []     = []
dropEveryOther (x:xs) = x : dropEveryOther (drop 1 xs)

-- floydCycleFinding :: (Eq a) => [a] -> Maybe Int
-- -- Returns the length of the cycle in the list.
-- floydCycleFinding xs = do
--     let fastXs = dropEveryOther xs
--     multipleCycleLength <- (+1) <$> firstEqual (tail xs) (tail fastXs)
--     cycleStartIndex <- firstEqual xs (drop multipleCycleLength xs)
--     (+1) <$> elemIndex (xs !! cycleStartIndex) (drop (cycleStartIndex + 1) xs)
--   where
--     -- firstEqual find the index i such that (as !! i == bs !! i).
--     -- If the index is found, return Just i.
--     -- Otherwise, return Nothing.
--     firstEqual as bs = elemIndex True $ zipWith (==) as bs

longestPrefix :: (Eq a) => [a] -> Maybe Int
-- Returns the length of the longest prefix of a list
-- which consist of distinct element.
-- Adapted from Floyd's cycle finding algorithm
longestPrefix xs = do
    let fastXs = dropEveryOther xs
    multipleCycleLength <- (+1) <$> firstEqual (tail xs) (tail fastXs)
    cycleStartIndex <- firstEqual xs (drop multipleCycleLength xs)
    cycleLength <- (+1) <$> elemIndex (xs !! cycleStartIndex) (drop (cycleStartIndex + 1) xs)
    return (cycleLength + cycleStartIndex)
  where
    -- firstEqual find the index i such that (as !! i == bs !! i).
    -- If the index is found, return Just i.
    -- Otherwise, return Nothing.
    firstEqual as bs = elemIndex True $ zipWith (==) as bs

results :: [(Maybe Int, Int)]
results = zip (map (longestPrefix . iterate digitFactorial) [1..10^6]) [1..]

answer :: Int
answer = length $ filter ((== Just 60) . fst) results


-- We can do better.
-- The question has already specified that there are only 4 digit factorials cycles.
-- 145 -> 145
-- 871 -> 45361 -> 871
-- 872 -> 45362 -> 872
-- 169 -> 363601 -> 1454 -> 169
--
-- Hence, we can iterate digitFactorial until it encounters one of these numbers,
-- and count how many iterations it takes to reach that number.
--
-- Furthermore, digitFactorial does not depend on the order of digits.
-- So we can create a digitFactorialVec as follows.

toAssocList :: (Ord a) => [a] -> [(a, Int)]
toAssocList = maybe [] (uncurry (go 1)) . uncons . sort
  where
    -- c is count of e
    -- e is the currently processing element
    go c e []     = [(e, c)]
    go c e (x:xs) =
        if e == x
        then go (c+1) e xs
        else (e, c) : go 1 x xs


digitFactorialFast :: Int -> Int
digitFactorialFast = go . digits
  where
    go :: [Int] -> Int
    go = foldl' (\ s d -> s + (smallFac V.! d)) 0
      where
        smallFac = V.generate 10 fac

longestChainFast :: Int -> Int
longestChainFast = go
  where
    go k =
        case k of
          169    -> 3
          363601 -> 3
          1454   -> 3
          871    -> 2
          45361  -> 2
          872    -> 2
          45362  -> 2
          _      -> let nextIter = digitFactorialFast k
                     in if nextIter == k
                        then 1
                        else longestChainFast nextIter + 1

resultsFast :: [Int]
resultsFast = map longestChainFast [1..bound]

answerFast :: Int
answerFast = length $ filter (== 60) resultsFast


main :: IO ()
main = print answerFast
