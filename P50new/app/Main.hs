module Main (main) where

import qualified Data.IntSet as IS

import Control.Monad.State.Strict
import Data.List (uncons, unfoldr)
import Data.Maybe (fromJust)
import Data.Numbers.Primes hiding (isPrime)

limit :: Int
limit = 1000000

-- search :: Int -> [Int]
-- search n = search' revPyramid [] False
--   where
--     primeL = takeWhile (<= n) primes
--
--     primesSet = IS.fromDistinctAscList primeL
--     isPrime = (`IS.member` primesSet)
--
--     revPyramid = partialSumGen n primeL
--
--     search' :: [[Int]] -> [Int] -> Bool -> [Int]
--     search' [] best _ = best
--     search' (x : xs) best b
--       | b = evenSpecial
--       | null result = search' xs best (not b)
--       | otherwise = search' xs result (not b)
--       where
--         evenSpecial =
--           if isPrime (head x)
--             then search' xs [head x] (not b)
--             else search' xs best (not b)
--         result = filter isPrime x

-- search :: Int -> Int
-- search n = foldl go 0 revPyramid
--   where
--     primeL = takeWhile (<= n) primes
--
--     primesSet = IS.fromDistinctAscList primeL
--     isPrime = (`IS.member` primesSet)
--
--     revPyramid = partialSumGen' n primeL
--
--     go :: Int -> [Int] -> Int
--     go acc [] = acc
--     go acc (x : xs)
--       | x `mod` 2 == 1 = if isPrime x then x else acc
--       | not (any isPrime xs) = acc
--       | otherwise = maximum xs

search :: Int -> Int
search n = evalState (foldM search' 0 revPyramid) True
  where
    primeL = takeWhile (<= n) primes

    primesSet = IS.fromDistinctAscList primeL
    isPrime = (`IS.member` primesSet)

    revPyramid = partialSumGen' n primeL

    search' :: Int -> [Int] -> State Bool Int
    search' acc [] = pure acc
    search' acc xl@(x : _) = do
      isHeadEven <- get
      modify not
      pure $ if isHeadEven then evenBranch else oddBranch
        where
          evenBranch = if isPrime x then x else acc
          oddBranch = maximum xl

-- reversePyramidGen :: Int -> [Int] -> [[Int]]
-- -- Generates a reverse pyramid of consecutive sums
-- -- with base at the head of the list.
-- -- Only works with sorted lists but the precondition is not checked.
-- reversePyramidGen n l = nextLevel l l
--   where
--     nextLevel :: [Int] -> [Int] -> [[Int]]
--     nextLevel [] _ = []
--     nextLevel _ [] = []
--     nextLevel acc (_ : bs) = acc : nextLevel result bs
--       where
--         result = filter (<= n) (zipWith (+) acc bs)

-- partialSumGen :: Int -> [Int] -> [[Int]]
-- -- Generates a reverse pyramid of consecutive sums
-- -- with base at the head of the list.
-- -- Only works with sorted lists but the precondition is not checked.
-- partialSumGen n l = unfoldr go tails
--   where
--     l' = takeWhile (<= n) l
--     tails = scanr (:) [] l'
--
--     go :: [[Int]] -> Maybe ([Int], [[Int]])
--     go (acc : ls : lss)
--       | null acc' = Just (acc, [])
--       | otherwise = Just (acc, acc' : lss)
--       where
--         -- This line fails if the list is not sorted
--         acc' = takeWhile (<= n) (zipWith (+) acc ls)
--     go [acc] = Just (acc, [])
--     go [] = Nothing

partialSumGen :: Int -> [Int] -> [[Int]]
-- Generates a reverse pyramid of consecutive sums
-- with base at the head of the list.
-- Only works with sorted lists but the precondition is not checked.
partialSumGen n l = unfoldr go (fromJust $ uncons tails)
  where
    l' = takeWhile (<= n) l
    tails = scanr (:) [] l'

    go :: ([Int], [[Int]]) -> Maybe ([Int], ([Int], [[Int]]))
    go ([], _) = Nothing
    go (acc, []) = Just (acc, ([], []))
    go (acc, x : xs) = Just (acc, (acc', xs))
      where
        -- This line fails if the list is not sorted
        acc' = takeWhile (<= n) (zipWith (+) x acc)

partialSumGen' :: Int -> [Int] -> [[Int]]
partialSumGen' n = takeWhile (not . null) . map (takeWhile (<= n)) . sumGen

sumGen :: [Int] -> [[Int]]
sumGen l = scanl1 (zipWith (+)) tails
  where
    tails = scanr (:) [] l

main :: IO ()
main = print $ search limit -- == 997651
-- main = print $ search 500000 -- == 499607
