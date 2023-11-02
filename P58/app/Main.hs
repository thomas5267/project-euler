module Main where

import Data.Numbers.Primes

import Control.Monad (foldM)
import Data.Either (fromLeft)

-- Bottom right diagonal is the squares of odd numbers
-- br n = (2n+1)^2, n <- [0..]
br :: Integer -> Integer
br n = (2*n+1)^2

-- Top right:
-- tr = [1, 3, 13, 31..]
-- c = 1
-- a + b = 2
-- 4a + 2b = 12
-- 2a = 8
-- a = 4
-- b = -2
tr :: Integer -> Integer
tr n = 4*n^2 - 2*n + 1

-- Top left:
-- tl = [1, 5, 17, 37..]
-- c = 1
-- a + b = 4
-- 4a + 2b = 16
-- 2a = 8
-- a = 4
-- b = 0
tl :: Integer -> Integer
tl n = 4*n^2 + 1

-- Bottom Left
-- bl = [1, 7, 21, 43..]
-- c = 1
-- a + b = 6
-- 4a + 2b = 20
-- 2a = 8
-- a = 4
-- b = 2
bl :: Integer -> Integer
bl n = 4*n^2 + 2*n + 1

-- The number of numbers on a diagonal of square of "radius" n
nd :: Integer -> Integer
nd n = 4*n + 1

input :: [[Integer]]
input = map (\ n -> [tr n, tl n, bl n]) [2..]

-- answer :: (Int, Int)
-- answer = go (5, 3) input
--   where
--     go :: (Int, Int) -> [[Integer]] -> (Int, Int)
--     go (n, c) [] = (n, c)
--     go (n, c) (x:xs)
--       | fI c / fI n <= 0.1 = (n, c)
--       | otherwise          = go (n+4, c + countPrime x) xs
--       where
--         countPrime = length . filter isPrime
--         fI = fromIntegral

answer :: (Int, Int)
answer = fromLeft (errorWithoutStackTrace "answer: finite list") $
             foldM go (5, 3) input
  where
    go :: (Int, Int) -> [Integer] -> Either (Int, Int) (Int, Int)
    go (n, c) [] = Right (n, c)
    go (n, c) xs
      | fI c / fI n <= 0.1 = Left (n, c)
      | otherwise          = Right (n+4, c + countPrime xs)
      where
        countPrime = length . filter isPrime
        fI = fromIntegral

process :: (Int, Int) -> Int
process (n, _) = (n-1) `quot` 2 + 1

main:: IO ()
main = print answer >> print (process answer)
