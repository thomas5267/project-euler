module Main where

import Data.Numbers.Primes
import qualified Data.IntSet as IS

f :: Int -> Int -> Int -> Int
f a b c = a^(2 :: Int) + b^(3 :: Int) + c^(4 :: Int)

bound :: Int
bound = 50*10^(6 :: Int)

results :: IS.IntSet
results = IS.fromList [ f x y z | x <- takeWhile (\a -> a^2 <= bound) primes,
                                  y <- takeWhile (\a -> a^3 <= bound) primes,
                                  z <- takeWhile (\a -> a^4 <= bound) primes,
                                  f x y z <= bound ]

answer :: Int
answer = IS.size results

main :: IO ()
main = print answer
