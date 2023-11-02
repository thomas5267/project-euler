module Main where

import Data.Numbers.Primes (primes)

twoSquares :: [Int]
twoSquares = map (\x -> 2*x^(2 :: Int)) [1..]

isGoldbach :: Int -> Bool
isGoldbach n = n `elem` sums
  where
    sums = (+) <$> takeWhile (< n) twoSquares <*> takeWhile (< n) primes

oddComposites :: [Int]
oddComposites = helper [3,5..] primes
  where
    helper nl [] = nl
    helper [] pl = pl
    helper nl@(n:ns) pl@(p:ps) = case n `compare` p of
        LT -> n : helper ns pl
        EQ -> helper ns ps
        GT -> helper nl ps


main :: IO ()
main = print $ head $ filter (not . isGoldbach) oddComposites
