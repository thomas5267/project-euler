module Main where

import Data.Numbers.Primes
import Euler.Helper hiding (nubOrd)

nubOrd :: Ord a => [a] -> [a]
-- nub when the list is ordered.
nubOrd [] = []
nubOrd [x] = [x]
nubOrd (x1:xs@(x2:_))
  | x1 == x2 = nubOrd xs
  | otherwise = x1 : nubOrd xs

candidates :: [(Int, Int)]
candidates = map (toSnd (length . nubOrd . primeFactors)) [2..]

search :: (a -> Bool) -> Int -> [a] -> [a]
search f n = search' 0 []
  where
    search' _ _   [] = []
    search' c acc (x:xs)
      | c == n    = reverse acc
      | f x       = search' (c+1) (x:acc) xs
      | otherwise = search' 0 [] xs

main :: IO ()
main = print $ search ((==4) . snd) 4 candidates
