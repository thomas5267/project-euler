module Main (main) where

import Euler.Helper

import Data.List (find, permutations)
import Data.Maybe

import Data.IntSet qualified as IS

import Control.Monad


intDiv :: Int -> Int -> Maybe Int
intDiv a b = if b /= 0 && r == 0
             then Just q
             else Nothing
  where
    (q, r) = a `quotRem` b

type SearchState = ([Int], [Int])

initSearchState :: [Int] -> [SearchState]
initSearchState xs = [(ps, []) | ps <- permutations xs]

step :: SearchState -> [SearchState]
step searchState@(numbers, stack) = case numbers of
      (n:ns) -> (ns, n:stack) : fourOperators searchState
      []     -> fourOperators searchState
  where
    fourOperators :: SearchState -> [SearchState]
    fourOperators (ns, s1:s2:ss) =
        [ (ns, x:ss) | x <- [s2+s1, s2*s1, s2-s1] ++ maybeToList (intDiv s2 s1)]
    fourOperators _ = []

step7 :: SearchState -> [SearchState]
step7 = foldr1 (>=>) (replicate 7 step)

summarise :: [SearchState] -> Int
summarise = subtract 1
            . fst
            . fromJust
            . find (uncurry (/=))
            . zip [1..]
            . dropWhile (< 1)
            . IS.toAscList
            . IS.fromList
            . map (head . snd)

grand :: [Int] -> (Int, [Int])
grand = toFst (summarise . concatMap step7 . initSearchState)


input :: [[Int]]
input = [ [a, b, c, d] | d <- [4..9],
                         c <- [3..d-1],
                         b <- [2..c-1],
                         a <- [1..b-1] ]

results :: [(Int, [Int])]
results = map grand input

main :: IO ()
main = print $ maximum results
