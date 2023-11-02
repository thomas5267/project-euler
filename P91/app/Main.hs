module Main (main) where

import Euler.Helper

import Data.Maybe

import Data.Bitraversable

square :: Int -> Int
square x = x^(2 :: Int)
{-# INLINE square #-}

-- isSquare :: Int -> Bool
-- isSquare = (`elemOrdF` (\ x -> x^(2 :: Int)))

sqrtInt :: Int -> Maybe Int
-- Returns the square root of an integer if it is a perfect square,
-- otherwise return Nothing.
sqrtInt n = if square r == n
            then Just r
            else Nothing
  where
    r = n `binarySearchF` square

quotInt :: Int -> Int -> Maybe Int
-- Returns Just (a `quot` b) if a is divisible by b,
-- otherwise return Nothing.
quotInt a b = if r == 0
              then Just q
              else Nothing
  where
    (q, r) = a `quotRem` b


countF :: Int -> Int
countF n = 3 * square n

g :: Int -> [(Int, Int)]
-- g returns the solutions to x^2 + y^2 - nx == 0, x, y > 0
-- This is used in the case where the hypotenuse is aligned with the grid
-- and the case where no edges are aligned with the grid.
--
-- For the case where hypotenuse is aligned with the grid,
-- g returns a list of RA triangles whose hypotenuse is of length n
-- and is aligned with the x-axis.
-- The hypotenuse is assumed to have one end at (0, 0) and the other end at (n, 0).
-- Hence, the RA triangles are represented solely by the vertex at which
-- the two legs intersect.
-- This function does not take account into reflection symmetry.
-- E.g. g 5 = [(1,2),(4,2)] despite the (4,2) triangle is a mirror image
-- of the (1,2) triangle.
--
-- This function is probably used only in the context of map g [1..50]
-- map g [1..50] probably admits a more efficient implementation by
-- calculating g for prime arguments and combining the results together.
-- But it is unnecessarily complicated for minimum gain if any.
--
-- traverse acts on the second component of a pair in this context.
-- traverse :: (a   -> f b)       -> t a        -> f (t b)
-- traverse :: (Int -> Maybe Int) -> (Int, Int) -> Maybe (Int, Int)
-- f :: Maybe
-- t :: (Int,)
g n = mapMaybe (traverse sqrtInt . toVertex) [1..n-1]
  where
    toVertex x = (x, x*(n-x))

countG :: Int -> Int
-- Counts the number of good triangles whose hypotenuse is aligned with a nxn
-- grid.
countG n = 2 * length (concatMap g [1..n])

h :: (Int, Int) -> [(Int, Int)]
-- h returns the list of good triangles with no edges aligned with the grid.
-- h (xc, yc) = mapMaybe filterer . concatMap go $ g (square xc + square yc)
h (xc, yc) = filter notAligned
           . mapMaybe filterer
           . concatMap go
           $ g (square xc + square yc)
  where
    c = square xc + square yc
    go (pa, qa) = [(xc*pa - yc*qa, yc*pa + xc*qa), (xc*pa + yc*qa, yc*pa - xc*qa)]
    filterer :: (Int, Int) -> Maybe (Int, Int)
    filterer = bitraverse (`quotInt` c) (`quotInt` c)
    notAligned (xa, ya) = xa >= 1 && ya >= 1 && xa /= xc && ya /= yc

countH :: Int -> Int
countH n = length $ filter inBounds $ concatMap h (grid n)
  where
    inBounds (xa, ya) = xa <= n && ya <= n



grid :: Int -> [(Int, Int)]
grid n = [ (x, y) | y <- [1..n], x <- [1..n] ]


totalCount :: Int -> Int
totalCount n = countF n + countG n + countH n



main :: IO ()
-- main = print $ map countH [1..50]
main = print $ totalCount 50
