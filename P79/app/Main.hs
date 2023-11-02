module Main where

import Euler.Helper

import qualified Data.Graph as G

toEdge :: Int -> [G.Edge]
-- E.g. the number 319 builds two edges, 3 -> 1 and 1 -> 9
toEdge = toEdge' . digits
  where
    toEdge' []             = []
    toEdge' [_]            = []
    toEdge' (x1:xs@(x2:_)) = (x1, x2) : toEdge' xs


main :: IO ()
main = do
    file <- readFile "p079_keylog.txt"
    let numbers :: [Int]
        numbers = (map read . lines) file
        edges = concatMap toEdge numbers
    print $ G.topSort (G.buildG (0, 9) edges)
    -- let allDigits = (foldr1 DS.union . map DS.fromInt) numbers
    -- print allDigits
