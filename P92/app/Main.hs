{-# LANGUAGE BangPatterns #-}

module Main where

import Euler.Helper

import Data.Vector qualified as V
import Data.List (foldl')

bound :: Int
bound = 10^(7 :: Int)

square :: Int -> Int
square n = n^(2 :: Int)

digitSquare :: Int -> Int
digitSquare = go . digits
  where
    go :: [Int] -> Int
    go !xs = foldl' (\ s d -> s + (smallSquares V.! d)) 0 xs
      where
        smallSquares = V.generate 10 square

chain89 :: Int -> Bool
chain89 0  = False
chain89 1  = False
chain89 89 = True
chain89 n  =
    let nextIter = digitSquare n
     in if nextIter > bound
        then chain89 nextIter
        else resultsFast V.! nextIter

resultsFast :: V.Vector Bool
resultsFast = V.generate (bound+1) chain89

answerFast :: Int
answerFast = V.length $ V.filter (== True) resultsFast


main :: IO ()
main = print answerFast
