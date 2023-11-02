module Main where

import Data.Numbers.Primes
import Euler.Helper
import qualified Euler.DigitSet as DS

import qualified Data.IntSet as IS

import Control.Monad (guard)

candidates :: [Int]
candidates = (takeWhile (<=9999) . dropWhile (<=1000)) primes

result :: [[Int]]
result = do
    p <- candidates
    q <- dropWhile (<=p) candidates
    guard (DS.fromInt p == DS.fromInt q)
    let d = q - p
        r = q + d
    guard ((q + d) `IS.member` candidatesS && DS.fromInt r == DS.fromInt q)
    return [p, q, r]
    where candidatesS = IS.fromDistinctAscList candidates

main :: IO ()
main = print $ fromDigits @Int (concatMap digits (result !! 1))
