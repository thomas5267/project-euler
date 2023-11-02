module Main where

import Euler.Helper
import qualified Euler.DigitSet as DS
import qualified Data.IntSet as IS

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = f a && g a

bound :: Int
bound = 1000000

isMultipleGood :: Int -> Int -> Bool
isMultipleGood m n = DS.fromInt n == DS.fromInt (m*n)

twoSet :: IS.IntSet
twoSet = IS.fromDistinctAscList twoList
  where
    twoList = filter (isMultipleGood 2) [1..bound]

answer :: IS.IntSet
answer = IS.filter (isMultipleGood 3 .&&. isMultipleGood 4 .&&.
                    isMultipleGood 5 .&&. isMultipleGood 6)
                   twoSet

main :: IO ()
main = print answer
