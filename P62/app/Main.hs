module Main where

import Euler.Helper
import Data.List (groupBy, partition)
import qualified Data.IntMultiSet as IMS

cubes :: Int -> Int
cubes k = k^(3 :: Int)

toIMS :: Int -> IMS.IntMultiSet
toIMS = IMS.fromList . digits

hasDigits :: Int -> Int -> Bool
-- Returns True if n has k digits
hasDigits k n = 10^(k-1) <= n && n < 10^k

hasEqualDigits :: Int -> Int -> Bool
hasEqualDigits a = hasDigits (length (digits a))

groupByDigits :: [Int] -> [[Int]]
groupByDigits = groupBy hasEqualDigits

isPermutationOf :: Int -> Int -> Bool
-- A number b is said to be a permutation of a if
-- they contain the same digits and permuting the digits of a yields b.
-- E.g. 125 `isPermutationOf` 512 == True
-- E.g. 216 `isPermutationOf` 343 == False
isPermutationOf m n = toIMS m == toIMS n

permutedNumbers :: [Int] -> [[Int]]
-- Seperates a list of numbers into sublists.
-- Each sublists contain numbers that have the same digits but permuted.
-- E.g. permutedNumbers [125,216,343,512,729] = [[125,512],[216],[343],[729]]
permutedNumbers [] = []
permutedNumbers l  = helper l
  where
    helper :: [Int] -> [[Int]]
    helper []       = []
    helper xl@(x:_) = result : helper yl
      where
        (result, yl) = partition (isPermutationOf x) xl

answer :: [Int]
answer = head $ filter ((==5) . length) (concatMap permutedNumbers cubesByDigits)
  where cubesByDigits = groupByDigits (map cubes [1..])

main :: IO ()
main = print answer
