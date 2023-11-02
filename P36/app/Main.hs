module Main (main) where

import Euler.Helper

import Data.Int

isPalindrome :: Int -> Bool
isPalindrome n = digits @Int @Int8 n == reverseDigits n

isBase2Palindrome :: Int -> Bool
isBase2Palindrome n = digitsBase2 n == reverseDigitsBase2 n
  where
    digitsBase2 :: Int -> [Int8]
    -- Returns the digits of an integer in a list.
    digitsBase2 = go []
      where
        go cs k
          | k < 2 = fromIntegral k : cs
          | otherwise = go (fromIntegral r : cs) q
          where
            (q, r) = k `quotRem` 2

    reverseDigitsBase2 :: Int -> [Int8]
    -- Returns the digits of an integer in a list, least significant digit first.
    reverseDigitsBase2 k
      | k < 2 = [fromIntegral k]
      | otherwise = fromIntegral r : reverseDigitsBase2 q
      where
        (q, r) = k `quotRem` 2

main :: IO ()
main = do
  let result = sum $ filter (\n -> isPalindrome n && isBase2Palindrome n) [1 .. 1000000]
  print result
