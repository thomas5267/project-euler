module Main where

import Euler.Helper

lychrelStep :: Integer -> Integer
lychrelStep n = n + fromDigits (reverseDigits n)

isPalindrome :: Integer -> Bool
isPalindrome n = digits n == reverseDigits n

isLychrel :: Integer -> Bool
-- It has been given for numbers below 10000 that either it becomes a palindrome
-- in less than 50 steps or no one has been able to map it to a palindrome with
-- all the computing power we have.
isLychrel = not . any isPalindrome . take 50 . tail . iterate lychrelStep

main :: IO ()
main = print $ length (filter isLychrel [1..10000])
