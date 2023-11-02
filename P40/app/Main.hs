module Main where

import Data.Char (digitToInt)

digits :: Int -> [Int]
digits n = map digitToInt (show n)

champ :: [Int]
champ = concatMap digits [0..]

answer :: Int
answer = product [champ !! (10 ^ x) | x <-  ([0 .. 6] :: [Int])]

main :: IO ()
main = print answer
