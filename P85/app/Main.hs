module Main where

import Data.List (minimumBy)
import Data.Ord

-- test :: Int -> Int -> Int
-- test x y = sum $ (*) <$> [1..x] <*> [1..y]

count :: Int -> Int -> Int
count x y = (x*(x+1)*y*(y+1)) `quot` 4

results :: [(Int, (Int, Int))]
results = [ (count x y - 2000000, (x, y)) | y <- [1..2000], x <- [y..2000] ]

answer :: (Int, (Int, Int))
answer = minimumBy (comparing (abs . fst)) results

main :: IO ()
main = print answer
