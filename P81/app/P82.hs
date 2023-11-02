module Main where

import Data.Array.Unboxed
import qualified Data.Set as S

import Common

p82 :: Adjacent
p82 bound (r, c) =
    S.fromAscList $ filter (inRange bound) [(r-1, c), (r, c+1), (r+1, c)]

p82Start :: Starter
p82Start m = let ((r0, _), (rm, _)) = bounds m
              in [((r, 0), Weight (m ! (r,0))) | r <- [r0..rm]]

p82Stop :: Stopper
p82Stop = (==79) . snd

main :: IO ()
main = do
    file <- readFile "matrix.txt"
    let input  = (parseToArray ((0, 0), (79, 79)). parseToList) file
    print $ dijkstra input p82 p82Start p82Stop
