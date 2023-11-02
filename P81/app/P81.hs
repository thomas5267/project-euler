module Main where

import Data.Array.Unboxed
import qualified Data.Set as S

import Common

p81 :: Adjacent
p81 bound (r, c) =
    S.fromAscList $ filter (inRange bound) [(r, c+1), (r+1, c)]

p81Start :: Starter
p81Start m = [((0, 0), Weight (m ! (0, 0)))]

p81Stop :: Stopper
p81Stop = (==(79,79))

main :: IO ()
main = do
    file <- readFile "matrix.txt"
    let input  = (parseToArray ((0, 0), (79, 79)). parseToList) file
    print $ dijkstra input p81 p81Start p81Stop
