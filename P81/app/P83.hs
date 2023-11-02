module Main where

import Data.Array.Unboxed
import qualified Data.Set as S

import Common

p83 :: Adjacent
p83 bound (r, c) =
    S.fromAscList $ filter (inRange bound) [(r-1, c), (r, c-1), (r, c+1), (r+1, c)]

p83Start :: Starter
p83Start m = [((0, 0), Weight (m ! (0, 0)))]

p83Stop :: Stopper
p83Stop = (== (79,79))

main :: IO ()
main = do
    file <- readFile "matrix.txt"
    let input  = (parseToArray ((0, 0), (79, 79)). parseToList) file
    print $ dijkstra input p83 p83Start p83Stop
