module Main where
import System.IO

f x y z = x + max y z
g xs ys = zipWith3 f xs ys $ tail ys

main :: IO ()
main = do
    contents <- readFile "p067_triangle.txt"
    let tri :: [[Int]]
        tri = map (map read . words) (lines contents)
    print $ head $ foldr1 g tri


