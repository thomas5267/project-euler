module Main (main) where

import Data.List.Split

import Data.List (foldl')

preprocess :: [Double] -> (Double, Double)
preprocess (x1:x2:_) = (x1, x2)
preprocess [_] = error "preprocess: only one item"
preprocess [] = error "preprocess: empty list"

main :: IO ()
main = do
    file <- readFile "p099_base_exp.txt"
    let input :: [(Double, Double)]
        input = map (preprocess . map read . splitOn ",") . lines $ file
    print $ foldl' go (1, (0, 0)) $ map (\ (b, e) -> e*log b) input
      where
        go :: (Int, (Int, Double)) -> Double -> (Int, (Int, Double))
        go (c, p@(_, m)) x
          | m < x = (c+1, (c, x))
          | otherwise = (c+1, p)
