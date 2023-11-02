module Main where

triangleNumber :: Int -> Int
triangleNumber n = (n*(n+1)) `quot` 2

pentagonalNumber :: Int -> Int
pentagonalNumber n = (n*(3*n-1)) `quot` 2

hexagonalNumber :: Int -> Int
hexagonalNumber n = n*(2*n-1)

elemOrdF :: (Integral a, Ord b) => b -> (a -> b) -> Bool
-- Binary search on monotonic functions
elemOrdF k f = uncurry elemOrdF' bound
  where
    bound = (1, genUpperBound 100)
      where
        genUpperBound b = if k <= f b then b else genUpperBound ((b+1)*2)
    elemOrdF' l r
      | l == r = f m == k
      | k <  y = elemOrdF' l (m-1)
      | k >= y = elemOrdF' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            y = f m


main :: IO ()
main = print $ take 3 $ filter (`elemOrdF` pentagonalNumber) (map hexagonalNumber [1..])

