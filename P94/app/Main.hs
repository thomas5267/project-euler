module Main (main) where


area :: (Integer, Integer) -> Double
area (a, c) = 1/4 * sqrt (fromIntegral (4*a^2*c^2 - c^4))

f :: (Int, Int) -> (Int, Int)
-- f corresponds to the case where the special edge is longer by 1 unit.
-- Magical incantation from https://www.alpertron.com.ar/QUAD.HTM
-- There is another solution given by the magic incantation.
-- However, that solution has the wrong sign.
f (x, s) = (-2*x - s + 1, -3*x - 2*s + 1)

resultsF :: [Int]
-- First two results from iterate f is not valid.
resultsF = map go $ filter ((> 0) . fst) $ drop 2 $ iterate f (1, 0)
  where
    go (x, _) = 3*x + 1


-- f' :: (Int, Int) -> (Int, Int)
-- -- This solution has the wrong sign.
-- -- E.g.
-- -- f' (1, 0) = (-1, 2)
-- -- f' (-1, 2) = (5, -8)
-- f' (x, y) = (-2*x + y + 1, 3*x - 2*y - 1)

g :: (Int, Int) -> (Int, Int)
-- g corresponds to the case where the special edge is shorter by 1 unit.
-- Magic incantation sponsored by https://www.alpertron.com.ar/QUAD.HTM
-- The other solution given by the magic incantation also has the wrong sign.
g (x, s) = (-2*x - s - 1, -3*x - 2*s - 1)

resultsG :: [Int]
-- First two results from iterate g is not valid.
resultsG = map go $ filter ((> 0) . fst) $ drop 2 $ iterate g (-1, 0)
  where
    go (x, _) = 3*x - 1

main :: IO ()
main = print $ sum (lessThanOneBil resultsF) + sum (lessThanOneBil resultsG)
  where
    lessThanOneBil = takeWhile (< 10^(9 :: Int))
