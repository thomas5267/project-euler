module Main (main) where

import Data.List (find)

-- n = r + b
-- First pair (b, n) such that b/n*(b-1)/(n-1) = 1/2
-- 2*b*(b-1) = n*(n-1)
-- Consider mod 4 of the equation above.
-- b*(b-1) is always even hence LHS is 0 mod 4
-- n therefore must be 0 or 1 mod 4.
--
-- 2*b*(b-1) = n*(n-1)
-- Consider mod 3 of the equation above.
-- b*(b-1) is always even hence LHS is 0 mod 4
-- n therefore must be 0 or 1 mod 4.
--
-- n = 21, 2*15*16 == 20*21
-- n = 120, 2*85*84 == 120*119
--
-- n^2 - n = 2b^2 - 2b
-- n^2 - 2b^2 = n - 2b
-- (n + sqrt 2 * b)(n - sqrt 2 * b) = n - 2b
--
--
-- I do not understand any of the following,
-- but the method works.
--
-- From https://www.alpertron.com.ar/METHODS.HTM#SolGral
-- n^2 - 2b^2 - n + 2b == 0
-- x = n
-- y = b
-- A = 1, B = 0, C = -2,
-- D = -1, E = 2, F = 0
-- in Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0
--
-- g = gcd (-8) 4 = 4
-- x1 = 2x - 1
-- y1 = -2y + 1
--
-- -2(x1)^2 + 4(y1)^2 + (-4 + 2) == 0
-- -2(x1)^2 + 4(y1)^2 - 2 == 0
-- -(x1)^2 + 2(y1)^2 - 1 == 0
-- (x1)^2 - 2(y1)^2 + 1 == 0
-- A = 1, C = -2, F = 1, B = D = E = 0
--
-- X(n+1) = P Xn + Q Yn
-- Y(n+1) = R Xn + S Yn
--
-- P = 3
-- Q = 4
-- R = 2
-- S = 3
--
-- x1 = 1
-- y1 = 1
--
-- X(n+1) = 3 Xn + 4 Yn
-- Y(n+1) = 2 Xn + 3 Yn
--
-- X2 = 7
-- Y2 = 5
--
-- 7 = 2x - 1
-- x = 4
-- -5 = -2y + 1
-- y = 3
--
-- X3 = 41
-- Y3 = 29


negativePellSolutions :: [(Int, Int)]
-- Solutions to x^2 - 2y^2 = -1
negativePellSolutions = iterate go (1, 1)
  where
    go (x, y) = (3*x + 4*y, 2*x + 3*y)

results :: [(Int, Int)]
results = map (\ (a, b) -> ((a+1) `quot` 2, (b+1) `quot` 2)) negativePellSolutions


main :: IO ()
main = print $ find ((>= 10^12) . fst) results
