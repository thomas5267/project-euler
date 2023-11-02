module Main where

-- Top right diagonal is the squares of odd numbers
-- tr n = (2n+1)^2, n <- [0..]
tr :: Integer -> Integer
tr n = (2*n+1)^2

-- Bottom right:
-- br = [1, 3, 13, 31..]
-- c = 1
-- a + b = 2
-- 4a + 2b = 12
-- 2a = 8
-- a = 4
-- b = -2
br :: Integer -> Integer
br n = 4*n^2 - 2*n + 1

-- Bottom left:
-- bl = [1, 5, 17, 37..]
-- c = 1
-- a + b = 4
-- 4a + 2b = 16
-- 2a = 8
-- a = 4
-- b = 0
bl :: Integer -> Integer
bl n = 4*n^2 + 1

-- Top Left
-- tl = [1, 7, 21, 43..]
-- c = 1
-- a + b = 6
-- 4a + 2b = 20
-- 2a = 8
-- a = 4
-- b = 2
tl :: Integer -> Integer
tl n = 4*n^2 + 2*n + 1

-- Sum of diagonals:
-- sd n = 16n^2 + 2n - 2n + 0n + 4n + 4
sd :: Integer -> Integer
sd n = 16*n^2 + 4*n + 4
-- Note: sd 1 = 4 which is incorrect

-- Sum of sums of diagonals:
-- Try if a cubic polynomial can fit the sum of sums with the correct sd 1 = 1
-- [1, 25, 101, 261, 537..]
-- f = an^2 + bn^2 + cn + d
-- d = 1
--   a +  b +  c = 24   (1)
--  8a + 4b + 2c = 100  (2)
-- 27a + 9b + 3c = 260  (3)
--
-- (2) - 2*(1):
-- 6a + 2b = 52  (4)
--
-- (3) - (2) - (1)
-- 18a + 4b = 136  (5)
--
-- (5) - 2*(4)
-- 6a = 32
-- a = 16/3
--
-- 6a + 2b = 52
-- 32 + 2b = 52
-- b = 10
--
-- a + b + c = 24
-- 16/3 + 10 + c = 24
-- 10 + c = 56/3
-- c = 26/3
--
-- f n = 16/3*n^3 + 10n^2 + 26/3*n + 1 (?)
-- f 4 = 537 as required
--
f :: Integer -> Integer
f n = 10*n^2 + 1 + (16*n^3 + 26*n) `quot` 3

main :: IO ()
main = print $ f 500 -- Layer 0 is 1x1, layer n is (2n+1)x(2n+1), question asks for 1001 x 1001
