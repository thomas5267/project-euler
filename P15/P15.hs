{-

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?

-}

fac :: Integer -> Integer

fac n = product [1..n]

f :: Integer -> Integer
f n = fac (2*n) `div` ((fac n)^2)

result = f 20
