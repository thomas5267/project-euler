module Main where

import Euler.Helper

-- We note that a^n has n*(logBase 10 a) + 1 digits.
-- In particular, 9^n has n*(logBase 10 9) + 1 digits and
-- 10^n has n+1 digits
-- Therefore, should 9^n has less than n digits we are done.
-- By experiment we find that 22*(logBase 10 9) + 1 < 22.

results :: [[Integer]]
results = map (\b -> filter ((==b) . length . digits) (map (^b) [1..9])) [1..21]

main :: IO ()
main = print $ length (concat results)
