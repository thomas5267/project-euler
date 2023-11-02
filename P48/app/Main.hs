module Main where

powerMod :: Integer -> Integer -> Integer -> Integer
-- Calculates a^b mod n
powerMod a b n = powerMod' 1 a b
  where
    powerMod' _   0 _ = 0
    powerMod' acc x 1 = (acc * x) `rem` n
    powerMod' acc x y = powerMod' (acc*(if r == 0 then 1 else x) `rem` n) (x^(2 :: Int) `rem` n) q
      where (q, r) = y `quotRem` 2

result :: [Integer]
result = [ powerMod a a (10^10) | a <- filter (\x -> x `rem` 100 /= 0) [1..1000] ]

answer :: Integer
answer = sum result `rem` 10^10

main :: IO ()
main = print "hello"
