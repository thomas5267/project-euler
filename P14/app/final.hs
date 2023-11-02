module Main where

stoppingTime :: Int -> (Int, Int)
stoppingTime n = stoppingTime' 1 n
  where
    stoppingTime' s 1 = (s, n)
    stoppingTime' s k
      | r == 0    = stoppingTime' (s+1) q
      | otherwise = stoppingTime' (s+2) (3*q + 2)
      where (q, r) = k `quotRem` 2

main :: IO ()
main = print $ maximum (map stoppingTime [1..1000000])
