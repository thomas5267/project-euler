module Main where
import Data.Numbers.Primes
import qualified Data.IntMap.Strict as IntMap

import Data.List (elemIndex)
import Data.Maybe (fromJust)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

reciprocalDigits :: Int -> [Int]
reciprocalDigits n = digit 10
  where digit k = q : digit (10*r)
          where (q,r) = k `quotRem` n

reciprocalRemainder :: Int -> [Int]
reciprocalRemainder k = tail $ iterate (\x -> x*10 `rem` k) 10
-- I do not know the significance of this
-- But seems very important

repetendSlow :: Int -> Int
repetendSlow n
  | n == 1         = 0
  | even n         = repetendSlow (n `quot` 2)
  | n `rem` 5 == 0 = repetendSlow (n `quot` 5)
  | otherwise      = fromJust (elemIndex r rs) + 1
    where (r:rs) = reciprocalRemainder n

main :: IO ()
main = print (i, m)
  where
    candidates = map repetendSlow [1,3..1000]
    m = maximum candidates
    i = 2 * fromJust (elemIndex m candidates) + 1
