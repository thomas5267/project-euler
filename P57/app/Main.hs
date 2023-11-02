module Main where

import Euler.Helper
import Data.Ratio
import Data.List (foldl', mapAccumL)

root2 :: [Integer]
root2 = 1 : repeat 2

convergents :: [Integer] -> [Rational]
convergents l = memo_convergents
  where
    memo_convergents = snd (mapAccumL slow_convergents 0 l)
    slow_convergents 0 a0 = (1, fromIntegral a0)
    slow_convergents 1 a1 = (2, head memo_convergents + 1 % a1)
    slow_convergents n an = (n+1, (an*(numerator cn1) + numerator cn2) %
                                  (an*(denominator cn1) + denominator cn2))
      where
        cn1 = memo_convergents !! (n-1)
        cn2 = memo_convergents !! (n-2)

result :: [Rational]
result = take 1001 $ convergents root2

answer :: Integer
answer = foldl' go 0 result
  where
    go acc r =
        case numerator r `compareDigits` denominator r of
          GT -> acc + 1
          _  -> acc

main :: IO ()
main = print answer
