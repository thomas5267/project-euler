module Main where
import Data.Char(digitToInt)

fac :: Integer -> Integer
fac n = product [1..n]

facReduced :: Integer -> Integer
facReduced n = facReduced' 1 [1..n]
  where
    facReduced' acc []     = acc
    facReduced' acc (x:xs)
      | r == 0    = facReduced' q xs
      | otherwise = facReduced' p xs
      where p = acc*x
            (q, r) = p `divMod` 10

digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

answer = digitSum $ facReduced 100

main :: IO ()
main = print answer
