module Main (main) where

import Euler.Helper

import Data.List (sort)

-- (1, 2)
-- 4 digit <> 5 digit
-- [5000 .. 9999]
--
-- (1, 2, 3)
-- 3 digit <> 3 digit <> 3 digit
-- 2 digit is impossible as 99 * 3 = 297 which means that 99 `concatMultiply` (1, 2, 3) = some 8 digit number
-- [100 .. 333]
--
-- (1, 2, 3, 4)
-- 2 digit <> 2 digit <> 2 digit <> 3 digit
-- [25 .. 33]
-- 30 impossible because it has 0, 33 impossible because it has repeated 3
--
-- 1 digit <> 2 digit <> 3 digit <> 3 digit impossible
-- 9 * 4 = 36
--
-- (1, 2, 3, 4, 5)
-- 1 digit <> 2 digit <> 2 digit <> 2 digit <> 2 digit
-- [5 .. 9]
--
-- (1, 2, 3, 4, 5, 6)
-- 1 digit <> 1 digit <> 1 digit <> 2 digit <> 2 digit <> 2 digit
-- [3 .. 3]
--
-- (1, 2, 3, 4, 5, 6, 7)
-- 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 2 digit <> 2 digit
-- Impossible, 5 * x < 10, so x < 2, 1 is clearly not possible
--
-- (1, 2, 3, 4, 5, 6, 7, 8)
-- 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 2 digit
-- Impossible, 1 is the only possible integer satisfying 7 * x < 10
--
-- (1, 2, 3, 4, 5, 6, 7, 8, 9)
-- 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit <> 1 digit
-- 123456789 only solution
--

concatProduct2Digits :: [Int]
concatProduct2Digits =
  fmap (* 100002)
    . filter
      ( \n ->
          n `mod` 10 /= 0
            && n `mod` 11 /= 0
            && n `mod` 101 /= 0
            && n `mod` 1001 /= 0
            && n `mod` 1010 /= 0
            && n `mod` 1100 /= 0
      )
    $ [5000 .. 9999]

concatProduct3Digits :: [Int]
concatProduct3Digits = fmap (* 1002003) . filter (\n -> n `mod` 10 /= 0 && n `mod` 11 /= 0 && n `mod` 101 /= 0) $ [100 .. 333]

concatProduct4Digits :: [Int]
concatProduct4Digits = fmap (* 10203004) $ [25 .. 29] ++ [31, 32]

concatProduct5Digits :: [Int]
concatProduct5Digits = fmap (* 102030405) [5 .. 9]

concatProduct6Digits :: [Int]
concatProduct6Digits = [369121518]

concatProduct :: [Int]
concatProduct =
  concatProduct2Digits
    ++ concatProduct3Digits
    ++ concatProduct4Digits
    ++ concatProduct5Digits
    ++ concatProduct6Digits

isPandigital :: Int -> Bool
isPandigital n = (sort . reverseDigits @Int @Int $ n) == [1 .. 9]

main :: IO ()
main = print $ maximum . filter isPandigital $ concatProduct
