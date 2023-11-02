module Main where

import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as V
-- import Euler.Helper (elemOrdV)
import Data.Numbers.Primes hiding (isPrime)


search :: Int -> [Int]
search n = search' revPyramid [] False
  where
    primeL = takeWhile (<=n) primes
    primeV = V.fromList primeL
    primeS = IS.fromDistinctAscList primeL
    revPyramid = reversePyramidGen n primeL

    search' :: [[Int]] -> [Int] -> Bool -> [Int]
    search' []     best _ = best
    search' (x:xs) best b
      | b = evenSpecial
      | null result = search' xs best (not b)
      | otherwise   = search' xs result (not b)
      where
        isPrime s = s `IS.member` primeS
        -- isPrime s = s `elemOrdV` primeV
        evenSpecial = if isPrime (head x)
                      then search' xs [head x] (not b)
                      else search' xs best     (not b)
        result = filter isPrime x


reversePyramidGen :: Int -> [Int] -> [[Int]]
-- Generates a reverse pyramid of consecutive sums
-- with base at the head of the list.
-- Only works with sorted lists but the precondition is not checked.
reversePyramidGen n l = nextLevel l l
  where
    nextLevel :: [Int] -> [Int] -> [[Int]]
    nextLevel [] _ = []
    nextLevel a (_:bs)  = a : nextLevel result bs
      where result = filter (<=n) (zipWith (+) a bs)

-- search :: Int -> [Int]
-- search n = search' pyramid (even $ V.length primeV)
--   where
--     primeL = takeWhile (<=n) primes
--     primeV = V.fromList primeL
--     -- primeS = IS.fromDistinctAscList primeL
--     pyramid = pyramidGen primeL
-- 
--     search' :: [[Int]] -> Bool -> [Int]
--     search' [] _ = []
--     search' (x:xs) b
--       | b            = if isPrime (head x) then [head x] else search' xs (not b)
--       | null results = search' xs (not b)
--       | otherwise    = results
--       where
--         -- isPrime s = s `IS.member` primeS
--         isPrime s = s `elemOrdV` primeV
--         isGood s = s <= n && isPrime s
--         results = filter isGood x
-- 
-- 
-- pyramidGen :: [Int] -> [[Int]]
-- pyramidGen l = nextLevel [sum l] (reverse l)
--   where
--     nextLevel :: [Int] -> [Int] -> [[Int]]
--     nextLevel _        []     = []
--     nextLevel al@(a:_) (b:bs) = al : nextLevel (result) bs
--       where result = (a-b) : zipWith (-) al l

-- search :: Int -> V.Vector Int
-- search n = search' pyramid (even $ V.length primeV)
--   where
--     primeL = takeWhile (<=n) primes
--     primeV = V.fromList primeL
--     -- primeS = IS.fromDistinctAscList primeL
--     pyramid = pyramidGen primeV
-- 
--     search' :: [V.Vector Int] -> Bool -> V.Vector Int
--     search' []     _ = V.empty
--     search' (x:xs) b
--       | b              = if isPrime (V.head x) then V.singleton (V.head x) else search' xs (not b)
--       | V.null results = search' xs (not b)
--       | otherwise      = results
--       where
--         -- isPrime s = s `IS.member` primeS
--         isPrime s = s `elemOrdV` primeV
--         isGood s = s <= n && isPrime s
--         results = V.filter isGood x
-- 
-- 
-- pyramidGen :: V.Vector Int -> [V.Vector Int]
-- pyramidGen v = take lenV $ iterate nextLayer (V.singleton (V.sum v))
--   where
--     lenV = V.length v
--     nextLayer :: V.Vector Int -> V.Vector Int
--     nextLayer w
--       | lenV == lenW = w
--       | otherwise    = V.generate (V.length w + 1) element
--       where
--         lenW = V.length w
--         element 0 = w V.! 0 - v V.! (lenV - lenW)
--         element n = w V.! (n-1) - v V.! (n-1)



-- Uses too much memory as it generates the entire pyramid
-- pyramidGen :: V.Vector Int -> [V.Vector Int]
-- pyramidGen v = pyramidGen' [v] (v, V.tail v)
--   where
--     pyramidGen' :: [V.Vector Int] -> (V.Vector Int, V.Vector Int) -> [V.Vector Int]
--     pyramidGen' acc (u, w)
--       | V.null w  = acc
--       | otherwise = pyramidGen' (result : acc) (result, V.tail w)
--       where
--         result = V.zipWith (+) u w

main :: IO ()
main = print $ search 1000000 -- == 997651
-- main = print $ search 500000 -- == 499607
