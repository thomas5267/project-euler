module Main where

import Data.List (sortOn)
import Data.List.Extra (groupOn, maximumOn)

import qualified Data.Vector.Unboxed as V

import Data.Maybe (maybeToList)

elemOrd :: (Ord a, V.Unbox a) => a -> V.Vector a -> Bool
-- Binary search on vectors
elemOrd k v
  | V.null v = False
  | otherwise = elemOrd' 0 (V.length v - 1)
  where
    elemOrd' l r
      | l == r = v V.! m == k
      | x >  k = elemOrd' l (m-1)
      | x <= k = elemOrd' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            x = v V.! m

elemIndexOrd :: (Ord a, V.Unbox a) => a -> V.Vector a -> Maybe Int
-- Binary search on vectors. Returns the index of the element.
elemIndexOrd k v
  | V.null v = Nothing
  | otherwise = elemIndexOrd' 0 (V.length v - 1)
  where
    elemIndexOrd' l r
      | l == r = if v V.! m == k then Just m else Nothing
      | x >  k = elemIndexOrd' l (m-1)
      | x <= k = elemIndexOrd' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            x = v V.! m

squares :: V.Vector Int
squares = V.generate 1000 (^2)

pythagoreanTriple :: [(Int, Int, Int, Int)]
pythagoreanTriple = [ (p, a, b, c) | a <- [1..999],
                                     b <- [a..(1000-a)],
                                     let c' = (a^2 + b^2) `elemIndexOrd` squares,
                                     c <- maybeToList c',
                                     c <= 1000 - a - b,
                                     let p = a + b + c]

results :: [[(Int, Int, Int, Int)]]
results = ((groupOn first) . (sortOn first)) pythagoreanTriple
  where
    first (a, _, _, _) = a

answer :: [(Int, Int, Int, Int)]
answer = maximumOn length results

main :: IO ()
main = print answer
