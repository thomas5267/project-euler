module Main where
import qualified Data.Vector.Unboxed as V
import Data.Maybe (fromJust)

fibList :: [Integer]
fibList = map int_fib [0 ..]
    where int_fib 0 = 0
          int_fib 1 = 1
          int_fib n = fibList !! (n-2) + fibList !! (n-1)

fib :: Int -> Integer
fib n = fibList !! n

-- log10 (fib 4750) = 992.3
-- log10 (fib 5000) = 1044.6
-- Binary search to find the first 1000 digit Fibonacci number

digits :: Integer -> Int
digits = length . show

fibDigits :: V.Vector Int
fibDigits = V.generate 250 (\n -> (digits . fib) (n + 4750))

elemIndexOrd :: (Ord a, Num a, V.Unbox a) => a -> V.Vector a -> Maybe Int
-- Binary search on vectors
elemIndexOrd k v
  | V.null v = Nothing
  | otherwise = elemIndexOrd' 0 (V.length v)
  where
    elemIndexOrd' :: Int -> Int -> Maybe Int
    elemIndexOrd' l r
      | l >= r = if l <= (V.length v - 1) then Just l else Nothing
      | k <= x = elemIndexOrd' l     m
      | k >  x = elemIndexOrd' (m+1) r
      where m = (l + r) `quot` 2
            x = v V.! m

answer = fromJust (elemIndexOrd 1000 fibDigits) + 4750

main :: IO ()
main = print answer

