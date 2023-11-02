module Main where

import Euler.Helper
import Data.Ratio
import Data.List (foldl1')

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

-- RootFrac as continuation?
-- Cont ::
-- ((RootFrac -> r) -> r)
-- -> (RootFrac -> ((RootFrac -> r) -> r))
-- -> ((RootFrac -> r) -> r)
--
-- Want:
-- want :: (Int -> RootFrac) -> (Int -> RootFrac) -> (Int -> RootFrac)
--
-- Fractions with root as (Int, Int, Int) -> RootFrac?
-- No, but possibly (Int, Int, Int, Int) -> RootFrac
-- (a, b, c, d) = (a*n + b*sqrt n + c) / d

data RootFrac = RootFrac { mul :: Int, root :: Int, numer :: Int, denom :: Int } deriving (Show, Eq)
-- Represents a fraction of the type (a * sqrt n + b) / c

moveSign :: RootFrac -> RootFrac
-- Move the sign from the denominator to the numerator.
moveSign a = if denom a > 0 then a else RootFrac (-1*mul a) (root a) (-1*numer a) (-1*denom a)

simplify :: RootFrac -> RootFrac
simplify z = moveSign $ if g == 1 then z else RootFrac az' nz' bz' cz'
  where
    g = foldl1' gcd [mul z, numer z, denom z]
    az' = (mul z) `quot` g
    bz' = (numer z) `quot` g
    nz' = root z
    cz' = (denom z) `quot` g

instance Num RootFrac where
-- None of the methods check whether the two RootFrac have the same root.

  -- (+) takes the radical in x and completely ignores the radical in y.
  (+) x y = simplify $ RootFrac a' n b' l
      where
        n = root x
        l = lcm (denom x) (denom y)
        ax = mul x
        ay = mul y
        bx = numer x
        by = numer y
        cx = denom x
        cy = denom y
        lx = l `quot` cx
        ly = l `quot` cy
        a' = ax*lx + ay*ly
        b' = bx*lx + by*ly

  -- (+) takes the radical in x and completely ignores the radical in y.
  (*) x y = simplify $ RootFrac a' n b' c'
    where
      n = root x
      ax = mul x
      ay = mul y
      bx = numer x
      by = numer y
      a' = ax*ay*n + bx*by
      b' = ax*by + ay*bx
      c' = denom x * denom y

  -- If a * sqrt n > b then no change else negate
  abs a = if (fI (mul a) * sqrt (fI (root a))) > fI (numer a) then a else negate a

  signum a
    | mul a == 0 && numer a == 0 = RootFrac 0 (root a) 0 1
    | (fI (mul a) * sqrt (fI (root a))) > fI (numer a) = RootFrac 0 (root a) 1 1
    | otherwise = RootFrac 0 (root a) 1 (-1)

  fromInteger a = RootFrac 0 1 (fI a) 1

  negate a = moveSign $ a { denom = -1*denom a }

instance Fractional RootFrac where
  fromRational a = RootFrac 0 1 (fI (numerator a)) (fI (denominator a))
  recip (RootFrac a n b c) = RootFrac (c*a) n (-c*b) (a^(2 :: Int) * n - b^(2 :: Int))

toRootFrac :: Int -> Int -> RootFrac
-- toRootFrac n a = (0*sqrt(n) + a) / 1
toRootFrac n a = RootFrac 0 n a 1

fromRootFrac :: RootFrac -> Double
fromRootFrac (RootFrac a n b c) = ((fI a)*sqrt(fI n) + fI b)/(fI c)

sqrtRootFrac :: Int -> RootFrac
-- Returns (sqrt n :: RootFrac) = (1*sqrt n + 0) / 1
sqrtRootFrac n = RootFrac 1 n 0 1

sqrtConvergents :: Int -> [Int]
-- Calculates the convergents of square root of n.
sqrtConvergents x = go (sqrtRootFrac x)
  where
    go :: RootFrac -> [Int]
    go k = a : go (recip b)
      where
        a :: Int
        a = floor (fromRootFrac k)
        b :: RootFrac
        b = k - toRootFrac x a

sqrtNotConvergents :: Int -> [RootFrac]
-- Returns the root fractions in the process of calculating covergents of square root of x.
sqrtNotConvergents x = tail $ go (sqrtRootFrac x)
  where
    go :: RootFrac -> [RootFrac]
    go k = k : go (recip b)
      where
        a :: Int
        a = floor (fromRootFrac k)
        b :: RootFrac
        b = k - toRootFrac x a

findCycle :: (Eq a) => [a] -> Int
findCycle l = findCycle' 1
  where
    findCycle' n = if a == take n b then n else findCycle' (n+1)
      where (a, b) = splitAt n l

answer :: Int
answer = (length . filter odd . map (findCycle . sqrtNotConvergents)) notSquares
  where
    notSquares :: [Int]
    notSquares = [1..10000] `deleteOrd` map (^(2 :: Int)) [1..]

main :: IO ()
main = print answer
