{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector.Unboxed as V

import Control.Monad.ST
import qualified Data.Vector.Algorithms.Intro as MV

import Data.Maybe (fromJust)

globalBound :: Int
globalBound = 1000000

square :: Int -> Int
square n = n^(2 :: Int)
{-# INLINE square #-}

-- Claim:
-- Consider an x by y by z cuboid with x >= y >= z,
-- x, y, z not necessarily integral.
-- The shortest path is necessarily sqrt (x^2 + (y+z)^2).
--
-- Proof:
-- The shortest path on the cuboid is a straight line on the net of cuboid.
-- Hence, this question is equivalent to minimising sqPath = a^2 + b^2,
-- where without loss of generality a is one of (x, y, z)
-- and b is the sum of other two edges.
-- Intuitively to minimise a^2 + b^2,
-- a and b should be as similiar in magnitude as possible.
--
-- Claim:
-- Minimum of a^2 + b^2 is obtained when a = b,
-- and given x >= y >= z, a = x and b = y+z minimises a^2 + b^2.
--
-- Proof:
-- Let s = a+b.
-- sqPath = a^2 + b^2 = a^2 + (s-a)^2
-- sqPath = 2a^2 - 2sa + s^2
-- d sqPath / da = 4a - 2s
-- d sqPath / da = 0 <=> a = s/2
-- The minimum is indeed obtained when a^2 = b^2.
--
-- Claim:
-- The closer a is to b, the smaller a^2 + b^2 will be.
--
-- Proof:
-- As sqPath is a parabola in a, sqPath has an unique global minimum.
-- Hence, the closer a is to the global minimum of b,
-- the smaller the sqPath will be.
--
-- --  Notes:
-- --  d sqPath / da = 4a - 2s
-- --  By construction x >= s/3
-- --  -2s/3 <= d sqPath / dx < 2s
-- --
-- --  The minimum is obtained when the aspect ratio of the cuboid is 2:1:1.
-- --  The shortest path on that cuboid is in fact the diagonal of
-- --  a square on the net of cuboid.
-- --  However, this diagonal is necessarily non-integral as sqrt(2) is irrational.
--
--
-- We have shown that a^2 + b^2 is minimised when a = b,
-- and the smaller |a-b| is, the smaller a^2 + b^2 will be.
-- However, we have not shown that a = x, b = y+z minimises |a-b|.
--
-- Claim:
-- a = x, b = y+z minimises |a-b|.
--
-- Proof:
-- Consider a = y, b = x+z.
-- We seek to prove |y - (x+z)| >= |x - (y+z)|.
-- As x >= y >= z, |y - (x+z)| = x+z-y.
--
-- x+z-y >= |x-y-z|
--
-- Either
-- x+z-y >= x-y-z <=> z >= 0 which is always true,
-- or
-- x+z-y >= y+z-x <=> 2x - 2y >= 0 <=> x >= y which is also always true.
-- So a = y, b = x+z is indeed no good.
--
-- Consider a = z, b = x+y.
-- We seek to prove |z - (x+y)| >= |x - (y+z)|.
-- As x >= y >= z, |z - (x+y)| = x+y-z.
--
-- x+y-z >= |x - (y+z)|
--
-- Either
-- x+y-z >= x-y-z <=> 2y >= 0 which is always true,
-- or
-- x+y-z >= y+z-x <=> 2x - 2z >= 0 <=> x >= z which is also always true.
-- So a = z, b = y+z is no good either.
--
-- We have exhausted all the possibilities.
-- Therefore, a = x, b = y+z minimises |a-b| and hence a^2 + b^2.
-- The shortest path on the cuboid is indeed sqrt(x^2 + (y+z)^2).
--
--
-- Since the question demands the shortest path on the cuboid be integral,
-- it is natural to consider Pythagorean triples.
-- To simplify the question, we shall consider only the
-- primitive triples.
-- In the proof we have considered without loss of generality
-- a = x, b = y+z.
-- However, the algorithm which generates the Pythagorean triples
-- considers the triple (a, b, c) equivalent to the
-- triple (b, a, c).
-- In other words, if (a, b, c) is the triple generated by the algorithm,
-- (b, a, c) would not be generated by the algorithm.
-- The proof is not affected by this,
-- but the implementation will be hugely problematic.
--
----------------------------------------------------------
--
-- https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
-- a = m^2 - n^2
-- b = 2mn
-- c = m^2 + n^2
-- m > n > 0
-- m, n coprime and not both odd
--
-- We note that a is always odd and b is always even.
-- b is a multiple of two.
-- If a were to be even then m and n would either be both odd or both even.
-- This either contradicts m, n not both odd, or m, n coprime.
--
--------------------------------------------------------------------------------

--
-- What are the bounds for m, n?
-- We seek (a <= bound || b <= bound)
-- Additionally, as x is the longest edge of the cuboid,
-- 2x > y+z
--
--------------------------------------------------------------------------------

--
-- Case A: a = x
--
-- a = x <= bound
-- m^2 - n^2 <= bound
-- m^2 <= bound + n^2
-- -sqrt(bound + n^2) <= m <= sqrt (bound + n^2)
-- n < m <= sqrt (bound + n^2)
-- n < m <= O(n)
-- Does not look promising.
-- WolframAlpha alleged that sqrt (bound + n^2) - n < 1 when n > (bound-1)/2
--
-- Suppose n < m <= sqrt (bound + n^2) < n+1,
-- i.e. m is between two integers thus cannot be an integer.
-- n < m <= sqrt (bound + n^2) < n+1
-- n^2 < m^2 <= bound + n^2 < n^2 + 2n + 1
-- 0 < m^2 - n^2 <= bound < 2n+1
-- 0 < a <= bound < 2n+1
--
-- When 2n+1 > bound,
-- sqrt (bound + n^2) < sqrt (2n + 1 + n^2) = n+1
-- n < m <= sqrt (bound + n^2) < n+1
-- Hence m cannot be integral.
-- For m to have solution,
-- n <= (bound-1)/2
--
--
-- 2x >= y+z
-- 2a >= b
-- 2m^2 - 2n^2 >= 2mn
-- 2m^2 - 2mn - 2n^2 >= 0
-- m^2 - nm - n^2 >= 0
-- This is a parabola in m opening upwards.
-- Therefore, the inequality is satisfied if m is *not* between
-- the roots of the parabola.
-- (n +- sqrt (n^2 + 4n^2))/2
-- (n +- n*sqrt 5)/2
-- (n*(1 +- sqrt 5))/2
-- n < n*(1 + sqrt 5)/2 <= m <= sqrt (bound + n^2)
--
--
-- In summary:
-- 0 < n <= (bound-1)/2
-- n*(1 + sqrt 5)/2 <= m <= sqrt (bound + n^2)
--
--------------------------------------------------------------------------------

--
-- Case 2: b = x
--
-- b = x <= bound
-- 2mn <= bound
-- m <= bound/2n
--
-- n < m <= bound/2n
-- n < bound/2n
-- n^2 < bound/2
-- n < sqrt (bound/2)
--
-- 2x >= y+z
-- 2b >= a
-- 4mn >= m^2 - n^2
-- m^2 - 4nm - n^2 >= 0
-- Parabola in m opening upwards.
-- Therefore, the inequality is satisfied if m is between the roots of parabola.
-- m = (4n +- sqrt (16n^2 + 4n^2))/2
-- m = (4n +- 2n*sqrt 5)/2
-- m = 2n +- n*sqrt 5
-- m = n*(2 +- sqrt 5)
-- m <= n*(2 + sqrt 5)    -- m > 0
--
-- In summary:
-- 0 < n < sqrt (bound/2)
-- n < m <= min (bound/2n) (n*(2 + sqrt 5))
--
--------------------------------------------------------------------------------

--
-- If a = x:
-- 0 < n <= (bound-1)/2
-- n*(1 + sqrt 5)/2 <= m <= sqrt (bound + n^2)
--
-- If b = x:
-- 0 < n < sqrt (bound/2)
-- n < m <= min (bound/2n) (n*(2 + sqrt 5))
--
--------------------------------------------------------------------------------

--
-- Translated to Haskell paying particular attention to rounding:
--
-- If a = x:
--
-- 1 <= n <= (bound-1) `quot` 2
--
-- n <= (bound-1) `quot` 2
-- Greatest integer less than (bound-1)/2 is (bound-1) `quot` 2 by definition.
--
-- ceiling (fromIntegral n * (1 + sqrt 5)/2) <= m <= floor (sqrt (fromIntegral (bound + n^2)))
--
-- ceiling @Float (fromIntegral n * (1 + sqrt 5)/2) <= m
-- If we calculate this by integer arithmetic we have
-- (n*(1 + floor (sqrt 5))) `quot` 2 = (n*3) `quot` 2
-- 3 is smaller than 1 + sqrt 5 by 7%.
-- ceiling x is that least integer greater than x.
--
-- m <= floor (sqrt (fromIntegral (bound + n^2)))
-- Expression inside sqrt is type conversion and exact.
-- If x is not a round number, floor x is the greatest integer less than x.
--
--
-- If b = x:
--
-- 1 <= n <= floor (sqrt (fromIntegral (bound `quot` 2)))
--
-- n < floor (sqrt (fromIntegral (bound `quot` 2)))
-- If bound/2 is a perfect square, then the conversion is exact.
-- If bound is even, then expression inside sqrt is an exact type conversion.
-- If bound is odd, then argument to sqrt will differ by 0.5.
-- However, after applying floor it will be the same.
-- The defining feature of this case is that
-- there exist an m such that b <= bound.
-- E.g. sqrt (114/2) ~ 7.55 and 2*7*8 = 112 < 114
-- The example illustrate the bound can be tight.
--
-- n+1 <= m <= floor (fromIntegral n*(2 + sqrt 5))
--
-- n+1 <= m <= min (floor (fromIntegral n*(2 + sqrt 5))) (bound `quot` (2*n))
--
-- m <= floor (fromIntegral n*(2 + sqrt 5))
-- To prevent rounding error associated with sqrt 5
--
-- m <= bound `quot` (2*n)
-- bound `quot` (2*n) is the greatest integer less than bound/(2n)
-- To prevent rounding error associated with sqrt 5
--
--------------------------------------------------------------------------------

--
-- The translated equations are:
--
-- Case A:
-- 1 <= n <= (bound-1) `quot` 2
-- ceiling (fromIntegral n * (1 + sqrt 5)/2) <= m <= floor (sqrt (fromIntegral (bound + n^2)))
--
-- Case B:
-- 1 <= n <= floor (sqrt (fromIntegral (bound `quot` 2)))
-- n+1 <= m <= min (floor (fromIntegral n*(2 + sqrt 5))) (bound `quot` (2*n))
--
--------------------------------------------------------------------------------


type PythagoreanTriple = (Int, Int, Int)

primitivePythagoreanPairs :: Int -> V.Vector (Int, Int)
-- Returns the two legs of all primitive Pythagorean triples that is valid.
-- See wall of text above.
primitivePythagoreanPairs bound =
    sort (V.concatMap goA (V.enumFromN 1 nA) V.++ V.concatMap goB (V.enumFromN 1 nB))
  where
    sort :: (Ord a, V.Unbox a) => V.Vector a -> V.Vector a
    sort v = runST $ do
        mv <- V.thaw v
        MV.sort mv
        V.unsafeFreeze mv
    nA :: Int
    nA = (bound-1) `quot` 2
    nB :: Int
    nB = floor (sqrt @Float (fromIntegral (bound `quot` 2)))

    isPrimitive n m = (even n || even m) && gcd n m == 1

    goA :: Int -> V.Vector (Int, Int)
    -- goA _ = V.empty
    goA n = (V.map toPair . V.filter (isPrimitive n)) (V.enumFromN m0 (mm - m0 + 1))
      where
        toPair m = (m2 - n2, 2*m*n)
          where
            m2 = square m
            n2 = square n
        m0 :: Int
        m0 = ceiling @Float (fromIntegral n * (1 + sqrt 5)/2)
        mm :: Int
        mm = floor @Float (sqrt (fromIntegral (bound + square n)))

    goB :: Int -> V.Vector (Int, Int)
    -- goB _ = V.empty
    goB n = (V.map toPair . V.filter (isPrimitive n)) (V.enumFromN m0 (mm - m0 + 1))
      where
        toPair m = (2*m*n, m2 - n2)
          where
            m2 = square m
            n2 = square n
        m0 :: Int
        m0 = n+1
        mm :: Int
        mm = min (floor @Float (fromIntegral n*(2 + sqrt 5))) (bound `quot` (2*n))


divisible :: Int -> Int -> Maybe Int
-- If a is divisible by b, then return Just (a `quot` b), else Nothing.
divisible a b = if r == 0 then Just q else Nothing
  where (q, r) = a `quotRem` b

scalePair :: Int -> (Int, Int) -> Maybe (Int, Int)
scalePair n (a, b) = divisible n a >>= (\k -> Just (k*a, k*b))

filterPairs :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int)
-- filterPairs n takes a vector of primitive Pythagorean pairs,
-- keep pairs where n is not divisible by fst pair,
-- and scale up the pair such that fst pair = n.
filterPairs n = V.mapMaybe (scalePair n) . V.takeWhile ((<= n) . fst)

countPairs :: V.Vector (Int, Int) -> Int
-- Given a Pythagorean pair (a, b),
-- count how many cuboid has x = a and y+z = b
-- subjected to x >= y >= z.
countPairs = V.foldl' go 0
  where
    go acc (a, b) = if b > a then acc + (b `quot` 2 - (b-a-1)) else acc + (b `quot` 2)

countCuboid :: Int -> V.Vector (Int, Int) -> Int
-- Given a vector of Pythagorean pairs,
-- count the number of distinct cuboids whose sides are of integral length,
-- longest side has length x,
-- and shortest path between two opposite corners are of integral length.
countCuboid x = countPairs . filterPairs x

totalSum :: Int -> V.Vector (Int, Int) -> Int
-- Given a vector of Pythagorean pairs,
-- count the number of distinct cuboids whose sides are of integral length,
-- longest side has length x,
-- and shortest path between two opposite corners are of integral length.
totalSum n v = sum $ map (`countCuboid` v) [1..n]

binarySearchF :: forall a b. (Integral a, Ord b) => b -> (a -> b) -> a
-- Binary search on monotonic functions.
-- Returns the largest x such that f x <= k.
binarySearchF k f = uncurry binarySearchF' bound
  where
    bound :: (a, a)
    bound = (0, genUpperBound 100)
      where
        genUpperBound b = if k <= f b then b else genUpperBound ((b+1)*2)
    binarySearchF' l r
      | l == r    = l
      | otherwise = case k `compare` y of
                      LT -> binarySearchF' l (m-1)
                      _  -> binarySearchF' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            y = f m
{-# INLINABLE binarySearchF #-}

--------------------------------------------------------------------------------

--
-- The above is a very slow approach where the vector of primitive pairs
-- are traversed multiple times to generate the pairs valid for a cuboid
-- whose longest side has length n. Each pair is tested to see
-- whether it can be scaled up to a cuboid with longest side of exactly
-- length x. This results in multiple traversals and divisions which
-- are in hindsight unnecessary.
--
-- Below is an approach where each of the primitive pair is visited
-- exactly once. All multiples of the pair and their associated cuboids
-- are tallied up before moving to next pair.
--
-- N.B. Stream fusion seemed to have fused everything.
-- This effectively runs in constant space.

scalarMul :: Int -> (Int, Int) -> (Int, Int)
scalarMul k (a, b) = (k*a, k*b)

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (a, b) (x, y) = (a+x, b+y)

primitivePythagoreanPairsFast :: Int -> V.Vector (Int, Int)
-- Returns the two legs of all primitive Pythagorean triples that is valid.
-- See wall of text above.
primitivePythagoreanPairsFast bound =
    V.concatMap goA (V.enumFromN 1 nA) V.++ V.concatMap goB (V.enumFromN 1 nB)
  where
    nA :: Int
    nA = (bound-1) `quot` 2
    nB :: Int
    nB = floor (sqrt @Float (fromIntegral (bound `quot` 2)))

    isPrimitive n m = (even n || even m) && gcd n m == 1

    goA :: Int -> V.Vector (Int, Int)
    -- goA _ = V.empty
    goA n = (V.map toPair . V.filter (isPrimitive n)) (V.enumFromN m0 (mm - m0 + 1))
      where
        toPair m = (m2 - n2, 2*m*n)
          where
            m2 = square m
            n2 = square n
        m0 :: Int
        m0 = ceiling @Float (fromIntegral n * (1 + sqrt 5)/2)
        mm :: Int
        mm = floor @Float (sqrt (fromIntegral (bound + square n)))

    goB :: Int -> V.Vector (Int, Int)
    -- goB _ = V.empty
    goB n = (V.map toPair . V.filter (isPrimitive n)) (V.enumFromN m0 (mm - m0 + 1))
      where
        toPair m = (2*m*n, m2 - n2)
          where
            m2 = square m
            n2 = square n
        m0 :: Int
        m0 = n+1
        mm :: Int
        mm = min (floor @Float (fromIntegral n*(2 + sqrt 5))) (bound `quot` (2*n))
{-# INLINE primitivePythagoreanPairsFast #-}

countPairFast :: Int -> (Int, Int) -> Int
-- Given a primitive Pythagorean pair (a, b) and a bound,
-- count how many cuboid has sides x = k*a and y+z = k*b
-- subjected to bound >= x >= y >= z.
countPairFast bound sp = countPairFast' 0 sp
  where
    countPairFast' acc p = if fst p <= bound
                           then countPairFast' (acc + count p) (p `addPairs` sp)
                           else acc
    count (a, b) = if b > a then b `quot` 2 - (b-a-1) else b `quot` 2

totalSumFast :: Int -> Int
-- Given a vector of primitive pairs,
-- count the number of distinct cuboids whose sides are of integral length,
-- the longest side has length equal or below n,
-- and shortest path between two opposite corners are of integral length.
totalSumFast n = V.sum $ V.map (countPairFast n) (primitivePythagoreanPairsFast n)

--------------------------------------------------------------------------------


answer :: Int
answer = binarySearchF 1000000 (`totalSum` primitivePythagoreanPairs 1000000)

profile :: Int
profile = binarySearchF 500000 (`totalSum` primitivePythagoreanPairs 500000)

extreme :: Int
extreme = binarySearchF (10^13) (`totalSum` primitivePythagoreanPairs (10^13))

answerFast :: Int
answerFast = binarySearchF 1000000 totalSumFast

extremeFast :: Int
extremeFast = binarySearchF (10^13) totalSumFast

main :: IO ()
main = print extremeFast