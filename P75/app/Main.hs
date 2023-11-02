{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Data.List (sort)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as V

import Control.Monad.State.Lazy

globalBound :: Int
globalBound = 1500000

square :: Int -> Int
square n = n^(2 :: Int)
{-# INLINE square #-}

type PythagoreanTriple = (Int, (Int, Int, Int))

primitivePythagoreanTriple :: Int -> [PythagoreanTriple]
-- Returns all Pythagorean triples with perimeters below a certain bound.
--
-- https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
-- a = m^2 - n^2
-- b = 2mn
-- c = m^2 + n^2
-- m > n > 0
-- m, n coprime and not both odd
--
-- a + b + c <= bound
-- m^2 - n^2 + 2mn + m^2 + n^2
-- = 2m^2 + 2mn
-- <= bound
-- 2m^2 + 2mn <= bound
-- 2m^2 + 2nm - bound <= 0
-- This is a parabola which opens upwards.
-- Therefore, the inequality is satisfied when m is between the two roots.
-- (-2n +- sqrt(4n^2 + 8*bound))/4 = m
-- (-n +- sqrt(n^2 + 2*bound))/2 = m
--
-- We also note that a is always odd and b is always even.
-- b is a multiple of two.
-- If a were to be even then m and n would either be both odd or both even.
-- This either contradicts m, n not both odd, or m, n coprime.
--
primitivePythagoreanTriple bound =
    [ (l, (a, b, c)) | n <- [1..bound],
                       let rootDet :: Float
                           rootDet = sqrt $ fromIntegral (square n + 2*bound)
                           -- For fear of rounding error, +- 1.
                           minBoundM = max n     ((-n - ceiling rootDet) `quot` 2 - 1)
                           maxBoundM = min bound ((-n + ceiling rootDet) `quot` 2 + 1),
                       m <- [minBoundM..maxBoundM],
                       even m || even n, -- DeMorgan's (m, n not both odd)
                       gcd m n == 1,
                       let m2 = square m
                           n2 = square n
                           a  = m2 - n2
                           b  = 2*m*n
                           c  = m2 + n2
                           l  = 2*m2 + b,
                       l <= bound
    ]

reducedPythagoreanTriple :: Int -> [(Int, (Int, Int, Int))]
-- Some primitive Pythagorean triples have the same perimeter,
-- e.g. (627, 364, 725) and (195, 748, 773), both have l == 1716.
-- We would like to get rid of that.
reducedPythagoreanTriple bound = prunePrimitive $ sort (primitivePythagoreanTriple bound)
  where
    prunePrimitive []             = []
    prunePrimitive [x]            = [x]
    prunePrimitive (x1:xs@(x2:_)) = if fst x1 == fst x2
                                    then prunePrimitive (dropWhile (\x -> fst x == fst x1) xs)
                                    else x1 : prunePrimitive xs

-- All reduced triples are singular in the sense of the question,
-- but not all singular triples are reduced.
-- A triple can still be singular even if it is a multiple of a reduced triple
-- as long as it is not a multiple of two different reduced triples.
-- E.g. (24, (6, 8, 10)) = 2*(12, (3, 4, 5)) is singular.
-- Counter e.g. 10*(12, (3, 4, 5)) == 4*(30, (5, 12, 13)) == 3*(40, (8, 15, 17))
--
-- We can reduce the problem into a problem on the list of perimeters.
-- Given a list of perimeters, consider the list of all multiples of those perimeters.
-- Which element in the new list is a multiple of only one perimeter i.e. singular?
-- e.g. [12, 30] yields a list containing all multiples of 12 and 30
-- less multiples of 60. lcm 12 30 = 60
--
-- To figure out which multiple of a reduced triple is singular,
-- we can use a technique borrowed from Sieve of Eratosthenes.
--
-- Create a priority queue with the multiples of perimeter of reduced triples as key,
-- and the circumference of reduced triple as value.
--
-- Repeatedly extract from the heap with delete-min
--
-- I don't know what I am doing.

type Perimeter = Int
type Queue = PQ.MinPQueue Perimeter (Perimeter, Int)

toQueue :: [PythagoreanTriple] -> Queue
toQueue = PQ.fromAscList . map (\t -> (fst t, (fst t, 1)))


genSingularTriples :: Int -> [(Perimeter, Int)] -> State Queue [(Perimeter, Int)]
genSingularTriples bound = genSingularTriples'
  where
    genSingularTriples' l = state (\q ->
      case PQ.getMin q of
        Nothing -> (l, q)
        Just (keyMin, pMin) ->
            if keyMin <= bound
            then let keyEqMin k _ = (k == keyMin)
                     (pairsWithMinKey, qn) = PQ.spanWithKey keyEqMin q
                     increment (p, (b, m)) = (p+b, (b, m+1))
                     newState = PQ.union (PQ.fromList (map increment pairsWithMinKey)) qn
                  in if length pairsWithMinKey > 1
                     then runState (genSingularTriples' l) newState
                     else runState (genSingularTriples' (pMin : l)) newState
            else (l, q)
                                  )


genSingularTriplesHeap :: Int -> Queue -> [(Perimeter, Int)]
genSingularTriplesHeap bound = genSingularTriplesHeap'
  where
    genSingularTriplesHeap' q =
        case PQ.getMin q of
          Nothing -> []
          Just (keyMin, pMin) ->
              let keyEqMin k _ = (k == keyMin)
                  (pairsWithMinKey, qn) = PQ.spanWithKey keyEqMin q
                  newState =
                      PQ.union
                      (PQ.fromList [ (p+b, (b, m+1)) | (p, (b, m)) <- pairsWithMinKey,
                                                       p+b <= bound ])
                      qn
               in if length pairsWithMinKey > 1
                  then genSingularTriplesHeap' newState
                  else pMin : genSingularTriplesHeap' newState


-- y i do dis?
-- y me no use map and filter repetitions?
-- y heap and mess?
-- idiot
--
genSingularTriplesMap :: Int -> IM.IntMap Int
genSingularTriplesMap bound =
    foldr (uncurry $ IM.insertWith (+)) IM.empty
          [ (x, 1) | per <- map fst (primitivePythagoreanTriple bound),
                     x <- takeWhile (<= bound) [per, 2*per..] ]


-- The question is bounded with no need for laziness.
-- So no laziness and vector it is.
-- Also stop generating after minBoundM > maxBoundM.
--
primitivePythagoreanTripleVec :: Int -> V.Vector PythagoreanTriple
primitivePythagoreanTripleVec bound =
    V.concat $ go [1..bound]
  where
    go [] = []
    go (n:ns) = if minBoundM <= maxBoundM then vec : go ns else []
      where
        rootDet :: Int
        rootDet = ceiling $ sqrt @Float (fromIntegral (square n + 2*bound))
        -- For fear of rounding error, +- 1.
        minBoundM = max n     ((-n - rootDet) `quot` 2 - 1)
        maxBoundM = min bound ((-n + rootDet) `quot` 2 + 1)
        vecM = V.enumFromN minBoundM (maxBoundM - minBoundM + 1)
        isGood m = (even m || even n) && gcd m n == 1
        vec = (V.filter ((<= bound) . fst) . V.map toPTriple . V.filter isGood) vecM
          where
            toPTriple m = (l, (a, b, c))
              where
                m2 = square m
                n2 = square n
                a  = m2 - n2
                b  = 2*m*n
                c  = m2 + n2
                l  = 2*m2 + b

genSingularTriplesVec :: Int -> V.Vector Int
genSingularTriplesVec bound =
    V.accumulate (+) (V.replicate (bound+1) 0) perimeter
  where
    perimeter :: V.Vector (Int, Int)
    perimeter = V.concatMap (genMultiples . fst) (primitivePythagoreanTripleVec bound)
    genMultiples per = V.map (\x -> (x, 1)) $ V.enumFromThenTo per (2*per) bound


primitivePythagoreanTripleFast :: Int -> V.Vector Int
primitivePythagoreanTripleFast bound =
    V.concat $ go [1..bound]
  where
    go [] = []
    go (n:ns) = if minBoundM <= maxBoundM then vec : go ns else []
      where
        rootDet :: Int
        rootDet = ceiling $ sqrt @Float (fromIntegral (square n + 2*bound))
        -- For fear of rounding error, +- 1.
        minBoundM = max n     ((-n - rootDet) `quot` 2 - 1)
        maxBoundM = min bound ((-n + rootDet) `quot` 2 + 1)
        vecM = V.enumFromN minBoundM (maxBoundM - minBoundM + 1)
        isGood m = (even m || even n) && gcd m n == 1
        vec = (V.filter (<= bound) . V.map toPerimeter . V.filter isGood) vecM
          where
            toPerimeter m = 2*square m + 2*m*n

genSingularTriplesFast :: Int -> V.Vector Int
genSingularTriplesFast bound =
    V.accumulate (+) (V.replicate (bound+1) 0) perimeter
  where
    perimeter :: V.Vector (Int, Int)
    perimeter = V.concatMap genMultiples (primitivePythagoreanTripleFast bound)
    genMultiples :: Int -> V.Vector (Int, Int)
    genMultiples per = V.map (\x -> (x, 1)) $ V.enumFromThenTo per (2*per) bound


testBound :: Int
testBound = 150000

main :: IO ()
main = print $ (V.length . V.filter (==1) . genSingularTriplesFast) globalBound
