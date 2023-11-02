{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Euler.Helper
import Data.Ratio
import Data.List (find, foldl1', maximumBy)
import Data.Ord (comparing)

import Data.Maybe (fromJust)
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader


fI :: (Integral a, Num b) => a -> b
fI = fromIntegral


-- NOTE TO FUTURE SELF:
-- The representation of a fraction of the type (m * sqrt r + n) / d
-- is partitioned into two parts.
-- A base RootFrac type which does not prohibit mixing of roots and is unsafe,
-- and a RootFracComp type which is effectively a Reader Int RootFrac.
-- The construction of RootFracComp guarantees there is exactly one root in the computation
-- but the performance is suspect.
-- I do not believe there is a way to combine the performance of RootFrac
-- and the safety of RootFracComp.
--
-- Consider the following example:
-- (r + sqrt r)^2
-- = (r^2 + r) + 2r*sqrt r
-- The integral part of input is r = (sqrt r)^2.
-- By squaring we have generated an output whose integral part is r^2.
-- Since r = (sqrt r)^2, we can generate arbitrary powers of r by
-- taking powers of (sqrt r)^2.
-- This requires unbounded storage in the worse case.
-- Formally, despite Q[sqrt r] is a field for any natural r,
-- I suspect there is no representations of elements in Q[sqrt r]
-- that is both finite and can be kept in a "reduced" form before r is known.


-- Represents a fraction of the type (m * sqrt r + n) / d
data RootFrac =
    RootFrac { mul :: Integer
             , root :: Integer
             , numer :: Integer
             , denom :: Integer
             } deriving (Show, Eq)


simplify :: RootFrac -> RootFrac
simplify z = moveSign $ if g == 1 then z else RootFrac m' r n' d'
  where
    r = root z
    m = mul z
    n = numer z
    d = denom z
    g = foldl1' gcd [m, n, d]
    m' = m `quot` g
    n' = n `quot` g
    d' = d `quot` g
    moveSign x = if denom x > 0
                 then x
                 else RootFrac (-1*mul x) (root x) (-1*numer x) (-1*denom x)

instance Num RootFrac where
  -- (+) is unsafe.
  -- x + y takes the root of x and complete ignores the root of y.
  (+) x y = simplify $ RootFrac m' r n' l
      where
        r = root x
        l = lcm (denom x) (denom y)
        mx = mul x
        my = mul y
        nx = numer x
        ny = numer y
        dx = denom x
        dy = denom y
        lx = l `quot` dx
        ly = l `quot` dy
        m' = mx*lx + my*ly
        n' = nx*lx + ny*ly

  -- (*) is unsafe.
  -- (*) takes the radical in x and completely ignores the root in y.
  (*) x y = simplify $ RootFrac m' r n' d'
    where
      r = root x
      mx = mul x
      my = mul y
      nx = numer x
      ny = numer y
      m' = mx*ny + my*nx
      n' = mx*my*r + nx*ny
      d' = denom x * denom y

  -- If a * sqrt n > b then no change else negate
  abs a = if (fI (mul a) * sqrt (fI (root a))) > fI (numer a) then a else negate a

  signum z
    | mul z == 0 && numer z == 0 = 0
    | (fI (mul z) * sqrt (fI (root z))) > fI (numer z) = 1
    | otherwise = -1

  -- fromInteger is unsafe.
  -- fromInteger returns a RootFrac with a root of 0.
  -- The interaction of fromInteger with (+) is problematic.
  fromInteger a = RootFrac 0 0 (fI a) 1

  negate z = z { denom = -1*denom z }

-- d / (m * sqrt r + n) = (d * (m * sqrt r - n)) / (r*m^2 - n^2)
instance Fractional RootFrac where
  fromRational q = RootFrac 0 0 (fI (numerator q)) (fI (denominator q))
  recip (RootFrac m r n d) = RootFrac m' r n' d'
    where
      m' = d*m
      n' = -d*n
      d' = r*m^(2 :: Int) - n^(2 :: Int)

fromRootFrac :: RootFrac -> Double
fromRootFrac (RootFrac m r n d) = ((fI m)*sqrt (fI r) + fI n)/(fI d)


-- Represents a computation which returns a RootFrac.
data RootFracComp where
  RFCData :: Integer -> Integer -> Integer -> RootFracComp
  RFCFunc :: Reader Integer RootFrac -> RootFracComp

runRFC :: RootFracComp -> Reader Integer RootFrac
runRFC (RFCData m n d) = reader (\ r -> RootFrac m r n d)
runRFC (RFCFunc rz) = rz

evalRFC :: RootFracComp -> Integer -> RootFrac
evalRFC = runReader . runRFC

instance Show RootFracComp where
  show (RFCData m n d) = "RFCData " ++ show m ++ show n ++ show d
  show (RFCFunc _) = "RFCFunc"

instance Num RootFracComp where
  (+) (RFCData mx nx dx) (RFCData my ny dy) = RFCData m' n' l
    where
      l = lcm dx dy
      lx = l `quot` dx
      ly = l `quot` dy
      m' = mx*lx + my*ly
      n' = nx*lx + ny*ly
  (+) x y = RFCFunc $ reader (\ r -> evalRFC x r + evalRFC y r)

  (*) (RFCData mx nx dx) (RFCData my ny dy) =
      RFCFunc $ reader (\ r -> let mt = mx*ny + my*nx
                                   nt = mx*my*r + nx*ny
                                   dt = dx*dy
                                   g = foldl1' gcd [mt, nt, dt]
                                   m' = mt `quot` g
                                   n' = nt `quot` g
                                   d' = dt `quot` g
                                in RootFrac m' r n' d'
                       )
  (*) x y = RFCFunc $ reader (\ r -> evalRFC x r * evalRFC y r)

  abs = RFCFunc . fmap abs . runRFC

  signum = RFCFunc . fmap abs . runRFC

  fromInteger a = RFCData 0 (fI a) 1

  negate (RFCData m n d) = RFCData m n (-d)
  negate (RFCFunc rz) = RFCFunc $ negate <$> rz

-- d / (m * sqrt r + n) = (d * (m * sqrt r - n)) / (r*m^2 - n^2)
instance Fractional RootFracComp where
  fromRational q = RFCData 0 (fI $ numerator q) (fI $ denominator q)
  recip (RFCData m n d) =
      RFCFunc $ reader (\ r -> let m' = d*m
                                   n' = -d*n
                                   d' = r*m^(2 :: Int) - n^(2 :: Int)
                                in RootFrac m' r n' d'
                       )
  recip (RFCFunc rz) = RFCFunc $ recip <$> rz

sqrtRFC :: RootFracComp
-- Returns (sqrt n :: RootFrac) = (1*sqrt n + 0) / 1
sqrtRFC = RFCData 1 0 1

-- I couldn't figure out how to write the following function
-- in the language of Reader monad.
--
-- sqrtCFPair :: Reader Int [(Int, RootFrac)]
-- -- Calculates the continued fraction representation of square root of n
-- -- along with the intermediate states.
-- -- Returns a list whose tail is the repeating part of the representation.
-- sqrtCFPair = do
--     r <- ask
--     intPart <- mapReaderT (Identity . flip evalState sqrtRFC) step
--     return [intPart]
-- 
--   where
--     step :: ReaderT Int (State RootFracComp) (Int, RootFrac)
--     -- step returns the greatest integer smaller than s
--     -- and the difference between that integer and s.
--     step = ReaderT
--         (\ r -> state
--             (\ s -> let a :: Int
--                         a = floor $ fromRootFrac $ evalRFC s r
--                         b :: RootFracComp
--                         b = recip $ s - fI a
--                      in ((a, evalRFC b r), b)
--             )
--         )

--     loop :: ReaderT Int (State RootFracComp) (Int, RootFrac)
--          -> Reader Int [(Int, RootFrac)]
--     loop s
--       | nextState == seedState = []
--       | otherwise = nextStep : loop nextState
--       where
--         nextStep = step (recip s)
--         nextState = snd nextStep


sqrtCFPairInf :: Reader Integer [Integer]
-- Returns the continued fraction representation of square root of n.
sqrtCFPairInf = mapReaderT (Identity . flip evalState sqrtRFC) (sequence (repeat step))
  where
    step :: ReaderT Integer (State RootFracComp) Integer
    -- step returns the greatest integer smaller than s
    -- and the reciprocal of the difference between the integer and s.
    step = ReaderT
        (\ r -> state
            (\ s -> let a :: Integer
                        a = floor $ fromRootFrac $ evalRFC s r
                        b :: RootFracComp
                        b = recip $ s - fI a
                     in (a, b)
            )
        )

convergents :: [Integer] -> Reader Integer [(Integer, Integer)]
-- Given a continued fraction representation in a list, returns the convergents
-- of the continued fraction in a list.
convergents = pure . drop 2 . go (0, 1) (1, 0)
  where
    go sn2 sn1 [] = [sn2, sn1]
    go sn2@(hn2, kn2) sn1@(hn1, kn1) (a:as) = sn2 : go sn1 sn0 as
      where
        sn0 = (a*hn1 + hn2, a*kn1 + kn2)

pellSolution :: [(Integer, Integer)] -> Reader Integer (Integer, Integer)
pellSolution xs = do
    r <- ask
    let isSolution (h, k) = h^(2 :: Int) - r*k^(2 :: Int) == 1
    return $ fromJust (find isSolution xs)

inputList :: [Integer]
inputList = deleteOrd [2..1000] [ x*x | x <- [2..] ]

results :: [((Integer, Integer), Integer)]
results = map (toFst (runReader (sqrtCFPairInf >>= convergents >>= pellSolution))) inputList

main :: IO ()
main = print $ maximumBy (comparing (fst . fst)) results
