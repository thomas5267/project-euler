{-# LANGUAGE ScopedTypeVariables #-}
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


-- Represents a fraction of the type (m * sqrt r + n) / d
-- using polynomials of the form (ax^2 + bx + c) / d.
data RootFrac =
    RootFrac { coeff2 :: Integer
             , coeff1 :: Integer
             , coeff0 :: Integer
             , coeffD :: Integer
             } deriving (Show, Eq)

degree :: RootFrac -> Int
degree z
  | coeff2 z /= 0 = 2
  | coeff1 z /= 0 = 1
  | otherwise     = 0

monicRootFrac :: Int -> Integer -> Integer -> RootFrac
monicRootFrac n c d = case n of
  2 -> RootFrac c 0 0 d
  1 -> RootFrac 0 c 0 d
  0 -> RootFrac 0 0 c d
  _ -> errorWithoutStackTrace "monicRootFrac: degree too large"

euclideanDivision :: RootFrac -> RootFrac -> (RootFrac, RootFrac)
euclideanDivision x y = go (fI 0, x)
  where
    degY = degree y
    lcY = case degY of
      2 -> (coeff2 y, coeffD y)
      1 -> (coeff1 y, coeffD y)
      _ -> (coeff0 y, coeffD y)
    go (q, r)
      | degR >= degY = go (q + s, r - s*y)
      | otherwise = (q, r)
      where
        degR = degree r
        lcR = case degR of
          2 -> (coeff2 r, coeffD r)
          1 -> (coeff1 r, coeffD r)
          _ -> (coeff0 r, coeffD r)
        s = monicRootFrac (degR - degY) (m `quot` g) (n `quot` g)
          where
            (m, n) = (fst lcR * snd lcY, snd lcR * fst lcY)
            g = gcd m n




instance Num RootFrac where
  (+) x y = RootFrac a' b' c' l
      where
        dx = coeffD x
        dy = coeffD y
        l = lcm dx dy
        mx = l `quot` dx
        my = l `quot` dy
        a' = mx*coeff2 x + my*coeff2 y
        b' = mx*coeff1 x + my*coeff1 y
        c' = mx*coeff0 x + my*coeff0 y

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

--   -- If a * sqrt n > b then no change else negate
--   abs a = if (fI (mul a) * sqrt (fI (root a))) > fI (numer a) then a else negate a
-- 
--   signum z
--     | mul z == 0 && numer z == 0 = 0
--     | (fI (mul z) * sqrt (fI (root z))) > fI (numer z) = 1
--     | otherwise = -1

  fromInteger a = RootFrac 0 0 a 1

  negate z = z { coeffD = -1*coeffD z }

instance Fractional RootFrac where
  fromRational q = RootFrac 0 0 (fI (numerator q)) (fI (denominator q))
  recip (RootFrac a b c d) = RootFrac m' r n' d'
    where
      m' = d*m
      n' = -d*n
      d' = r*m^(2 :: Int) - n^(2 :: Int)

-- fromRootFrac :: RootFrac -> Double
-- fromRootFrac (RootFrac m r n d) = ((fI m)*sqrt (fI r) + fI n)/(fI d)
-- 
-- 
-- -- Represents a computation which returns a RootFrac.
-- data RootFracComp where
--   RFCData :: Integer -> Integer -> Integer -> RootFracComp
--   RFCFunc :: Reader Integer RootFrac -> RootFracComp
-- 
-- runRFC :: RootFracComp -> Reader Integer RootFrac
-- runRFC (RFCData m n d) = reader (\ r -> RootFrac m r n d)
-- runRFC (RFCFunc rz) = rz
-- 
-- evalRFC :: RootFracComp -> Integer -> RootFrac
-- evalRFC = runReader . runRFC
-- 
-- instance Show RootFracComp where
--   show (RFCData m n d) = "RFCData " ++ show m ++ show n ++ show d
--   show (RFCFunc _) = "RFCFunc"
-- 
-- instance Num RootFracComp where
--   (+) (RFCData mx nx dx) (RFCData my ny dy) = RFCData m' n' l
--     where
--       l = lcm dx dy
--       lx = l `quot` dx
--       ly = l `quot` dy
--       m' = mx*lx + my*ly
--       n' = nx*lx + ny*ly
--   (+) x y = RFCFunc $ reader (\ r -> evalRFC x r + evalRFC y r)
-- 
--   (*) (RFCData mx nx dx) (RFCData my ny dy) =
--       RFCFunc $ reader (\ r -> let mt = mx*ny + my*nx
--                                    nt = mx*my*r + nx*ny
--                                    dt = dx*dy
--                                    g = foldl1' gcd [mt, nt, dt]
--                                    m' = mt `quot` g
--                                    n' = nt `quot` g
--                                    d' = dt `quot` g
--                                 in RootFrac m' r n' d'
--                        )
--   (*) x y = RFCFunc $ reader (\ r -> evalRFC x r * evalRFC y r)
-- 
--   abs = RFCFunc . fmap abs . runRFC
-- 
--   signum = RFCFunc . fmap abs . runRFC
-- 
--   fromInteger a = RFCData 0 (fI a) 1
-- 
--   negate (RFCData m n d) = RFCData m n (-d)
--   negate (RFCFunc rz) = RFCFunc $ negate <$> rz
-- 
-- -- d / (m * sqrt r + n) = (d * (m * sqrt r - n)) / (r*m^2 - n^2)
-- instance Fractional RootFracComp where
--   fromRational q = RFCData 0 (fI $ numerator q) (fI $ denominator q)
--   recip (RFCData m n d) =
--       RFCFunc $ reader (\ r -> let m' = d*m
--                                    n' = -d*n
--                                    d' = r*m^(2 :: Int) - n^(2 :: Int)
--                                 in RootFrac m' r n' d'
--                        )
--   recip (RFCFunc rz) = RFCFunc $ recip <$> rz
-- 
-- sqrtRFC :: RootFracComp
-- -- Returns (sqrt n :: RootFrac) = (1*sqrt n + 0) / 1
-- sqrtRFC = RFCData 1 0 1
-- 
-- -- I couldn't figure out how to write the following function
-- -- in the language of Reader monad.
-- --
-- -- sqrtCFPair :: Reader Int [(Int, RootFrac)]
-- -- -- Calculates the continued fraction representation of square root of n
-- -- -- along with the intermediate states.
-- -- -- Returns a list whose tail is the repeating part of the representation.
-- -- sqrtCFPair = do
-- --     r <- ask
-- --     intPart <- mapReaderT (Identity . flip evalState sqrtRFC) step
-- --     return [intPart]
-- -- 
-- --   where
-- --     step :: ReaderT Int (State RootFracComp) (Int, RootFrac)
-- --     -- step returns the greatest integer smaller than s
-- --     -- and the difference between that integer and s.
-- --     step = ReaderT
-- --         (\ r -> state
-- --             (\ s -> let a :: Int
-- --                         a = floor $ fromRootFrac $ evalRFC s r
-- --                         b :: RootFracComp
-- --                         b = recip $ s - fI a
-- --                      in ((a, evalRFC b r), b)
-- --             )
-- --         )
-- 
-- --     loop :: ReaderT Int (State RootFracComp) (Int, RootFrac)
-- --          -> Reader Int [(Int, RootFrac)]
-- --     loop s
-- --       | nextState == seedState = []
-- --       | otherwise = nextStep : loop nextState
-- --       where
-- --         nextStep = step (recip s)
-- --         nextState = snd nextStep
-- 
-- 
-- sqrtCFPairInf :: Reader Integer [Integer]
-- -- Returns the continued fraction representation of square root of n.
-- sqrtCFPairInf = mapReaderT (Identity . flip evalState sqrtRFC) (sequence (repeat step))
--   where
--     step :: ReaderT Integer (State RootFracComp) Integer
--     -- step returns the greatest integer smaller than s
--     -- and the reciprocal of the difference between the integer and s.
--     step = ReaderT
--         (\ r -> state
--             (\ s -> let a :: Integer
--                         a = floor $ fromRootFrac $ evalRFC s r
--                         b :: RootFracComp
--                         b = recip $ s - fI a
--                      in (a, b)
--             )
--         )

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

-- results :: [((Integer, Integer), Integer)]
-- results = map (toFst (runReader (sqrtCFPairInf >>= convergents >>= pellSolution))) inputList

main :: IO ()
main = print "hello"
-- main = print $ maximumBy (comparing (fst . fst)) results
