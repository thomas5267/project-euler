module Distributions where

import Data.IntMap qualified as IM
import Data.Map qualified as M

import System.Random

import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad.State

data DistInt = DistInt
    { unboxDistInt :: IM.IntMap Integer
    , sumOfDistInt :: [IM.IntMap Integer]
    } deriving (Show, Eq, Ord)
-- DistInt is a distribution whose outcomes are Int
-- and the (non-normalised) probabilities are Integer.

simpleDist :: IM.IntMap Integer -> DistInt
simpleDist m = DistInt { unboxDistInt = m, sumOfDistInt = [m] }

addDist :: DistInt -> DistInt -> DistInt
addDist a b =
    DistInt
        { unboxDistInt = IM.fromList $
            liftA2 go (IM.toList (unboxDistInt a)) (IM.toList (unboxDistInt b))
        , sumOfDistInt = sumOfDistInt a ++ sumOfDistInt b
        }
  where
    go :: (Num a) => (Int, a) -> (Int, a) -> (Int, a)
    go (px, x) (py, y) = (px + py, x * y)


instance Semigroup DistInt where
  a <> b = addDist a b

instance Monoid DistInt where
  mempty = simpleDist delta
    where
      delta :: IM.IntMap Integer
      delta = IM.singleton 0 1


weight :: DistInt -> Integer
-- Return the normalisation factor of the distribution.
weight = sum . unboxDistInt

die :: Int -> DistInt
-- Returns the distribution of a n-sided die.
die n = simpleDist dieIntMap
  where
    dieIntMap :: IM.IntMap Integer
    dieIntMap = IM.fromAscList [ (k, 1) | k <- [1..n] ]


cumulative :: IM.IntMap Integer -> IM.IntMap Integer
-- Returns the cumulative distribution function in the form of a IntMap.
cumulative = snd . IM.mapAccum go 0
  where
    go a b = (s, s)
      where
        s = a + b

invertCumulative :: IM.IntMap Integer -> M.Map Integer Int
-- Inverts the cumulative distribution function,
-- yielding a map whose keys are Integer and the values are Int.
-- Useful in the function sample.
invertCumulative = M.fromList . map flipPair . IM.toList
-- Should work fine with M.fromAscList too but just to be safe.
  where
    flipPair (a, b) = (b, a)

sample :: DistInt -> State StdGen Int
sample = fmap sum . traverse sampleIntMap . sumOfDistInt
  where
    sampleIntMap :: IM.IntMap Integer -> State StdGen Int
    sampleIntMap m =
        fmap
            (snd . fromJust . flip M.lookupGE invertedCumulativeMap)
            (state $ uniformR (1, sum m))
      where
        invertedCumulativeMap = (invertCumulative . cumulative) m


