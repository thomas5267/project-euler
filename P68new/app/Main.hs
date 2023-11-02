module Main (main) where

import Data.Foldable
import Data.Ord (comparing)

import Data.IntSet qualified as IS

import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU



data SearchState = SearchState { index           :: {-# UNPACK #-} !Int
                               , total           :: {-# UNPACK #-} !Int
                               , outerMin        :: {-# UNPACK #-} !Int
                               , availableNumber :: {-# UNPACK #-} !IS.IntSet
                               , ring            :: {-# UNPACK #-} !(U.Vector Int)
                               } deriving (Show, Eq)

outerRingIndex :: U.Vector Int
outerRingIndex = U.fromList [0,3,5,7,9]

outerRing :: SearchState -> U.Vector Int
outerRing s = U.backpermute (ring s) outerRingIndex

innerRingIndex :: U.Vector Int
innerRingIndex = U.fromList [1,2,4,6,8]

innerRing :: SearchState -> U.Vector Int
innerRing s = U.backpermute (ring s) innerRingIndex

value :: SearchState -> Int
-- value returns the string concatenation of the ring as specified in the
-- problem. It is returned as an Int instead of a string for performance
-- reasons.
value = U.foldl' go 0 . tripleUp . rotateRing . ring
  where
    rotateRing v = case U.minIndex (U.backpermute v outerRingIndex) of
        0 -> v
        1 -> U.backpermute v (U.fromList [3,2,4,5,6,7,8,9,1,0])
        2 -> U.backpermute v (U.fromList [5,4,6,7,8,9,1,0,2,3])
        3 -> U.backpermute v (U.fromList [7,6,8,9,1,0,2,3,4,5])
        4 -> U.backpermute v (U.fromList [9,8,1,0,2,3,4,5,6,7])

    tripleUp v = U.backpermute v (U.fromList [0,1,2,3,2,4,5,4,6,7,6,8,9,8,1])

    go acc 10 = acc * 100 + 10
    go acc x  = acc * 10 + x


whenEQ :: (a -> a -> Ordering)
       -> (a -> a -> Ordering)
       -> (a -> a -> Ordering)
-- Combines two comparision functions.
-- When the first comparision function returns EQ,
-- use the second comparision function to tiebreak.
whenEQ fstCmp sndCmp a b =
    case fstCmp a b of
      EQ -> sndCmp a b
      x  -> x
{-# INLINE whenEQ #-}

instance Ord SearchState where
  -- SearchState is ordered by its closeness to the required solution.
  -- The required solution has all elements filled.
  -- The larger the index of a state, the closer it is to being filled.
  -- Therefore, a state with a larger index is larger than a state with a
  -- smaller index.
  -- If two states have the same index, usually in the case where both states
  -- complete, we compare the smallest element on the outer ring.
  -- The larger than smallest element, the better is the solution.
  -- Finally, if two states have the same index and the smallest element on
  -- outer ring, we make the expensive call to value and compare on the
  -- resultant integer.
  -- The larger the integer, the better the state.
  compare = comparing index
                `whenEQ` (comparing outerMin `whenEQ` comparing value)

instance Semigroup SearchState where
  x <> y = case compare x y of
      LT -> y
      _  -> x


newtype SearchResult = SearchResult { unSearchResult :: SearchState
                                    } deriving (Show, Eq)
-- A compelted SearchState is a SearchResult

instance Ord SearchResult where
  compare = comparing (outerMin . unSearchResult)
      `whenEQ` comparing (value . unSearchResult)

instance Semigroup SearchResult where
  x <> y = case compare x y of
      LT -> y
      _  -> x


type Search = SearchState -> Maybe SearchState

updateSearchState :: Int -> SearchState -> SearchState
updateSearchState c s = s { index = i+1
                          , availableNumber = IS.delete c an
                          , ring = updateRing r
                          }
  where
    i = index s
    an = availableNumber s
    r = ring s
    updateRing = U.modify (\ mu -> MU.write mu i c)


updateOuterMin :: SearchState -> Maybe SearchResult -> SearchState
-- Updates the outerMin of s when given a known good solution to prune the DFS
updateOuterMin s Nothing = s
updateOuterMin s (Just (SearchResult sn)) = s { outerMin = U.minimum (outerRing sn) }

choose :: Int -> Search
choose c = pure . updateSearchState c
{-# INLINE choose #-}

chooseWithCheck :: Int -> Search
chooseWithCheck c s =
    if c `IS.member` availableNumber s
    then choose c s
    else Nothing

deduce :: Search
deduce s = chooseWithCheck (total s - U.sum (U.slice (i-2) 2 (ring s))) s
  where
    i = index s

setTotal :: Search
setTotal s = Just $ s { total = U.sum . U.take 3 . ring $ s }


-- This is just DFS with pruning
fill :: SearchState -> Maybe SearchResult
fill s = foldl' (<>) mempty [ go (a, b) | a <- [1..9], b <- [1..a-1] ++ [a+1..9] ]
  where
    go (a, b) = choose a s >>= choose b >>= setTotal >>= loop

loop :: SearchState -> Maybe SearchResult
loop s = if index s /= 9 -- Final triple is different
         then IS.foldl' go Nothing (splitGT (outerMin s) (availableNumber s))
         else SearchResult <$> chooseWithCheck (total s - r U.! 1 - r U.! 8) s
  where
    r = ring s
    splitGT k = snd . IS.split k
    go acc c = acc <> (choose c (updateOuterMin s acc) >>= deduce >>= loop)


initState :: SearchState
-- 10 from 10 elements on a magic 5-gon
initState = SearchState { index = 1
                        , total = 0
                        , outerMin = 0
                        , availableNumber = IS.fromAscList [1..9]
                        , ring = U.fromList (10 : replicate 9 0)
                        }

main :: IO ()
main = print $ maybe 0 (value . unSearchResult) $ fill initState
