module Main (main) where

import Data.Either (fromRight)
import Data.List (foldl')
import Data.Ord (comparing)

import Data.IntSet qualified as IS

import Data.Vector.Unboxed qualified as U

import Control.Monad.Except


-- The intent of this program was to rewrite the solution in continuation passing
-- style in an attempt to make the solution more clear.
-- However, after much exploration it would seem writing the program in CPS would
-- in fact make the program even more unclear.
-- Hence, the program is not written in CPS but the name stuck.

data SearchState = SearchState { index           :: {-# UNPACK #-} !Int
                               , total           :: {-# UNPACK #-} !Int
                               , outerMin        :: {-# UNPACK #-} !Int
                               , ring            :: {-# UNPACK #-} !(U.Vector Int)
                               , availableNumber :: !IS.IntSet
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


-- Currently isomorphic with Maybe SearchState
data SearchResult = NoFill
                  | Completed SearchState
                 deriving (Show, Eq)

instance Ord SearchResult where
  Completed x `compare` Completed y = compare x y
  Completed _ `compare` NoFill      = GT
  NoFill      `compare` Completed _ = LT
  NoFill      `compare` NoFill      = EQ

instance Semigroup SearchResult where
  NoFill <> x = x
  x <> NoFill = x
  Completed x <> Completed y = case compare x y of
    GT -> Completed x
    LT -> Completed y
    EQ -> Completed x

instance Monoid SearchResult where
  mempty = NoFill




updateSearchState :: Int -> SearchState -> SearchState
updateSearchState c s =
    let i = index s
        an = availableNumber s
        r = ring s
     in s { index = i+1
          , availableNumber = IS.delete c an
          , ring = r U.// [(i, c)]
          }
{-# INLINE updateSearchState #-}

choose :: Int -> SearchState -> Either SearchResult SearchState
choose c s = pure (updateSearchState c s)

chooseWithCheck :: Int -> SearchState -> Either SearchResult SearchState
chooseWithCheck c s =
    if c `IS.member` availableNumber s
    then choose c s
    else throwError NoFill

check :: SearchState -> Either SearchResult SearchState
check s = chooseWithCheck (total s - U.sum (U.slice (i-2) 2 (ring s))) s
  where
    i = index s

setTotal :: SearchState -> Either SearchResult SearchState
setTotal s = pure $ s { total = U.sum . U.take 3 . ring $ s }

initState :: SearchState
-- 10 from 10 elements on a magic 5-gon
initState = SearchState { index = 1
                        , total = 0
                        , outerMin = 0
                        , ring = U.fromList (10 : replicate 9 0)
                        , availableNumber = IS.fromAscList [1..9]
                        }


step :: Int -> SearchState -> Either SearchResult SearchState
-- We choose an element on the outer ring.
-- The corresponding element in the inner ring is fixed.
-- If the chosen element is less than the current outerMin,
-- return Left NoFill as the solutions generated from that branch of the search
-- tree are useless.
step c s = if c >= outerMin s
           then choose c s >>= check
           else Left NoFill
{-# INLINE step #-}

loop :: SearchState -> SearchResult
-- What is the type of loop?
-- It takes a SearchState
-- It should return a SearchResult
-- Then why Either?
loop s = IS.foldl' dispatch NoFill (snd . IS.split (outerMin s - 1) . availableNumber $ s)
--   | index s == 9 = either id onExit (chooseWithCheck (total s - r U.! 1 - r U.! 8) s)
--   | otherwise    = IS.foldl' go NoFill (snd . IS.split (outerMin s - 1) . availableNumber $ s)
  where
    dispatch :: SearchResult -> Int -> SearchResult
    dispatch acc c = if index s < 9
                     then go acc c
                     else either id onExit (chooseWithCheck (total s - r U.! 1 - r U.! 8) s)
    {-# INLINE dispatch #-}

    go :: SearchResult -> Int -> SearchResult
    go acc c = acc <> branch
      where
        branch :: SearchResult
        branch = either id loop (step c (updateOuterMin acc))

    updateOuterMin :: SearchResult -> SearchState
    updateOuterMin NoFill = s
    updateOuterMin (Completed f) = s { outerMin = outerMin f }
    {-# INLINE updateOuterMin #-}

    r :: U.Vector Int
    r = ring s

    onExit :: SearchState -> SearchResult
    onExit f = Completed $ f { outerMin = U.minimum (outerRing f) }
    {-# INLINE onExit #-}

fill :: SearchState -> SearchResult
fill s = foldl'
            go
            NoFill
            [ initialChoices a b | a <- [1..9], b <- [1..a-1] ++ [a+1..9] ]
  where
    initialChoices :: Int -> Int -> SearchState
    initialChoices a b = fromRight (error "fill: incorrect initialChoices") $
        choose a s >>= choose b >>= setTotal

    go :: SearchResult -> SearchState -> SearchResult
    go acc x = acc <> loop (modifyOuterMin acc x)

    modifyOuterMin :: SearchResult -> SearchState -> SearchState
    modifyOuterMin NoFill x = x
    modifyOuterMin (Completed f) x = x { outerMin = outerMin f }

postprocess :: SearchResult -> SearchState
postprocess (Completed f) = f
postprocess NoFill = bomb
  where
    bomb = errorWithoutStackTrace "postprocess: fill failed"


main :: IO ()
-- main = print "hello"
main = print $ value $ postprocess $ fill initState
