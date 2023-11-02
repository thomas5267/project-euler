module Main (main) where

import Data.Either (fromLeft)
import Data.Foldable (asum)
import Data.Ord (comparing)

import Data.IntSet qualified as IS

import Data.Vector.Unboxed qualified as U

import Control.Monad.Except
import Control.Monad.State.Strict


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


type Search = ExceptT SearchResult (State SearchState)
-- This program implements a depth-first search by abusing ExceptT and the
-- Alternative instance of ExceptT e.
-- The end result of DFS in encoded in the error part of ExceptT e.
-- ExceptT e m a is a monad m a extended with early exit support by throwing an
-- error.
-- The early exit function is used to implement the backtracking in DFS.
-- If DFS fails and backtracking is required, ExceptT is used to throw a
-- NoFill error back to the caller.
-- If DFS suceeded in producing a completed magic 5-gon, ExceptT still throws
-- an "error" to return the completed state to the caller.
-- Under *no* circumstances this implementation of DFS returns a plain value.
--
-- The "errors" from different branches of the search tree are then combined
-- with the Alternative instance.
-- The Alternative instance of ExceptT e m a acts like a parser.
-- Let ma, mb :: ExceptT e m a.
-- ma <|> mb does the following.
-- ma returns a value: ma <|> mb = ma
-- ma does not return a value but mb does: ma <|> mb = mb
-- Both ma and mb throws and error: Combine the errors using the Monoid
-- instance on e and throws the combined error.
-- The Monoid instance of SearchResult combines two "errors" by taking the
-- better result (c.f. Max newtype on Int).
-- As the result of DFS is encoded in the error part of ExceptT, the DFS
-- cannot return a plain value lest the Alternative instance cannot be abused
-- to produce the result.
--
-- The drawback of this method is that the control flow must be encoded in an
-- inconvenient way and mingled with the program logic.
-- This is because a single non-error return kills the entire Alternative chain.

updateSearchState :: Int -> SearchState -> SearchState
updateSearchState c s =
    let i = index s
        an = availableNumber s
        r = ring s
     in s { index = i+1
          , availableNumber = IS.delete c an
          , ring = r U.// [(i, c)]
          }

choose :: Int -> Search ()
choose c = do
    s <- get
    when (index s == 10) $ error (show (index s, c))
    modify' (updateSearchState c)
{-# INLINE choose #-}

chooseWithCheck :: Int -> Search ()
chooseWithCheck c = do
    an <- gets availableNumber
    if c `IS.member` an
    then choose c
    else throwError NoFill

check :: Search ()
check = do
    s <- get
    let i = index s
    chooseWithCheck (total s - U.sum (U.slice (i-2) 2 (ring s)))

setTotal :: Search ()
setTotal = modify' (\ s -> s { total = U.sum . U.take 3 . ring $ s })

continue :: SearchState -> SearchResult -> Search ()
-- continue is used in conjunction with catchError.
-- If the "error" is a completed state, revert state to save, update outerMin,
-- and propagate the error.
-- Otherwise just revert the state and propagate the error.
continue save e = do
    case e of
        (Completed s) -> put $ save { outerMin = U.minimum $ outerRing s }
        _             -> pure ()
    put save
    throwError e

fill :: Search ()
-- asum is being abused (see above).
fill = asum [ go (a, b) | a <- [1..9], b <- [1..a-1] ++ [a+1..9] ]
  where
    go (a, b) = do
        save <- get
        (choose a >> choose b >> setTotal >> loop) `catchError` continue save


loop :: Search ()
loop = do
    save <- get
    let an = availableNumber save
    if index save /= 9 -- Final triple is different
    then asum $ map go (IS.toList (splitGT (outerMin save) an))
    else do
        let r = ring save
        chooseWithCheck (total save - r U.! 1 - r U.! 8)
        end <- get
        throwError $ Completed end
  where
    splitGT k = snd . IS.split k
    go c = do
        save <- get
        (choose c >> check >> loop) `catchError` continue save


postprocess :: Either SearchResult () -> SearchState
postprocess e = case fromLeft bomb e of
    Completed s -> s
    NoFill      -> bomb
  where
    bomb = errorWithoutStackTrace "postprocess: fill failed"

initState :: SearchState
-- 10 from 10 elements on a magic 5-gon
initState = SearchState { index = 1
                        , total = 0
                        , outerMin = 0
                        , availableNumber = IS.fromAscList [1..9]
                        , ring = U.fromList (10 : replicate 9 0)
                        }

main :: IO ()
main = print $ value $ postprocess $ evalState (runExceptT fill) initState
