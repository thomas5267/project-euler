module Main (main) where

import Euler.Helper

import Data.Bifunctor

import Data.MemoTrie
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU

import Data.Numbers.Primes

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe


primeFactorsAList :: Int -> [(Int, Int)]
primeFactorsAList n = toAssocList $ primeFactors n

-- primeFactorsAList :: Int -> [(Int, Int)]
-- primeFactorsAList n = tail $ go (0, 0) (primeFactors n)
--   where
--     go :: (Int, Int) -> [Int] -> [(Int, Int)]
--     go acc []     = [acc]
--     go (a, c) (x:xs)
--       | a == x    = go (a, c+1) xs
--       | otherwise = (a, c) : go (x, 1) xs

sumDivisorsPP :: Int -> Int -> Int
-- Returns the sum of divisors of prime powers
sumDivisorsPP = memo2 slow_sumDivisorsPP
  where slow_sumDivisorsPP p k = (p^(k+1) - 1) `quot` (p-1)

sumDivisors :: Int -> Int
-- We note that sum of divisors is a multiplicative function, i.e.
-- sumDivisors (i*j) == sumDivisors i * sumDivisors j if i, j coprime.
-- Therefore, factorise n into prime powers,
-- then use the identity to compute sumDivisors n.
sumDivisors = memo $ product . map (uncurry sumDivisorsPP) . primeFactorsAList

sumProperDivisors :: Int -> Int
sumProperDivisors n = sumDivisors n - n

amicableStep :: Int -> Int
amicableStep = sumProperDivisors


-- BUG: 1336 enters into the 1210 -> 1184 -> 1210 loop
amicableChain :: Int -> [Int]
amicableChain n = takeWhile (\ k -> k /= 0 && k /= n) (tail $ iterate amicableStep n)


hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

throwNothing :: (Applicative m) => MaybeT m b
throwNothing = MaybeT $ pure Nothing

catchNothing :: (Monad m) => MaybeT m b -> MaybeT m b -> MaybeT m b
catchNothing mx my = MaybeT $ do
    x <- runMaybeT mx
    case x of
      Nothing -> runMaybeT my
      Just _  -> runMaybeT mx

-- floydCycleFinding :: (Eq a) => [a] -> Maybe Int
-- -- Returns the length of the cycle in the list.
-- floydCycleFinding xs = do
--     let fastXs = dropEveryOther xs
--     multipleCycleLength <- (+1) <$> firstEqual (tail xs) (tail fastXs)
--     cycleStartIndex <- firstEqual xs (drop multipleCycleLength xs)
--     (+1) <$> elemIndex (xs !! cycleStartIndex) (drop (cycleStartIndex + 1) xs)
--   where
--     -- firstEqual find the index i such that (as !! i == bs !! i).
--     -- If the index is found, return Just i.
--     -- Otherwise, return Nothing.
--     firstEqual as bs = elemIndex True $ zipWith (==) as bs

data SearchState = SearchState { index :: {-# UNPACK #-} !Int
                               , searchVector :: {-# UNPACK #-} !(U.Vector Int)
                               } deriving (Show, Eq)

-- We use Int to encode the following.
-- l >=  1, l = length of cycle
-- l ==  0, length of cycle waiting to be determined
-- l == -1, chain ends on 1 and there is no cycle
-- l == -2, chain enters a cycle but not immediately thus ineligible
-- l == -3, cycle/chain has element larger than 10^6 thus ineligible
-- otherwise l is undefined
initVector :: U.Vector Int
initVector = U.create (do
    v <- MU.replicate (10^(5 :: Int) + 1) 0
    MU.write v 0 (-1)
    MU.write v 1 (-1)
    return v
                      )

initSearchState :: SearchState
initSearchState = SearchState { index = 0, searchVector = initVector }

modifySearchVector :: (U.Vector Int -> U.Vector Int) -> SearchState -> SearchState
modifySearchVector f s = s { searchVector = f (searchVector s) }
{-# INLINE modifySearchVector #-}


generateUpdate :: MaybeT (State SearchState) (Int, [Int])
generateUpdate = do
    v <- gets searchVector
    prev <- gets index
    i <- hoistMaybe $ (prev+1+) <$> U.elemIndex 0 (U.drop (prev+1) v)
    let chain = amicableChain i
        go :: (Int, [Int]) -> Int -> ExceptT (Int, [Int]) (State SearchState) (Int, [Int])
        go acc k
          | k `notElem` snd acc = case v U.!? k of
                Just 0  -> return (bimap (+1) (k:) acc)
                Nothing -> throwError (-3, snd acc)
                Just l  -> throwError (l, snd acc)
          | k == i = throwError acc
          | otherwise = throwError (-2, [i])
    lift . postprocess $ foldM go (1, [i]) chain
  where
    postprocess :: (Monad m) => ExceptT a m a -> m a
    postprocess ma = do
        a <- runExceptT ma
        return $ either id id a

updateState :: (Int, [Int]) -> MaybeT (State SearchState) ()
updateState (l, xs) = modify' . modifySearchVector $
    U.modify (\ v -> forM_ xs (\ x -> MU.write v x l))

-- generateUpdate :: MaybeT (State SearchState) (Int, [Int])
-- generateUpdate = do
--     v <- gets searchVector
--     prev <- gets index
--     i <- hoistMaybe $ (prev+1+) <$> U.elemIndex 0 (U.drop (prev+1) v)
--     let chain = amicableChain i
--         go :: (Int, [Int]) -> Int -> ExceptT (Int, [Int]) (State SearchState) (Int, [Int])
--         go acc k
--           | k `notElem` snd acc = case v U.!? k of
--                 Just 0  -> return (bimap (+1) (k:) acc)
--                 Nothing -> throwError (-3, snd acc)
--                 Just l  -> throwError (l, snd acc)
--           | k == i = throwError acc
--           | otherwise = throwError (-2, [i])
--     lift . postprocess $ foldM go (1, [i]) chain
--   where
--     postprocess :: (Monad m) => ExceptT a m a -> m a
--     postprocess ma = do
--         a <- runExceptT ma
--         return $ either id id a
-- 
-- updateState :: (Int, [Int]) -> MaybeT (State SearchState) ()
-- updateState (l, xs) = modify' . modifySearchVector $
--     U.modify (\ v -> forM_ xs (\ x -> MU.write v x l))

-- updateChain :: Int -> MaybeT (State (U.Vector Int)) Int
-- updateChain p = do
--     v <- get
--     i <- hoistMaybe $ (p+1+) <$> U.elemIndex 0 (U.drop (p+1) v)
--     let chain = amicableChain i
--         go :: (Int, [Int]) -> Int -> ExceptT (Int, [Int]) (State (U.Vector Int)) (Int, [Int])
--         go acc k
--           | k `notElem` snd acc = case v U.!? k of
--                 Just 0  -> return (bimap (+1) (k:) acc)
--                 Nothing -> throwError (-3, snd acc)
--                 Just l  -> throwError (l, snd acc)
--           | k == i = throwError acc
--           | otherwise = throwError (-2, [i])
--     exceptToMaybeT ((foldM go (1, [i]) chain >>= updateState) `catchError` updateState)
--     return i
--   where
--     updateState :: (Int, [Int]) -> ExceptT (Int, [Int]) (State (U.Vector Int)) ()
--     updateState (l, xs) = modify' $
--         U.modify (\ v -> forM_ xs (\ x -> MU.write v x l))

-- updateChain :: Int -> MaybeT (State (U.Vector Int)) Int
-- updateChain p = do
--     v <- get
--     i <- hoistMaybe $ (p+1+) <$> U.elemIndex 0 (U.drop (p+1) v)
--     let chain = amicableChain i
--         go :: (Int, [Int]) -> Int -> MaybeT (State (U.Vector Int)) (Int, [Int])
--         go acc k
--           | k `notElem` snd acc = case v U.!? k of
--                 Just 0    -> return (bimap (+1) (k:) acc)
--                 Nothing   -> updateState (-3, snd acc) >> throwNothing
--                 Just l    -> updateState (l, snd acc) >> throwNothing
--           | k == i = updateState acc >> throwNothing
--           | otherwise = updateState (-2, [i]) >> throwNothing
--     (foldM go (1, [i]) chain >>= updateState) `catchNothing` return ()
--     return i
--   where
--     updateState :: (Int, [Int]) -> MaybeT (State (U.Vector Int)) ()
--     updateState (l, xs) = modify' $
--         U.modify (\ v -> forM_ xs (\ x -> MU.write v x l))


loop :: MaybeT (State SearchState) Int
loop = generateUpdate >>= updateState >> loop

onState :: (s -> a) -> State s a
onState f = state (\ s -> (f s, s))


main :: IO ()
main = print $ (evalState . runMaybeT)
                    (loop `catchNothing` lift (onState (U.maxIndex . searchVector)))
                    initSearchState



