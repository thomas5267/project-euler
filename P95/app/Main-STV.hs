{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Euler.Helper

import Data.Bifunctor

import Data.MemoTrie
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU

import Data.Numbers.Primes

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Control.DeepSeq
import Control.Monad.ST
import Data.STRef


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

data SearchState s = SearchState { index :: {-# UNPACK #-} !(STRef s Int)
                                 , searchVector :: {-# UNPACK #-} !(MU.MVector s Int)
                                 }

-- We use Int to encode the following.
-- l >=  1, l = length of cycle
-- l ==  0, length of cycle waiting to be determined
-- l == -1, chain ends on 1 and there is no cycle
-- l == -2, chain enters a cycle but not immediately thus ineligible
-- l == -3, cycle/chain has element larger than 10^6 thus ineligible
-- otherwise l is undefined

-- initVector :: U.Vector Int
-- initVector = U.create (do
--     v <- MU.replicate (10^(5 :: Int) + 1) 0
--     MU.write v 0 (-1)
--     MU.write v 1 (-1)
--     return v
--                       )

initSearchState :: ST s (SearchState s)
initSearchState = do
    i <- newSTRef 0
    mv <- MU.replicate (5*10^(4 :: Int) + 1) 0
    MU.write mv 0 (-1)
    MU.write mv 1 (-1)
    return $ SearchState { index = i, searchVector = mv }

-- ST s (SearchState s -> (a, SearchState s))

-- modifySearchVector :: (U.Vector Int -> U.Vector Int) -> SearchState -> SearchState
-- modifySearchVector f s = s { searchVector = f (searchVector s) }
-- {-# INLINE modifySearchVector #-}



elemIndexMU :: (Eq a, U.Unbox a) => a -> MU.MVector s a -> ST s (Maybe Int)
elemIndexMU e = MU.ifoldr go Nothing
  where
    go i x acc = if e == x
                 then Just i
                 else acc
{-# INLINE elemIndexMU #-}

testVector :: ST s (MU.MVector s Int)
testVector = do
    v <- MU.replicate (10^(5 :: Int) + 1) 0
    MU.write v 0 (-1)
    MU.write v 1 (-1)
    return v

-- elemIndexMU :: forall a s. (Eq a, U.Unbox a) => a -> MU.MVector s a -> ST s (Maybe Int)
-- elemIndexMU e mv = postprocess <$> MU.foldr go (Right 0) mv
--   where
--     go :: a -> Either Int Int -> Either Int Int
--     go x acc = if force $ e /= x
--                then first (+1) acc
--                else Left 0
--     postprocess :: Either Int Int -> Maybe Int
--     postprocess (Left i) = Just i
--     postprocess (Right _) = Nothing
-- {-# INLINE elemIndexMU #-}

maxIndexMU :: MU.MVector s Int -> ST s Int
-- Not correct for all cases but good enough for this case
maxIndexMU = fmap snd . MU.ifoldl' go (0, 0)
  where
    go acc@(!_, !mx) !i !x = if mx >= x
                             then acc
                             else (i, x)

safeRead :: (U.Unbox a) => MU.MVector s a -> Int -> ST s (Maybe a)
safeRead mv i = if i < MU.length mv - 1
                then Just <$> MU.read mv i
                else return Nothing

type StateST s = ReaderT (SearchState s) (ST s)

-- ST s (Maybe Int) -> MaybeT (StateT (SearchState s) (ST s)) Int
-- ST s (Maybe Int) -> StateT (SearchState s) (ST s) (Maybe Int)
-- ST s (Maybe Int) -> (SearchState s -> ST s (Maybe Int))
--
-- lift :: ST s (Maybe Int) -> StateST s (Maybe Int)
-- 
-- gets searchVector :: MaybeT (StateST s) (ST s (MU.MVector s Int))

-- Performance characteristics of mutable vectors are absurd.
-- Using the homeroll version of elemIndexMU is much slower (1000x) than
-- converting the mutable vector to an immutable vector then call U.elemIndex
-- generateUpdate :: forall s. MaybeT (StateST s) (Int, [Int])
-- generateUpdate = do
--     mv <- asks searchVector
--     v <- {-# SCC v #-} U.freeze mv
--     stprev <- asks index
--     prev <- lift . lift $ readSTRef stprev
--     i <- {-# SCC i #-} hoistMaybe $ fmap (prev+1+) . U.elemIndex 0 . U.drop (prev+1) $ v
--     let chain = amicableChain i
--         go :: (Int, [Int]) -> Int -> ExceptT (Int, [Int]) (StateST s) (Int, [Int])
--         go acc@(!_, !_) !k
--           | k `notElem` snd acc = do
--               case v U.!? k of
--                 Just 0  -> return (bimap (+1) (k:) acc)
--                 Nothing -> throwError (-3, snd acc)
--                 Just l  -> throwError (l, snd acc)
--           | k == i = throwError acc
--           | otherwise = throwError (-2, [i])
--     lift . lift $ writeSTRef stprev i
--     lift . postprocess $ foldM go (1, [i]) chain
-- 
--   where
--     postprocess :: (Monad m) => ExceptT a m a -> m a
--     postprocess ma = do
--         a <- runExceptT ma
--         return $ either id id a

generateUpdate :: forall s. MaybeT (StateST s) (Int, [Int])
generateUpdate = do
    mv <- {-# SCC mv #-} asks searchVector
    stprev <- {-# SCC stprev #-} asks index
    prev <- {-# SCC prev #-} lift . lift $ readSTRef stprev
    i <- {-# SCC i #-} MaybeT . lift $ fmap (prev+1+) <$!!> (elemIndexMU 0 . MU.drop (prev+1)) mv
    let go :: (Int, [Int]) -> Int -> ExceptT (Int, [Int]) (StateST s) (Int, [Int])
        go acc@(!_, !_) !k
          | k `notElem` snd acc = do
              x <- lift . lift $ safeRead mv k
              case x of
                Just 0  -> return (bimap (+1) (k:) acc)
                Nothing -> throwError (-3, snd acc)
                Just l  -> throwError (l, snd acc)
          | k == i = throwError acc
          | otherwise = throwError (-2, [i])
    lift . lift $ writeSTRef stprev i
    lift . postprocess $ foldM go (1, [i]) (amicableChain i)

  where
    postprocess :: (Monad m) => ExceptT a m a -> m a
    postprocess ma = do
        a <- runExceptT ma
        return $ either id id a

updateState :: (Int, [Int]) -> MaybeT (StateST s) ()
updateState (l, xs) = do
    mv <- asks searchVector
    forM_ xs (\ x -> MU.write mv x l)

-- -- generateUpdate :: MaybeT (State SearchState) (Int, [Int])
-- -- generateUpdate = do
-- --     v <- gets searchVector
-- --     prev <- gets index
-- --     i <- hoistMaybe $ (prev+1+) <$> U.elemIndex 0 (U.drop (prev+1) v)
-- --     let chain = amicableChain i
-- --         go :: (Int, [Int]) -> Int -> ExceptT (Int, [Int]) (State SearchState) (Int, [Int])
-- --         go acc k
-- --           | k `notElem` snd acc = case v U.!? k of
-- --                 Just 0  -> return (bimap (+1) (k:) acc)
-- --                 Nothing -> throwError (-3, snd acc)
-- --                 Just l  -> throwError (l, snd acc)
-- --           | k == i = throwError acc
-- --           | otherwise = throwError (-2, [i])
-- --     lift . postprocess $ foldM go (1, [i]) chain
-- --   where
-- --     postprocess :: (Monad m) => ExceptT a m a -> m a
-- --     postprocess ma = do
-- --         a <- runExceptT ma
-- --         return $ either id id a
-- -- 
-- -- updateState :: (Int, [Int]) -> MaybeT (State SearchState) ()
-- -- updateState (l, xs) = modify' . modifySearchVector $
-- --     U.modify (\ v -> forM_ xs (\ x -> MU.write v x l))
-- 
-- -- updateChain :: Int -> MaybeT (State (U.Vector Int)) Int
-- -- updateChain p = do
-- --     v <- get
-- --     i <- hoistMaybe $ (p+1+) <$> U.elemIndex 0 (U.drop (p+1) v)
-- --     let chain = amicableChain i
-- --         go :: (Int, [Int]) -> Int -> ExceptT (Int, [Int]) (State (U.Vector Int)) (Int, [Int])
-- --         go acc k
-- --           | k `notElem` snd acc = case v U.!? k of
-- --                 Just 0  -> return (bimap (+1) (k:) acc)
-- --                 Nothing -> throwError (-3, snd acc)
-- --                 Just l  -> throwError (l, snd acc)
-- --           | k == i = throwError acc
-- --           | otherwise = throwError (-2, [i])
-- --     exceptToMaybeT ((foldM go (1, [i]) chain >>= updateState) `catchError` updateState)
-- --     return i
-- --   where
-- --     updateState :: (Int, [Int]) -> ExceptT (Int, [Int]) (State (U.Vector Int)) ()
-- --     updateState (l, xs) = modify' $
-- --         U.modify (\ v -> forM_ xs (\ x -> MU.write v x l))
-- 
-- -- updateChain :: Int -> MaybeT (State (U.Vector Int)) Int
-- -- updateChain p = do
-- --     v <- get
-- --     i <- hoistMaybe $ (p+1+) <$> U.elemIndex 0 (U.drop (p+1) v)
-- --     let chain = amicableChain i
-- --         go :: (Int, [Int]) -> Int -> MaybeT (State (U.Vector Int)) (Int, [Int])
-- --         go acc k
-- --           | k `notElem` snd acc = case v U.!? k of
-- --                 Just 0    -> return (bimap (+1) (k:) acc)
-- --                 Nothing   -> updateState (-3, snd acc) >> throwNothing
-- --                 Just l    -> updateState (l, snd acc) >> throwNothing
-- --           | k == i = updateState acc >> throwNothing
-- --           | otherwise = updateState (-2, [i]) >> throwNothing
-- --     (foldM go (1, [i]) chain >>= updateState) `catchNothing` return ()
-- --     return i
-- --   where
-- --     updateState :: (Int, [Int]) -> MaybeT (State (U.Vector Int)) ()
-- --     updateState (l, xs) = modify' $
-- --         U.modify (\ v -> forM_ xs (\ x -> MU.write v x l))
-- 
-- 
loop :: MaybeT (StateST s) ()
loop = generateUpdate >>= updateState >> loop

-- onStateT :: (Applicative m) => (s -> a) -> StateT s m a
-- onStateT f = StateT (\ s -> pure (f s, s))

-- Performance characteristics of mutable vectors are absurd.
-- Similarly for maxIndex it is highly likely that it is much faster
-- to use unsafeFreeze to make a copy of the mutable vector than call U.maxIndex
-- than to use the homeroll version of maxIndex.
processResult :: StateST s Int
processResult = do
    mv <- asks searchVector
    v <- U.unsafeFreeze mv
    return $ U.maxIndex v





main :: IO ()
-- main = print $ runST (test2 >>= U.freeze)
-- main = print "hello"
-- main = print $ runST $
--     evalStateT
--         (runMaybeT loop) -- (runMaybeT (loop `catchNothing` lift (onState (U.maxIndex . searchVector))))
--         initSearchState

main = print $ runST $
    initSearchState >>= runReaderT (runMaybeT loop >> processResult)


