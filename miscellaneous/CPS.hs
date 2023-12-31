module Main where

import Control.Monad.Trans.Cont

-- newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
--
--
-- instance Functor (ContT r m) where
--   fmap :: (a -> b) -> Cont r m a -> Cont r m b
--   fmap f m = ContT $ \ c -> runContT m (c . f)
--
--   ContT $ \ c -> runContT m (c . f) :: Cont r m b
--
--   \ c -> runContT m (c . f) :: (b -> m r) -> m r
--   c :: b -> m r
--   runContT m (c . f) :: m r
--
--   m (c . f) :: (a -> r) -> r
--   (c . f) :: (a -> r)
--
--
-- instance Applicative (ContT r m) where
--   pure :: a -> ContT r m a
--   ContT ($ x) :: Cont r m a
--   ($ x) :: (a -> r) -> r
--
--   (<*>) :: Cont r m (a -> b) -> Cont r m a -> Cont r m b
--   f <*> v = ContT $ \ c -> runContT f $ \ g -> runContT v (c . g)
--
--   ContT $ \ c -> runContT f $ \ g -> runContT v (c . g) :: Cont r m b
--
--   \ c -> runContT f $ \ g -> runContT v (c . g) :: (b -> m r) -> m r
--   c :: b -> m r
--   runContT f $ \ g -> runContT v (c . g) :: m r
--
--   runContT f :: ((a -> b) -> m r) -> m r
--   \ g -> runContT v (c . g) :: (a -> b) -> m r
--
--   g :: a -> b
--   runContT v (c . g):: m r
--
--   v :: Cont r m a
--   (c . g) :: a -> m r
--
--
-- instance Monad (ContT r m) where
--   (>>=) :: Cont r m a -> (a -> Cont r m b) -> Cont r m b
--   m >>= k = ContT $ \ c -> runContT m (\ x -> runContT (k x) c)
--
--   (\ c -> runContT m (\ x -> runContT (k x) c)) :: (b -> m r) -> m r
--   c :: b -> m r
--   runContT m (\ x -> runContT (k x) c) :: m r
--
--   runContT m :: (a -> m r) -> m r
--   (\ x -> runContT (k x) c) :: a -> m r
--
--   x :: a
--   runContT (k x) c :: m r
--
--   runContT (k x) :: (b -> m r) -> m r
--
--
-- (>>=) passes the suspended computation backwards in time?
-- (b -> m r) and (a -> Cont r m b) generates a (a -> m r)?
--
-- a is implicitly saved in Cont r m a
-- That a is used to generate a (Cont r m b)
--
-- (\ x -> runContT (k x) c) :: a -> m r
--
--
-- runContT (m >>= k) id
-- = runContT (ContT $ \ c -> runContT m (\ x -> runContT (k x) c)) id
-- = (\ c -> runContT m (\ x -> runContT (k x) c)) id
-- = runContT m (\ x -> runContT (k x) id)
--
-- callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
-- callCC f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c
--
-- ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c :: ContT r m a
-- (\ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c) :: (a -> m r) -> m r
-- c :: a -> m r
-- runContT (f (\ x -> ContT $ \ _ -> c x)) c :: m r
--
-- runContT (f (\ x -> ContT $ \ _ -> c x)) :: (a -> m r) -> m r
-- f :: (a -> ContT r m b) -> ContT r m a
-- (\ x -> ContT $ \ _ -> c x) :: a -> ContT r m b
--
-- x :: a
-- ContT $ \ _ -> c x :: Cont r m b    -- WTF?
-- It is technically a Cont r m b in the sense that the m r has been predetermined
-- \ _ -> c x == const (c x)
-- (\ _ -> c x) :: (b -> m r) -> m r
-- _ :: (b -> m r)
-- c x :: m r
--
--
--
--
-- What is the type of ContT (const a)?
-- ContT :: (a -> m r) -> m r
-- const mr :: a -> m r
-- Therefore,
-- ContT (const mr) :: ContT r m a
-- where a can be any type
-- which is rather peculiar
--
-- const :: a -> b -> a represents a constant function with a domain of b and a
-- codomain of a
--
--
--
--
-- pure :: a -> Cont r m a
-- ContT :: ((a -> m r) -> m r) -> ContT r m a
-- They are two very different things
--
--
--
--
-- What is the type of cont (const 1)
-- :t cont (const 1)
-- cont (const 1) :: Num r => Cont r a    -- WTF??????????
-- How can r be constrained while Cont r is still a monad?
--
-- cont :: ((a -> r) -> r) -> Cont r a
-- const 1 :: Num a => b -> a
--
--
--
--
-- foo :: Int -> Cont r String
-- foo x = callCC $ \k -> do
--     let y = x ^ 2 + 3
--     when (y > 20) $ k "over twenty"
--     return (show $ y - 4)
--
-- callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
-- callCC f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c
--
-- foo x =
--     ContT $
--         \ c ->
--             runContT
--                 ((\ k ->
--                      do
--                           let y = x ^ 2 + 3
--                           when (y > 20) $ k "over twenty"
--                           return (show $ y - 4)
--                  )
--                     (\ x1 -> ContT $ \ _ -> c x1)
--                 )
--                 c
--
-- foo x =
--     ContT $
--         \ c ->
--             runContT
--                 (do
--                      let y = x ^ 2 + 3
--                      when (y > 20) $ (\ x1 -> ContT $ \ _ -> c x1) "over twenty"
--                      return (show $ y - 4)
--                 )
--                 c
--
-- foo x =
--     ContT $
--         \ c ->
--             runContT
--                 (do
--                      let y = x ^ 2 + 3
--                      when (y > 20) $ (ContT $ \ _ -> c "over twenty")
--                      return (show $ y - 4)
--                 )
--                 c
--
-- (>>=) :: ContT r m a -> (a -> Cont r m b) -> Cont r m b
-- m >>= k = ContT $ \ c -> runContT m (\ x -> runContT (k x) c)
--
--
--
--
--
-- foo :: Int -> Cont r String
-- foo x = callCC $ \k -> do
--     let y = x ^ 2 + 3
--     when (y > 20) $ k "over twenty"
--     return (show $ y - 4)
--
-- k :: a -> Cont r b
--
-- callCC :: ((String -> Cont r ()) -> ContT r String)
--        -> ContT r String
-- callCC f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c
--
--
--
-- foo :: Int -> Cont r String
-- foo x = callCC $ \k -> do
--     let y = x ^ 2 + 3
--     when (y > 20) $ k "over twenty"
--     return (show $ y - 4)
--
-- callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
-- callCC f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c
--
-- k = (\ x -> ContT $ \ _ -> c x) ????
--
--
--
--
-- What is (\ x -> ContT $ \ _ -> c x)?
-- h c = (\ x -> ContT $ \ _ -> c x)
-- h :: (a -> r) -> a -> ContT r m b
-- h takes a continuation and an (a) and returns a (ContT r m b)
-- The (ContT r m b) is only in name.
-- The continuation passed to (ContT r m b) is not used at all
--
-- The magic of f lies in secretly holding an a?
--
--
--
--
-- Is the type (forall a m b r. a -> ContT r m b) inhabited?
-- Let suppose m = Identity and hence (a -> ContT r m b) ~ (a -> Cont r b)
-- (a -> Cont r b) ~ (a -> (b -> r) -> r)
-- There is no way in general to use (b -> r) given an (a).
-- Hence, the existence of a function (a -> Cont r b) would imply a way to
-- generate an (r) given an (a) for any type r and a.
-- That seems impossible.
--
-- However, this is not how callCC is used.
-- callCC is passed (forall r. (a -> ContT r m b) -> ContT r m a) for some
-- fixed a, m and b.
-- This type is inhabited by at least some choices of a, m and b.
--
-- What is the type (forall r. (a -> ContT r m b) -> ContT r m a) inhabited by?
-- Choosing (a ~ Int), (b ~ ()), (m ~ Identity), we have the example above.
--
-- Having a (ContT r m a) in the return type affords us a (a -> r).
-- Can we cheat by intercepting (a) and package it up to (ContT r m a)?
--
-- f :: forall b r. (a -> ContT r m b) -> ContT r m a
-- f x = ContT $ \ c -> 
--
--
--
--
--
--
--
--
--
--
-- callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
-- (ContT r m b) and (ContT r m a) both produce a (m r) when a suitable
-- continuation is passed
--
-- (\ x -> ContT $ \ _ -> c x)
--
-- (a -> ContT r m b)
--
--
--
--
-- What is the simplest ((a -> ContT r m b) -> ContT r m a) I could think of?
-- (Cont r m a) isomorphic to ((a -> m r) -> m r))
--
--
--
-- c :: a -> m r
-- mr :: m r
--
-- Can m r be generated somehow?
-- (a -> m r) -> m r
--
-- g :: (a -> ContT r m b) -> ContT r m a
-- g x = ContT $ \ c -> mr
--
--
--
--
--
--
-- (>>=) passes the suspended computation backwards in time?
-- (b -> m r) and (a -> Cont r m b) generates (a -> m r)?
--
add :: (Num a) => a -> a -> (a -> t) -> t
add x y cont = cont (x + y)

sub :: (Num a) => a -> a -> (a -> t) -> t
sub x y cont = cont (x - y)

say :: (Show a, Num a) => a -> (String -> t) -> t
say x cont = cont (show x)

-- add 5 2 (sub 3) :: _
-- add 5 2 :: (Num a) => (a -> t) -> t
-- sub 3 :: (Num a1) => a1 -> (a1 -> t1) -> t1
-- a -> t ~ a1 -> (a1 -> t1) -> t1
-- a ~ a1
-- t ~ (a1 -> t1) -> t1
--
--
main :: IO ()
main = print "hello"
