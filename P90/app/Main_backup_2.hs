module Main where

import Euler.Helper

import Data.List (groupBy, insert, sort)
import Data.Ord (comparing)
import Data.Bifunctor

import Data.IntSet qualified as IS

import Control.Monad.State


-- data Pair a = Pair a a
-- 
-- instance Functor Pair where
--   fmap f (Pair x y) = Pair (f x) (f y)

symmetricDifference :: IS.IntSet -> IS.IntSet -> IS.IntSet
symmetricDifference a b = (a `IS.difference` b) `IS.union` (b `IS.difference` a)

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)
{-# INLINE flipPair #-}


edges0 :: [(Int, Int)]
edges0 = [ (0, 1)
         , (0, 4)
         , (0, 9)
         , (1, 6)
         , (3, 6)
         , (4, 9)
         , (6, 4)
         , (8, 1)
         ]

edges1 :: [(Int, Int)]
edges1 = [ (0, 1)
         , (0, 4)
         , (0, 6)
         , (1, 6)
         , (3, 6)
         , (4, 6)
         , (8, 1)
         ]

edges2 :: [(Int, Int)]
edges2 = [ (0, 1)
         , (0, 4)
         , (0, 6)
         , (1, 6)
         , (3, 6)
         , (4, 9)
         , (6, 4)
         , (8, 1)
         ]

edges3 :: [(Int, Int)]
edges3 = [ (0, 1)
         , (0, 4)
         , (0, 9)
         , (1, 6)
         , (3, 6)
         , (4, 6)
         , (6, 4)
         , (8, 1)
         ]


newtype Die = Die { unDie :: [Int]
                  } deriving (Show, Eq)

instance Ord Die where
  compare = comparing (length . unDie) `whenEQ` comparing unDie

-- unionDie :: Die -> Die -> Die
-- unionDie (Die a) (Die b) = Die (a `IS.union` b)


newtype Foo = Foo { unFoo :: (Die, Die)
                  } deriving (Show, Eq, Ord)

card :: Foo -> Int
card (Foo (a, b)) = (length . unDie) a + (length . unDie) b

-- instance Ord Foo where
--   compare = comparing card
--             `whenEQ` comparing (fst . unFoo)
--             `whenEQ` comparing (snd . unFoo)

normalise :: Foo -> Foo
normalise (Foo (a, b)) = case compare a b of
    GT -> Foo (b, a)
    _  -> Foo (a, b)


addEdge :: (Int, Int) -> Foo -> Foo
addEdge (x, y) (Foo (Die as, Die bs)) =
    Foo (Die (insert x as), Die (insert y bs))


-- unionFoo :: Foo -> Foo -> Foo
-- unionFoo (Foo (ax, ay)) (Foo (bx, by)) = Foo (ax `unionDie` bx, ay `unionDie` by)


-- test :: [(Int, Int)] -> [Foo]
-- test xl = map fst $
--             foldr go [(Foo (Die IS.empty, Die IS.empty), halfRD (length xl))] xl
--   where
--     halfRD x = q -- + r
--       where
--         (q, r) = x `quotRem` 2
-- 
--     go :: (Int, Int) -> [(Foo, Int)] -> [(Foo, Int)]
--     go x = concatMap choose
--       where
--         choose :: (Foo, Int) -> [(Foo, Int)]
--         choose (as, 0) = [(x `addEdge` as, 0)]
--         choose (as, s) = [(x `addEdge` as, s), (flipPair x `addEdge` as, s-1)]
-- 
edgesToFoos :: [(Int, Int)] -> [Foo]
edgesToFoos = nubOrd . sort . map (normalise . flip addEdge (Foo (Die [], Die [])))

addEdgesToFoos :: [(Int, Int)] -> [Foo] -> [Foo]
addEdgesToFoos xs = nubOrd . sort . map normalise . liftM2 addEdge xs

unsafeAddEdgesToFoos :: [(Int, Int)] -> [Foo] -> [Foo]
unsafeAddEdgesToFoos xs = sort . map normalise . liftM2 addEdge xs

pairToEdges :: (Int, Int) -> [(Int, Int)]
pairToEdges (a, b) = [(a, b), (b, a)]

addPairToFoos :: (Int, Int) -> [Foo] -> [Foo]
addPairToFoos = addEdgesToFoos . pairToEdges

-- test2 :: [Foo] -> [Foo]
-- test2 = filter (\ (Foo (Die a, Die b)) -> IS.size a <= 5 && IS.size b <= 5)
-- 
-- test3 :: [Foo] -> [Foo]
-- test3 = nubOrd . sort . map normalise

isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
-- Assumes ordered lists
isSubsetOf []    _  = True
isSubsetOf (_:_) [] = False
isSubsetOf al@(a:as) (b:bs) = case compare a b of
    EQ -> isSubsetOf as bs
    GT -> isSubsetOf al bs
    LT -> False

isSubDiceOf :: Foo -> Foo -> Bool
isSubDiceOf (Foo (Die ax, Die ay)) (Foo (Die bx, Die by)) =
    (ax `isSubsetOf` bx && ay `isSubsetOf` by)
    || (ax `isSubsetOf` by && ay `isSubsetOf` bx)

prune :: [Foo] -> [Foo]
-- prune is O(n^2) but since the list is sorted I think it could be done in O(n)
prune [] = []
prune (x:xs) = x : prune (filter (not . (x `isSubDiceOf`)) xs)


-- temp0 :: [Foo]
-- temp0 = prune . test3 . test2 . test $ edges0
-- 
-- temp1 :: [Foo]
-- temp1 = prune . test3 . test2 . test $ edges1

testEdges0 :: [(Int, Int)]
testEdges0 = [ (a, b) | a <- [0..9], b <- [0..9] ]

tempFoos :: [Foo]
tempFoos = addEdgesToFoos [(2, 5), (5, 2)] $ edgesToFoos [(0,1)]

tempF0 :: [Int] -> [Int] -> Bool
tempF0 (a1:a2:a3:a4:[]) (b1:b2:b3:b4:[]) =
    ((a1, a3) == (b1, b3) || (a1, a3) == (b3, b1))
    && ((a2, a4) == (b2, b4) || (a2, a4) == (b4, b2))

-- test0 :: [Foo]
-- test0 = 

main :: IO ()
main = print "hello"
