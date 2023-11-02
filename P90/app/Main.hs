{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Bifunctor
import Data.Int
import Data.List (partition)
import Data.Ord
import Data.Vector.Unboxed qualified as U


class SimplePrint a where
  simplePrint :: a -> String

newtype Die = Die { unDie :: U.Vector Int8
                  } deriving (Show, Eq)
-- Die is backed by a vector.
-- The number in the n-th position of the vector is the number of times the
-- digit n appears on the die.

instance SimplePrint Die where
  simplePrint (Die u) = show
    $ fst
    $ U.foldr go ([], fromIntegral $ U.length u - 1) u
    where
      go :: Int8 -> ([Int8], Int8) -> ([Int8], Int8)
      go n (xs, c) = if n /= 0
                     then go (n-1) (c:xs, c)
                     else (xs, c-1)

emptyDie :: Die
emptyDie = Die { unDie = U.replicate 10 0
               }

instance Ord Die where
  compare = comparing (Down . unDie)


newtype Foo = Foo { unFoo :: (Die, Die)
                  } deriving (Show, Eq, Ord)

instance SimplePrint Foo where
  simplePrint (Foo (a, b)) = "Foo (" ++ simplePrint a ++ "," ++ simplePrint b ++ ")"

emptyFoo :: Foo
emptyFoo = Foo (emptyDie, emptyDie)


hasNice :: Die -> Bool
hasNice d = d `has` 6 || d `has` 9

has :: Die -> Int -> Bool
has d n = unDie d U.! n > 0
{-# INLINE has #-}

has01 :: Foo -> Bool
has01 (Foo (a, b)) = (a `has` 0 && b `has` 1) || (a `has` 1 && b `has` 0)

has04 :: Foo -> Bool
has04 (Foo (a, b)) = (a `has` 0 && b `has` 4) || (a `has` 4 && b `has` 0)

has09 :: Foo -> Bool
has09 (Foo (a, b)) = (a `has` 0 && hasNice b) || (hasNice a && b `has` 0)

has16 :: Foo -> Bool
has16 (Foo (a, b)) = (a `has` 1 && hasNice b) || (hasNice a && b `has` 1)

has25 :: Foo -> Bool
has25 (Foo (a, b)) = (a `has` 2 && b `has` 5) || (a `has` 5 && b `has` 2)

has36 :: Foo -> Bool
has36 (Foo (a, b)) = (a `has` 3 && hasNice b) || (hasNice a && b `has` 3)

has49 :: Foo -> Bool
has49 (Foo (a, b)) = (a `has` 4 && hasNice b) || (hasNice a && b `has` 4)

has64 :: Foo -> Bool
has64 (Foo (a, b)) = (hasNice a && b `has` 4) || (a `has` 4 && hasNice b)

has81 :: Foo -> Bool
has81 (Foo (a, b)) = (a `has` 8 && b `has` 1) || (a `has` 1 && b `has` 8)

hasEverything :: Foo -> Bool
hasEverything x = all ($ x) hasList
  where
    hasList = [ has01
              , has04
              , has09
              , has16
              , has25
              , has36
              , has49
              -- , has64  -- Note: has49 == has64
              , has81
              ]


generateDice :: (Integral a) => a -> a -> a -> [[a]]
-- l is length of the backing vector in the die, i.e. the cardinality of the set
-- of digits.
-- m is the maximum amount of times a digit is allowed to appear on a die
-- E.g. m == 1 then the digits on the die must be distinct.
-- s is the number on faces on each die.
generateDice l m = go l
  where
    go 1 s = [[s] | s <= m]
    go len s = concatMap (\ x -> map (x:) (go (len-1) (s-x))) [0..min s m]

candidateDice :: [Die]
candidateDice = map (Die . U.fromList) $ generateDice 10 1 6

candidateFoos :: [Foo]
candidateFoos = go candidateDice
  where
    go :: [Die] -> [Foo]
    go [] = []
    go xl@(x:xs) = map (Foo . (x,)) xl ++ go xs


result1 :: [Foo]
result1 = filter hasEverything candidateFoos

result2 :: (Int, Int)
result2 = bimap length length
    $ partition (\ (Foo (a, b)) -> a == b) result1


main :: IO ()
main = print result2
-- main = print $ length $ filter hasEverything testFoos
