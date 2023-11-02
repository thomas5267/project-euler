{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Euler.Helper

import Data.Foldable
import Data.Int
import Data.IntMap.Strict qualified as IM
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Semigroup
import Data.Vector.Algorithms.Tim qualified as MU
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU

import Control.Monad
import Control.Monad.ST


findAnagram :: M.Map k [a] -> M.Map k [a]
findAnagram = M.filter go
  where
    go [] = False
    go [_] = False
    go _ = True


newtype Text = Text { unText :: U.Vector (Char, Int8)
                    } deriving (Eq, Ord)
-- Cannot use the Text library because I need underlying access to the vector
-- for permutations-related operations.

instance Show Text where
  show t = "(\""
    ++ U.foldr (:) [] cs
    ++ "\","
    ++ show is
    ++ ")"
    where
      (cs, is) = U.unzip $ unText t

processText :: U.Vector Char -> (UpperMultiSet, Text)
-- Text is not U.Vector Char but U.Vector (Char, Int8) because we wish to
-- distinguish between identitcal Chars for operations related to permutations.
-- Labelling Char requires us to generate UpperMultiSet in the process so it is
-- also returned to prevent recomputation at a later step.
processText cs = ( UpperMultiSet upperMS
                 , Text $ U.zip cs (U.replicate (U.length cs) 0 U.// updates)
                 )
  where
    upperMS :: U.Vector Int8
    updates :: [(Int, Int8)]
    (upperMS, updates, _) = U.foldl' go (U.replicate 26 0, [], 0) cs
      where
        go (ums, xs, i) c =
            (U.accum (+) ums [(cInt, 1)], (i, ums U.! cInt) : xs, i+1)
          where
            cInt = uppercaseToInt c


newtype UpperMultiSet = UpperMultiSet { unUpperMultiSet :: U.Vector Int8
                                      } deriving (Show, Eq, Ord)

uppercaseToInt :: Char -> Int
uppercaseToInt c = if result >= 0 && result <= 25
                   then result
                   else error "uppercaseToInt: invalid character"
  where
    result = fromEnum c - 65

wordListIO :: IO [(UpperMultiSet, Text)]
wordListIO = map (processText . U.fromList)
    . splitOn ","
    . filter (/= '"')
    <$> readFile "p098_words.txt"

wordMapIO :: IO (M.Map UpperMultiSet [Text])
wordMapIO = findAnagram . foldr go M.empty <$> wordListIO
  where
    go (k, t) = M.insertWith (++) k [t]


newtype DigitMultiSet = DigitMultiSet { unDigitMultiSet :: U.Vector Int8
                                      } deriving (Show, Eq, Ord)

squares :: [Int]
-- The longest anagram word in the list has 9 letters.
-- Therefore, the longest anagramic square can have 9 digits.
squares = takeWhile (< 10^(9 :: Int)) [ x^(2 :: Int) | x <- [1..] ]

type DigitMap = IM.IntMap (U.Vector Int8)
-- A DigitMap is an IntMap with Int as keys and digits of the keys as values.

squareMap :: M.Map DigitMultiSet DigitMap
-- squareMap = M.filter (\ dm -> IM.size dm > 1) $ foldr addToMap M.empty squares
squareMap = foldr addToMap M.empty squares
  where
    addToMap :: Int
             -> M.Map DigitMultiSet (IM.IntMap (U.Vector Int8))
             -> M.Map DigitMultiSet (IM.IntMap (U.Vector Int8))
    addToMap x = M.insertWith IM.union dms (IM.singleton x xds)
      where
        xdl :: [Int8]
        xdl = digits x
        xds = U.fromList xdl
        dms = DigitMultiSet $ U.create $ do
            mu <- MU.replicate 10 0
            mapM_ (MU.modify mu (+1) . fromIntegral) xdl
            return mu
--         dms = DigitMultiSet
--             $ U.accum (+) (U.replicate 10 0)
--             . map (\ k -> (fromIntegral k, 1))
--             $ xdl


newtype NumberCount = NumberCount { unNumberCount :: U.Vector Int8
                                  } deriving (Show, Eq, Ord)

-- Count the number of times an alphabet has

multisetNumberCount :: U.Vector Int8 -> NumberCount
multisetNumberCount = NumberCount . U.accumulate (+) (U.replicate 9 0) . U.mapMaybe go
  where
    go :: Int8 -> Maybe (Int, Int8)
    go 0 = Nothing
    go x = Just (fromIntegral (x-1), 1)

squareNCMap :: M.Map NumberCount [DigitMap]
squareNCMap = M.mapKeysWith
    (++)
    (multisetNumberCount . unDigitMultiSet)
    (M.map (:[]) squareMap)

wordNCMapIO :: IO (M.Map NumberCount [[Text]])
wordNCMapIO = M.mapKeysWith (++) (multisetNumberCount . unUpperMultiSet)
    . M.map (:[])
    <$> wordMapIO


-- generateBackpermute :: (U.Unbox a, Ord a) => U.Vector a -> U.Vector a -> U.Vector Int
-- -- This function obeys the following identity:
-- -- U.backpermute y (generateBackpermute x y) = x
-- -- This function is suspected to be O(n^2 log(n)) which is far from the optimal
-- -- O(n log(n)).
-- generateBackpermute x y = runST $ do
--     mis <- MU.generate (U.length y) id
--     my <- U.thaw y
--     MU.sort (MU.zip my mis)
--     U.unsafeFreeze mis
--     invertPermutation mis
--     mx <- U.thaw
--   where
--     cmp (a, _) (b, _) = fromJust $ liftM2 compare (U.elemIndex a x) (U.elemIndex b x)

-- generateBackpermute (unText . snd $ processText (U.fromList "THESE")) (unText . snd $ processText (U.fromList "SHEET")) == U.fromList [4,1,2,0,3]
--
-- SHEET
-- 01234
--
-- EEHST
-- 23104
--
-- 23104
-- 01234
--
-- 32014
--
-- SHEET
-- 23104
-- EEHST
--
-- EEHST
-- 32014
-- SHEET
--
--
--
-- THESE
-- 01234
--
-- EEHST
-- 24130
--
-- generateBackpermute
--     (unText . snd $ processText (U.fromList "THESE"))
--     (unText . snd $ processText (U.fromList "SHEET")) == U.fromList [4,1,2,0,3]
--
-- generateBackpermute a (generateBackpermute b c) == generate
--
--
--
-- invertPermutation :: U.Vector Int -> U.Vector Int
-- invertPermutation u = generateBackpermute iden u
--   where
--     iden = U.enumFromN (U.length u)
--
-- U.backpermute y (generateBackpermute x y) == x
--
-- U.backpermute (U.backpermute (U.fromList "SHEET") s1) s2 == U.fromList "THESE"
-- U.backpermute (U.fromList "SHEET") s1 == U.fromList "EEHST"
-- U.backpermute
--     (U.fromList "SHEET")
--     (generateBackpermute (U.fromList "EEHST") (U.fromList "SHEET")) == U.fromList "EEHST"
-- s1 = generateBackpermute (U.fromList "EEHST") (U.fromList "SHEET") == U.fromList [2,3,1,0,4]
--
-- flip U.backpermute s2 (U.fromList "EEHST") == U.fromList "THESE"
-- U.backpermute (U.fromList "EEHST") s2 == U.fromList "THESE"
-- U.backpermute
--     (U.fromList "EEHST")
--     (generateBackpermute (U.fromList "THESE") (U.fromList "EEHST")) == U.fromList "THESE"
-- s2 = generateBackpermute (U.fromList "THESE") (U.fromList "EEHST") == U.fromList [4,2,0,3,1]
-- s3 = U.fromList [2,3,1,0,4]
--
-- Is this identity true?
-- invertPermutation (generateBackpermute x y) == generateBackpermute y x
--
-- generateBackpermute (U.fromList "EEHST") (U.fromList "SHEET") = U.fromList [2,3,1,0,4]
-- generateBackpermute (U.fromList "SHEET") (U.fromList "EEHST") = U.fromList [3,2,0,1,4]
-- Seems true
--
-- Seek
-- U.fromList [4,1,2,0,3]
--
-- Have:
-- U.fromList [2,3,1,0,4]
-- U.fromList [2,4,1,3,0]
-- invertPermutation
--
-- Seek:
-- U.backpermute
--
-- Have:
-- generateBackpermute
-- invertPermutation
--
-- Specialise U.backpermute :: U.Vector Int -> U.Vector Int -> U.Vector Int
--
-- Does f == U.backpermute?
-- f a b = generateBackpermute (invertPermutation a) (invertPermutation b)
-- f iden b = generateBackpermute iden (invertPermutation b)
-- f iden b = invertPermutation (invertPermutation b)
-- f iden b = b
--
-- f a iden = generateBackpermute (invertPermutation a) iden
-- f a iden = invertPermutation $ generateBackpermute iden (invertPermutation a)
-- f a iden = invertPermutation $ inverPermutation $ (invertPermutation a)
-- f a iden = invertPermutation a
--
-- generateBackpermute (invertPermutation a) iden
-- generateBackpermute (invertPermutation $ U.fromList [2,3,1,0,4]) iden
-- generateBackpermute (U.fromList [3,2,0,1,4]) (U.fromList [0,1,2,3,4])
-- generateBackpermute (U.fromList [3,2,0,1,4]) (U.fromList [0,1,2,3,4])
-- U.fromList [3,2,0,1,4]
--
-- This suggests
-- f a b = generateBackpermute a (invertPermutation b)
--
-- flip U.backpermute s2 . flip U.backpermute s1 == U.backpermute (U.backpermute s1 s2)
-- U.backpermute (U.backpermute s1 s2)
-- == U.backpermute (generateBackpermute s1 (invertPermutation s2))
-- == U.backpermute (generateBackpermute s1 (generateBackpermute iden s2))
-- == U.backpermute (generateBackpermute s1 (generateBackpermute iden s2))
--
-- 23104
-- 01234
--
-- 32014
--
--
-- s1 generated by sort
-- s2 generated by sort then inverted
--
-- flip U.backpermute s2 . flip U.backpermute s1 == U.backpermute (U.backpermute s1 s2)
-- U.backpermute (U.backpermute s1 s2) = U.backpermute (U.backpermute s1 (U.backpermute iden s3))
-- U.backpermute s1 (U.backpermute iden s3) == U.backpermute s1 . U.backpermute iden $ s3
-- U.backpermute s1 . U.backpermute iden $ s3 == U.backpermute (U.backpermute iden s1) $ s3
--
-- U.backpermute iden s1 = U.fromList [3,2,0,1,4]
--
--
--
-- SHEET -> THESE = SHEET -> EEHST -> THESE
--
--------------------------------------------------------------------------------
--
-- Identities:
-- U.backpermute y (generateBackpermute x y) = x
-- invertPermutation u = generateBackpermute iden u
-- invertPermutation (generateBackpermute x y) == generateBackpermute y x
-- flip U.backpermute s2 . flip U.backpermute s1 == U.backpermute (U.backpermute s1 s2)
-- U.backpermute a b = generateBackpermute b (invertPermutation a)
--
-- Is the following equation true?
-- invertPermutation (U.backpermute a b)
--     == U.backpermute (invertPermutation b) (invertPermutation a)
--
-- Test:
-- a == U.fromList [4,1,2,0,3]
-- b == U.fromList [3,2,0,1,4]
--
-- invertPermutation $ U.backpermute (U.fromList [4,1,2,0,3]) (U.fromList [3,2,0,1,4])
-- == invertPermutation $ U.fromList [0,2,4,1,3]
-- == U.fromList [0,3,1,4,2]
--
-- invertPermutation (U.fromList [4,1,2,0,3]) == U.fromList [3,1,2,4,0]
-- invertPermutation (U.fromList [3,2,0,1,4]) == U.fromList [2,3,1,0,4]
-- U.backpermute (U.fromList [2,3,1,0,4]) (U.fromList [3,1,2,4,0])
--     == U.fromList [0,3,1,4,2]
--
-- It would seem the equation is true.
--
--
-- Consider the following
-- generateBackpermute a (generateBackpermute b c) == generateBackpermute ?
--
-- First the types
-- generateBackpermute :: U.Vector Int -> U.Vector Int -> U.Vector Int
--
-- U.backpermute y (generateBackpermute x y) = x
-- generateBackpermute y (invertPermutation (generateBackpermute x y))
-- generateBackpermute y (generateBackpermute y x)
-- suggesting
-- generateBackpermute a (generateBackpermute b c)
-- == generateBackpermute (U.backpermute a (invertPermutation b)) c
-- or
-- == generateBackpermute (U.backpermute (invertPermutation a) b) c
--
--
-- generateBackpermute a (generateBackpermute b c)
-- == generateBackpermute a (c^(-1)b)
-- == (c^(-1)b)^(-1)a
-- == b^(-1)ca
-- == U.backpermute ((U.backpermute (invertPermutation b) c) a)
-- == generateBackpermute a (invertPermutation (U.backpermute (invertPermutation b) c))
-- == generateBackpermute a (U.backpermute (invertPermutation c) b)
-- == generateBackpermute a (invertPermutation (U.backpermute b (invertPermutation c)))
-- == generateBackpermute a (invertPermutation (generateBackpermute c b))
-- == generateBackpermute a (generateBackpermute b c)

-- generateBackpermute a (generateBackpermute b c)
-- == generateBackpermute a (c^(-1)b)
-- == (c^(-1)b)^(-1)a
-- == b^(-1)ca
-- == U.backpermute (invertPermutation b) (U.backpermute c a)
-- == generateBackpermute (U.backpermute c a) b
-- == generateBackpermute (generateBackpermute a (invertPermutation c)) b
--
--
--


generateBackpermute :: (U.Unbox a, Ord a) => U.Vector a -> U.Vector a -> U.Vector Int
-- This function obeys the following identity:
-- U.backpermute y (generateBackpermute x y) = x
-- This function is suspected to be O(n^2 log(n)) which is far from the optimal
-- O(n log(n)).
generateBackpermute x y = snd . U.unzip $ runST $ do
    my <- U.thaw (U.zip y (U.enumFromN 0 (U.length y)))
    MU.sortBy cmp my
    U.unsafeFreeze my
  where
    cmp (a, _) (b, _) = fromJust $ liftM2 compare (U.elemIndex a x) (U.elemIndex b x)

invertPermutation :: U.Vector Int -> U.Vector Int
invertPermutation u = U.update_ (U.replicate len 0) u (U.enumFromN 0 len)
  where
    len = U.length u
-- invertPermutation u = generateBackpermute iden u
--   where
--     iden = U.enumFromN @Int 0 (U.length u)

maybeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
maybeMaximum = fmap getMax . foldMap' (Just . Max)
{-# INLINE maybeMaximum #-}


f2 :: U.Vector Char -> U.Vector Int8 -> Bool
f2 cs is = isFunction && isInjective
  where
    isFunction = runST $ do
        mcs <- U.thaw cs
        mis <- U.thaw is
        let zipped = MU.zip mcs mis
        MU.sort zipped
        MU.foldr go True $ MU.zip zipped (MU.tail zipped)
      where
        go ((c1, i1), (c2, i2)) acc = (c1 /= c2 || i1 == i2) && acc

    isInjective = runST $ do
        mcs <- U.thaw cs
        mis <- U.thaw is
        let zipped = MU.zip mis mcs
        MU.sort zipped
        MU.foldr go True $ MU.zip zipped (MU.tail zipped)
      where
        go ((i1, c1), (i2, c2)) acc = (i1 /= i2 || c1 == c2) && acc

f1 :: NumberCount -> [Text] -> DigitMap -> Maybe Int
-- My English fails me in expressing my dissatisfaction with this function.
-- I need quick membership checks and intersections of m Int
f1 nc = if U.all (== 0) (U.tail $ unNumberCount nc)
        then withoutDuplicates
        else withDuplicates
  where
    toInt :: U.Vector Int8 -> Int
    toInt = U.foldl' go 0
      where
        go :: Int -> Int8 -> Int
        go acc d = acc*10 + fromIntegral d

    withoutDuplicates :: [Text] -> DigitMap -> Maybe Int
    withoutDuplicates [] _ = Nothing
    withoutDuplicates (t:ts) dm = max
        (IM.foldlWithKey' go Nothing dm)
        (withoutDuplicates ts dm)
      where
        backpermutes :: [U.Vector Int]
        backpermutes = map (generateBackpermute (unText t) . unText) ts

        go :: Maybe Int -> Int -> U.Vector Int8 -> Maybe Int
        go acc x xds = max
            acc
            (max x <$> maybeMaximum (filter (`IM.member` dm) candidateNumbers))
          where
            candidateNumbers :: [Int]
            candidateNumbers = map (toInt . U.backpermute xds) backpermutes

    withDuplicates :: [Text] -> DigitMap -> Maybe Int
    withDuplicates [] _ = Nothing
    withDuplicates (t:ts) dm = max
        (IM.foldlWithKey' go Nothing dm)
        (withDuplicates ts dm)
      where
        backpermutes :: [U.Vector Int]
        backpermutes = map (generateBackpermute (unText t) . unText) ts

        go :: Maybe Int -> Int -> U.Vector Int8 -> Maybe Int
        go acc x xds = max
            acc
            (max x <$> maybeMaximum (filter (`IM.member` dm) candidateNumbers))
          where
            candidateNumbers :: [Int]
            candidateNumbers = map toInt
                $ filter (f2 (fst . U.unzip . unText $ t))
                $ map (U.backpermute xds) backpermutes


resultsIO :: IO (M.Map NumberCount ([[Text]], [DigitMap]))
resultsIO = do
    wordNCMap <- wordNCMapIO
    return $ M.intersectionWith (,) wordNCMap squareNCMap

answerIO :: IO (Maybe Int)
answerIO = join
    . maybeMaximum
    . M.mapWithKey (\ k -> join . maybeMaximum . uncurry (liftM2 (f1 k)))
    <$> resultsIO

-- main :: IO ()
-- main = print "hello"

main :: IO ()
main = answerIO >>= print
