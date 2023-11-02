module Main where

import Data.Bits

import Data.List (group, sort)
import Data.List.Split

import Data.Function (on)


encrypt :: Char -> Char -> Char
-- xor encrypt and decrypt is identical.
encrypt k = toEnum . (xor `on` fromEnum) k

decrypt :: Char -> Char -> Char
decrypt = encrypt

toChar :: Int -> Char
toChar = toEnum

toOccList :: (Ord a) => [a] -> [(a, Int)]
toOccList = map (\l -> (head l, length l)) . group . sort

-- data Count = Zero | One | Two
-- 
-- splitThree :: [a] -> ([a], [a], [a])
-- splitThree l = (splitter Zero l, splitter One l, splitter Two l)
--   where
--     splitter _    []     = []
--     splitter Zero (a:as) = a : splitter One  as
--     splitter One  (_:as) =     splitter Two  as
--     splitter Two  (_:as) =     splitter Zero as

mapFst3 :: (a -> x) -> (a, b, c) -> (x, b, c)
mapFst3 f (a, b, c) = (f a, b, c)
{-# INLINE mapFst3 #-}

mapSnd3 :: (b -> y) -> (a, b, c) -> (a, y, c)
mapSnd3 f (a, b, c) = (a, f b, c)
{-# INLINE mapSnd3 #-}

mapThd3 :: (c -> z) -> (a, b, c) -> (a, b, z)
mapThd3 f (a, b, c) = (a, b, f c)
{-# INLINE mapThd3 #-}


splitThree :: [a] -> ([a], [a], [a])
splitThree = foldr f ([], [], [])
  where
    f a (ls, ms, rs) = (a : rs, ls, ms)

combineThree :: ([a], [a], [a]) -> [a]
combineThree (x, y, z) = combiner x y z
  where
    combiner []     _      _      = []
    combiner (a:as) []     []     = a : combiner as [] []
    combiner (a:as) (b:bs) []     = a : b : combiner as bs []
    combiner (a:as) (b:bs) (c:cs) = a : b : c : combiner as bs cs
    combiner _      _      _      = errorWithoutStackTrace "combiner: should not happen"

parseString :: String -> [Int]
parseString = map read . splitOn ","

-- "exp" is the key after some frequency analysis.
main :: IO ()
main = do
    file <- readFile "p059_cipher.txt"
    let (a', b', c') = splitThree $ parseString file
--         (a, b, c) = (toOccList a', toOccList b', toOccList c')
        plaintext = combineThree ( map (decrypt 'e' . toChar) a',
                                   map (decrypt 'x' . toChar) b',
                                   map (decrypt 'p' . toChar) c' )
    print plaintext
    print $ sum $ map fromEnum plaintext
