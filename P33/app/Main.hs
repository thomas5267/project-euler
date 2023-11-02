module Main where
import Data.Char (digitToInt)
import Data.Ratio
import Data.List (foldl', (\\))

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)
{-# INLINE mapFst #-}

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
{-# INLINE mapSnd #-}

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
{-# INLINE toFst #-}

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

digits :: Int -> [Int]
digits n = map digitToInt (show n)

digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\k d -> 10*k + d) 0

numerators :: [Int]
numerators = filter (\n -> n `rem` 10 /= 0) [11..99]

genDenominators :: Int -> [Int]
genDenominators n
  | n <= 9 || n >= 100 = []
  | otherwise          = filter (> n) (genDenominators' f s)
  where
    l = digits n
    f = head l
    s = l !! 1
    genDenominators' a b
      | a == b    = [ a*10 + x | x <- [(b+1)..9] ]
      | otherwise = [ a*10 + x | x <- [(b+1)..9] ] ++
                    [ x*10 + a | x <- [a..9] ]     ++
                    [ b*10 + x | x <- [0..9] ]     ++
                    [ x*10 + b | x <- [(a+1)..9] ]

candidates :: [(Int, Int, Ratio Int)]
candidates = filter isSmall unfiltered
  where
    temp :: [(Int, [Int])]
    temp = map (toSnd genDenominators) numerators
    unfiltered :: [(Int, Int, Ratio Int)]
    unfiltered = concatMap (\(n, ds) -> map (\d -> (n, d, n % d)) ds) temp
    isSmall :: (Int, Int, Ratio Int) -> Bool
    isSmall (_, _, r) = (numerator r <= 9) && (denominator r <= 9)

results :: [(Int, Int, Ratio Int)]
results = filter isGood candidates
  where
    isGood (n, d, r) = d' /= 0 && (n' % d' == r)
      where
        dn = digits n
        dd = digits d
        n' = digitsToInt (dn \\ dd)
        d' = digitsToInt (dd \\ dn)

answer :: Int
answer = denominator (foldl' (\p (_, _, r) -> p*r) 1 results)

main :: IO ()
main = putStrLn "Hello, Haskell!"
