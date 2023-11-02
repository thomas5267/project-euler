module Main (main) where

import Data.Numbers.Primes
import Euler.Helper

import Data.Int

import Data.List (elemIndices, find)

import Control.Monad (foldM)
import Data.Either

bound :: Int
bound = 10 ^ (6 :: Int)

-- Is it possible for a single digit replacement to yield 8 primes?
-- Let p be a prime such that varying a digit d will result in 8 primes.
-- d cannot be the last digit of p.
-- When d is even, so is p.
-- There are at most 5 primes.
--
-- Now consider the divisibility of p by 3 when d is varied.
-- If the sum of digits of p is divisible by 3, then p is divisible by 3.
-- The possible values of d are partitioned into three equivalence classes.
-- d = {0, 3, 6, 9}  (d = 0 mod 3)
-- d = {1, 4, 7}     (d = 1 mod 3)
-- d = {2, 5, 8}     (d = 2 mod 3)
-- Let the sum of digits of p excluding d be s.
-- If s + d = 0 mod 3, then p is divisible by 3.
-- Hence, we can only choose d from two equivalent classes.
-- At most there are 7 possible primes.
--
-- So it is impossible for a single digit replacement to yield 8 primes.
--
-- Now consider the combinations of two-digit replacements.
-- d = {00, 33, 66, 99}  (d = 0 mod 3)
-- d = {22, 55, 88}      (d = 1 mod 3)
-- d = {11, 44, 77}      (d = 2 mod 3)
--
-- For the same reason it is not possible with two digits replacement either.
--
-- Now three-digit replacements.
-- d = {000, 111, 222, 333, 444, 555, 666, 777, 888, 999} (d = 0 mod 3)
-- Hence, if s = 1 or 2 (mod 3), then p can be a prime.

type Digit = Int8
type ReverseDigits = [Digit]

fromReverseDigits :: (Integral a) => ReverseDigits -> a
fromReverseDigits = foldr (\d a -> a * 10 + fromIntegral d) 0

nsd :: Int
-- Number of same digits
nsd = 3

nsol :: Digit
nsol = 8

-- We seek the minimal element of a set of nsol primes such that
-- the primes differ in exactly nsd identitcal digits.
-- E.g. [56003, 56113, 56333, 56443, 56663, 56773, 56993] (56XX3)
-- is such a set of primes with nsol = 7 and nsd = 2.
-- We will call such sets sets of digit replacement primes.
--
-- We start the search from the smallest prime with nsd identitcal digits,
-- and vary those digits to see if they are still primes.
-- Let p be a prime with nsd identitcal digits.
-- If the digit to be replaced is larger than 10-nsol,
-- then p cannot be the minimal element of a set of nsol primes.
-- E.g. 56773 cannot be the minimum element of a set of 7 primes
-- with 2 identitcal digits because the best case is
-- 56883 and 56993 are both primes but that is still only three primes.
--
-- In addition, suppose p is a minimal element of a set of digit replacement primes.
-- Let m be the largest element of such set.
-- If p < 10^k for some k, then m < 10^k.
-- Digit replacement cannot increase the number of digits.

ncomp :: Digit
-- Number of composites allowed in a set.
-- E.g. When nsol = 7, there can be at most 3 composites in the set.
ncomp = 10 - nsol

-- These divisibleByX functions act on a list of digits
-- with the least significant digits first.

divisibleBy2 :: ReverseDigits -> Bool
divisibleBy2 = even . head

divisibleBy5 :: ReverseDigits -> Bool
divisibleBy5 = (\d -> (d == 0) || (d == 5)) . head

isPrimeQuick :: Int -> Bool
isPrimeQuick = isPrime

type DigitChoices = [(Digit, [Int])]

data PrimeData = PrimeData
  { revDigits :: ReverseDigits
  , choices :: DigitChoices
  }
  deriving (Show, Eq, Ord)

digitChoices :: ReverseDigits -> DigitChoices
digitChoices = filter (not . null . snd) . genChoices
  where
    genChoices :: ReverseDigits -> DigitChoices
    genChoices ds =
      [(d, cs) | d <- [0 .. ncomp], cs <- chooseElems nsd (elemIndices d ds)]

chooseElems :: Int -> [a] -> [[a]]
-- Returns all possible combinations of choosing n elements from a list.
chooseElems 0 _ = [[]]
chooseElems _ [] = []
chooseElems c (x : xs) = map (x :) (chooseElems (c - 1) xs) ++ chooseElems c xs

primeData :: Int -> PrimeData
primeData p =
  PrimeData
    { revDigits = reverseD
    , choices = digitChoices reverseD
    }
  where
    reverseD = reverseDigits p

candidates :: [(Int, PrimeData)]
candidates = filter (not . null . choices . snd) $ map (toSnd primeData) inputPrimes
  where
    inputPrimes = takeWhile (<= bound) $ dropWhile (<= 1000) primes

replaceDigitsWith :: Digit -> ReverseDigits -> [Int] -> ReverseDigits
-- rd is the digit to replace with.
-- dl is a list of reversed digits
-- il is a list of indices indicating which digits of dl needs to be replaced.
-- Returns dl but with digits il replaced with rd.
-- E.g. replaceDigits 3 [9,1,1,1,1] [2,3,4] = [9,1,3,3,3],
-- i.e. returns the digits of 11119 but with the 2nd, 3rd, 4th digit replaced by 3.
-- Digits are counted from 0 and with the least-significant digit first.
replaceDigitsWith rd = go 0
  where
    go _ [] _ = []
    go _ dl [] = dl
    go c (d : ds) il@(i : is) =
      if c == i
        then rd : go (c + 1) ds is
        else d : go (c + 1) ds il

replaceDigits :: ReverseDigits -> Digit -> [Int] -> [ReverseDigits]
-- Given a reversed digit list and a list of digit indices to replace,
-- returns all possible replacement except itself.
-- Assumes rl is minimal element of a set of digit replacement primes.
replaceDigits ds d rl = map (\rd -> replaceDigitsWith rd ds rl) [(d + 1) .. 9]

allBut :: Int -> (a -> Bool) -> [a] -> Bool
allBut n p = fromLeft True . foldM go n
  where
    go k a
      | p a = Right k
      | k /= 0 = Right (k - 1)
      | otherwise = Left False

isMinimalElement :: (Int, PrimeData) -> Bool
isMinimalElement (_, pd) = any (isMinimalChoice . genReplacements) (choices pd)
  where
    genReplacements :: (Digit, [Int]) -> (Digit, [ReverseDigits])
    genReplacements (d, cs) = (d, replaceDigits (revDigits pd) d cs)

    isPrimeRevDigits ds =
      not (divisibleBy2 ds)
        && not (divisibleBy5 ds)
        && isPrimeQuick (fromReverseDigits ds)

    isMinimalChoice :: (Digit, [ReverseDigits]) -> Bool
    isMinimalChoice (d, ds) = allBut (fromIntegral (ncomp - d)) isPrimeRevDigits ds

main :: IO ()
main = print $ find isMinimalElement candidates
