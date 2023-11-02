module Main where

import qualified Data.PQueue.Prio.Min as PQ
import Data.List (takeWhile)

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve' xs (insertPrime x xs PQ.empty)
  where
    insertPrime p xs table = PQ.insert (p*p) (map (* p) xs) table
    sieve' :: [Integer] -> PQ.MinPQueue Integer [Integer] -> [Integer]
    sieve' [] table = []
    sieve' (x:xs) table
      -- If x is the smallest element of the table, then it is a composite.
      -- Therefore go to next number and adjust the table of composites.
      -- If x is not the smallest element, then it is a prime.
      -- Therefore multiply the rest of the candidate list with this prime
      -- and add to the table of composites.
      | nextComposite <= x = sieve' xs (adjust table)
      -- nextComposite < x does not happen
      | otherwise          = x : sieve' xs (insertPrime x xs table)
        where
          nextComposite = (fst . PQ.findMin) table
          -- When insertPrime is called,
          -- the key-value pair inserted is (p^2, l),
          -- where l consists of multiples of p in the candidate list and some extra multiples of p.
          -- adjust decides whether a key-value pair needs "rotating",
          -- and rotates an k-v pair (n, n':ns) to (n', ns).
          -- A k-v pair needs rotating when x is the smallest element in the table
          -- and hence is composite.
          --
          -- table acts like a stack of composite numbers,
          -- except there is a stack for each prime number,
          -- and if a candidate number is on one of the stacks,
          -- we check all the stacks and pop if the candidate number is on the stack.
          adjust table
            | n == x    = adjust (deleteMinAndInsert n' ns table)
            -- n < x does not happen
            | otherwise = table
              where
                (n, n':ns) = PQ.findMin table
                deleteMinAndInsert n' ns table = PQ.insert n' ns (PQ.deleteMin table)

primes = 2 : sieve [3,5..]
answer = sum $ takeWhile (<2000000) primes

main :: IO ()
main = print answer
