module Main where
import Data.Numbers.Primes
import Data.Maybe (fromJust)
import Data.List (find, foldl1')
import qualified Data.IntSet as IS

bound = 100
count = bound - 1


-- P29 asks for the number of distinct a^b for 2 <= a <= 100 and 2 <= b <= 100
-- Let a < c
-- We note the a^b /= c^d for any b and d if c /= a^k for some integral k
-- Therefore we split [2..100] into two parts,
-- the terrible part consisting of prime and prime powers, 6 and 36, and 10 and 100,
-- and the nice part which is the rest of [2..100]

isTerrible :: Int -> Bool
isTerrible 6   = True
isTerrible 36  = True
isTerrible 10  = True
isTerrible 100 = True
isTerrible n = isPrimePower n
  where
    p = fromJust $ find (\p -> n `rem` p == 0) primes
    isPrimePower k
      | k == 1    = True
      | r == 0    = isPrimePower q
      | otherwise = False
      where
        (q,r) = k `quotRem` p

niceList :: [Int]
niceList = filter (not . isTerrible) [2..100]

niceCount :: Int
niceCount = count * length niceList

terribleList :: [Int]
terribleList = 2 : 2 : foldr (helper 1) undefined primes
  where
    helper :: Int -> Int -> [Int] -> [Int]
    helper k p acc
      | p > bound    = []
      | p^(k+1) <= bound = helper (k+1) p acc
      | otherwise    = k : acc

-- terribleCounter :: Int -> Int
-- terribleCounter 1 = count
-- terribleCounter n = terribleCounter (n-1) + count - sum (map (countGen n) [1..(n-1)])
--   where
--     countGen :: Int -> Int -> Int
--     countGen n 1 = countMultiple (2*n) 100 n
--     countGen n k = countMultiple (100*(k-1) + 1) (100*k) (lcm k n)

terribleSetList :: [IS.IntSet]
terribleSetList = map setBuilder [1..]
  where
    setBuilder 1 = IS.fromAscList [2..bound]
    setBuilder n = IS.fromAscList [2*n,3*n..bound*n] `IS.union` (terribleSetList !! (n-2))
    -- terribleSetList !! 0 is IS.fromAscList [2..bound]
    -- setBuilder 2 = IS.fromAscList [2*n,3*n..bound*n] `IS.Union` IS.fromAscList [2..bound]
    -- 1 is at index 0

terribleCounter :: Int -> Int
terribleCounter n = IS.size $ terribleSetList !! (n-1)

terribleCount :: Int
terribleCount = sum $ map terribleCounter terribleList

decompose :: Int -> (Int, Int)
decompose n = decompose' n 0
  where
    p = fromJust $ find (\p -> n `rem` p == 0) primes
    decompose' k c
      | k == 1    = (p,c)
      | r == 0    = decompose' q (c+1)
      | otherwise = (n,1)
      where
        (q,r) = k `quotRem` p

test :: Int -> Int -> Int
-- Equivalent to length [k,2*k..u]
test u k = u `quot` k

countMultiple :: Int -> Int -> Int -> Int
-- Equivalent to length [f,f+k..u]
-- where f is the smallest integer divisible by k that is larger or equal to l.
countMultiple l u k = u `quot` k - (l-1) `quot` k

main :: IO ()
main = print (niceCount + terribleCount)
