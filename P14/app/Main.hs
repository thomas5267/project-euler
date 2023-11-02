module Main where

import qualified Data.Vector as V (Vector, fromList, maxIndex, take, tail, (!))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Criterion.Main

collatz :: Int -> Int
collatz n
  | n <= 0    = error "collatz: non-positive number"
  | even n    = n `div` 2
  | otherwise = 3*n + 1

stoppingTime :: Int -> V.Vector Int
stoppingTime n = V.take n $ V.tail memo
  where
    memo :: V.Vector Int
    memo = V.fromList $ 0 : 1 : map stoppingTime' [2..fromIntegral n]
    stoppingTime' :: Integer -> Int
    stoppingTime' k
      | r == 0    = if q < fromIntegral n then 1 + memo V.! fromIntegral q else 1 + stoppingTime' q
      | otherwise = if t < fromIntegral n then 2 + memo V.! fromIntegral t else 2 + stoppingTime' t
      where
        (q, r) = k `quotRem` 2
        t = 3*q + 2

stoppingTimeInt :: Int -> V.Vector Int
stoppingTimeInt n = V.take n $ V.tail memo
  where
    memo :: V.Vector Int
    memo = V.fromList $ 0 : 1 : map stoppingTimeInt' [2..n]
    stoppingTimeInt' :: Int -> Int
    stoppingTimeInt' k
      | r == 0    = if q < n then 1 + memo V.! q else 1 + stoppingTimeInt' q
      | otherwise = if t < n then 2 + memo V.! t else 2 + stoppingTimeInt' t
      where
        (q, r) = k `quotRem` 2
        t = 3*q + 2

stoppingTimeNaive :: Integer -> Integer
stoppingTimeNaive = stoppingTimeNaive' 1
  where stoppingTimeNaive' s 1 = s
        stoppingTimeNaive' s n
          | even n    = stoppingTimeNaive' (s+1) (n `div` 2)
          | otherwise = stoppingTimeNaive' (s+2) ((3*n + 1) `div` 2)

stoppingTimeTR :: Integer -> Integer
stoppingTimeTR = stoppingTimeTR' 1
  where stoppingTimeTR' s 1 = s
        stoppingTimeTR' s n
          | r == 0    = stoppingTimeTR' (s+1) q
          | otherwise = stoppingTimeTR' (s+2) (3*q + 2)
            where (q, r) = n `quotRem` 2

stoppingTimeTRInt :: Int -> Int
stoppingTimeTRInt = stoppingTimeTRInt' 1
  where stoppingTimeTRInt' s 1 = s
        stoppingTimeTRInt' s n
          | r == 0    = stoppingTimeTRInt' (s+1) q
          | otherwise = stoppingTimeTRInt' (s+2) (3*q + 2)
            where (q, r) = n `quotRem` 2

g :: Integer -> [Integer]
g x = map stoppingTimeNaive [1..x]

maximumWithIndex :: Ord t => [t] -> (t, Int)
maximumWithIndex [] = error "maximumWithIndex: empty list"
maximumWithIndex (x:xs) = maximumWithIndex' (x, 0) 0 xs
  where
    maximumWithIndex' p        _ []  = p
    maximumWithIndex' p@(m, _) n (y:ys)
      | y > m     = maximumWithIndex' (y, n+1) (n+1) ys
      | otherwise = maximumWithIndex' p        (n+1) ys

answerWithFunc :: (Integral a) => (a -> a) -> Int -> (a, Int)
answerWithFunc f n = (\(x, y) -> (x, y+1)) $ maximumWithIndex $ map f [1..(fromIntegral n)]

answerWithFuncZip :: (Integral a) => (a -> a) -> Int -> (a, Int)
answerWithFuncZip f n = maximumBy (comparing fst) $ zip (map f [1..(fromIntegral n)]) [1..]

answerWithVector :: (Int -> V.Vector Int) -> Int -> (Int, Int)
answerWithVector f n = let v = f n; i = V.maxIndex v in (v V.! i, i+1)

bound :: Int
bound = 1000000

-- main = print test
--
-- main = defaultMain [
--   bgroup "stoppingTime" [ bench "10000"    $ nf answerWithstoppingTime 10000
--                         , bench "100000"   $ nf answerWithstoppingTime 100000
--                         , bench "1000000"  $ nf answerWithstoppingTime 1000000
--                         , bench "10000000" $ nf answerWithstoppingTime 10000000
--                         ],
--   bgroup "stoppingTime3" [ bench "10000"    $ nf (answerWithFunc stoppingTime3) 10000
--                          , bench "100000"   $ nf (answerWithFunc stoppingTime3) 100000
--                          , bench "1000000"  $ nf (answerWithFunc stoppingTime3) 1000000
--                          , bench "10000000" $ nf (answerWithFunc stoppingTime3) 10000000
--                          ],
--   bgroup "stoppingTime3 w/zip" [ bench "10000"    $ nf (answerWithFuncZip stoppingTime3) 10000
--                                , bench "100000"   $ nf (answerWithFuncZip stoppingTime3) 100000
--                                , bench "1000000"  $ nf (answerWithFuncZip stoppingTime3) 1000000
--                                , bench "10000000" $ nf (answerWithFuncZip stoppingTime3) 10000000
--                                ]
--   ]

main :: IO ()
main = defaultMain [
  bgroup "stoppingTime" [ bench "10000"    $ nf (answerWithVector stoppingTime) 10000
                        , bench "100000"   $ nf (answerWithVector stoppingTime) 100000
                        , bench "1000000"  $ nf (answerWithVector stoppingTime) 1000000
                        , bench "10000000" $ nf (answerWithVector stoppingTime) 10000000
                        ],
  bgroup "stoppingTimeInt" [ bench "10000"    $ nf (answerWithVector stoppingTimeInt) 10000
                           , bench "100000"   $ nf (answerWithVector stoppingTimeInt) 100000
                           , bench "1000000"  $ nf (answerWithVector stoppingTimeInt) 1000000
                           , bench "10000000" $ nf (answerWithVector stoppingTimeInt) 10000000
                           ],
  bgroup "stoppingTimeTR" [ bench "10000"    $ nf (answerWithFunc stoppingTimeTR) 10000
                          , bench "100000"   $ nf (answerWithFunc stoppingTimeTR) 100000
                          , bench "1000000"  $ nf (answerWithFunc stoppingTimeTR) 1000000
                          , bench "10000000" $ nf (answerWithFunc stoppingTimeTR) 10000000
                          ],
  bgroup "stoppingTimeTRInt" [ bench "10000"    $ nf (answerWithFunc stoppingTimeTRInt) 10000
                             , bench "100000"   $ nf (answerWithFunc stoppingTimeTRInt) 100000
                             , bench "1000000"  $ nf (answerWithFunc stoppingTimeTRInt) 1000000
                             , bench "10000000" $ nf (answerWithFunc stoppingTimeTRInt) 10000000
                             ]
  ]
