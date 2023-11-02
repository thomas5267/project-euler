import Data.List (maximumBy)
import Data.Ord (comparing)
import Criterion.Main


collatz :: Int -> [Int] -> [Int]
collatz num lst | num == 1  = 1:lst
                | even num  = collatz (div num 2) (num:lst)
                | otherwise = collatz (3*num + 1) (num:lst)

collatzLength :: Int -> (Int, Int)
collatzLength max = maximumBy (comparing fst) $ [(length $ collatz x [], x) | x <- [2..max]]

main :: IO ()
main = defaultMain [
  bgroup "collatzLength" [ bench "10000"    $ nf collatzLength 10000
                         , bench "100000"   $ nf collatzLength 100000
                         , bench "1000000"  $ nf collatzLength 1000000
                         , bench "10000000" $ nf collatzLength 10000000
                         ]
  ]
