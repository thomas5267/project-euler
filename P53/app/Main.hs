module Main where

bound :: Int
bound = 1000000

choose :: Int -> Int -> Int
choose 1 1 = 1
choose _ 0 = 1
choose n k
  | n <= 0 = 0
  | n == k = 1
  | k >  n = 0
  | otherwise = fast_choose (n-1) (k-1) + fast_choose (n-1) k

fast_choose :: Int -> Int -> Int
fast_choose n k = memo_choose !! n !! k

memo_choose :: [[Int]]
memo_choose = map (\n -> map (choose n) [0..]) [0..]

chooseGood :: Int -> Int -> Bool
chooseGood 1 1 = False
chooseGood _ 0 = False
chooseGood n k
  | n <= 0 = False
  | n == k = False
  | k >  n = False
  | otherwise = fast_choose_good (n-1) (k-1) || fast_choose_good (n-1) k ||
                choose n k > bound

fast_choose_good :: Int -> Int -> Bool
fast_choose_good n k = memo_choose_good !! n !! k

memo_choose_good :: [[Bool]]
memo_choose_good = map (\n -> map (chooseGood n) [0..]) [0..]

result :: [((Int, Int), Int)]
result = filter ((>1000000) . snd) [ ((n, k), choose n k) | n <- [1..100], k <- [1..n] ]

main :: IO ()
main = print $ length result
