module Main where
import Data.List.Split (splitOn)

elemOrd :: (Integral a, Ord b) => b -> (a -> b) -> Bool
-- Binary search on monotonic functions
elemOrd k f = uncurry elemOrd' bound
  where
    bound = (0, genUpperBound 100)
      where
        genUpperBound b = if k <= f b then b else genUpperBound ((b+1)*2)
    elemOrd' l r
      | l == r = f m == k
      | k <  y = elemOrd' l (m-1)
      | k >= y = elemOrd' m r
      where (s, t) = (l + r) `quotRem` 2
            m = s + t
            y = f m

triangleNumbers :: Int -> Int
triangleNumbers n = n * (n+1) `quot` 2

wordToNumber :: String -> Int
wordToNumber = sum . map (subtract 64 . fromEnum)

main :: IO ()
main = do
        file <- readFile "p042_words.txt"
        let list :: [String]
            list = map read (splitOn "," file)
            results = filter (\w -> wordToNumber w `elemOrd` triangleNumbers) list
        print results
        print $ length results
