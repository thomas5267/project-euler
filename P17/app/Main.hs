{-# LANGUAGE BangPatterns #-}
module Main (main) where


f :: Int -> Int
f = go 0
  where
    go :: Int -> Int -> Int
    go !a n
      | n == 1000 = 11 -- "one thousand" has 6 letters
      | q >= 1  = case r of
                    0 -> go 0 q + 7  -- +7 from x "hundred and" y
                    _ -> go (go 0 q + 10) r  -- +10 from x "hundred and" y
      | n >= 90 = go (a+6) (n-90) -- "ninety"
      | n >= 80 = go (a+6) (n-80) -- "eighty"
      | n >= 70 = go (a+7) (n-70) -- "seventy"
      | n >= 60 = go (a+5) (n-60) -- "sixty"
      | n >= 50 = go (a+5) (n-50) -- "fifty"
      | n >= 40 = go (a+5) (n-40) -- "forty"
      | n >= 30 = go (a+6) (n-30) -- "thirty"
      | n >= 20 = go (a+6) (n-20) -- "twenty"
      | n == 19 = go (a+8) (n-19) -- "nineteen"
      | n == 18 = go (a+8) (n-18) -- "eighteen"
      | n == 17 = go (a+9) (n-17) -- "seventeen"
      | n == 16 = go (a+7) (n-16) -- "sixteen"
      | n == 15 = go (a+7) (n-15) -- "fifteen"
      | n == 14 = go (a+8) (n-14) -- "fourteen"
      | n == 13 = go (a+8) (n-13) -- "thirteen"
      | n == 12 = go (a+6) (n-12) -- "twelve"
      | n == 11 = go (a+6) (n-11) -- "eleven"
      | n == 10 = go (a+3) (n-10) -- "ten"
      | n == 9  = a + 4 -- "nine"
      | n == 8  = a + 5 -- "eight"
      | n == 7  = a + 5 -- "seven"
      | n == 6  = a + 3 -- "six"
      | n == 5  = a + 4 -- "five"
      | n == 4  = a + 4 -- "four"
      | n == 3  = a + 5 -- "three"
      | n == 2  = a + 3 -- "two"
      | n == 1  = a + 3 -- "one"
      | n == 0  = a
      | otherwise = undefined
      where
        (q, r) = n `quotRem` 100

main :: IO ()
main = print "hello"
