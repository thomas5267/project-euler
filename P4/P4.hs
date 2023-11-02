largest_palindrome :: Integer
largest_palindrome = maximum $ filter isPalindrome list_of_products

largest_palindrome2 :: Integer
largest_palindrome2 = maximum $ filter isPalindrome2 list_of_products

largest_palindrome3 :: Integer
largest_palindrome3 = maximum $ filter isPalindrome3 list_of_products

list_of_products = [ x*y | x <- [100..999], y <- [x..999] ]


helper :: Integer -> (Integer, Integer)
helper n = helper' (n, 0)
    where helper' (m, e)
            | (-10) < m && m < 10 = (m, e)
            | otherwise = helper' (m `div` 10, e+1)

mostSignificantDigit :: Integer -> Integer
mostSignificantDigit = fst . helper

eExp :: Integer -> Integer
-- eExp returns the number of digits minus 1
-- eExp 100  = eExp (1E2) = 2
-- eExp 2000 = eExp (2E3) = 3
eExp = snd . helper

digits :: Integer -> Integer
digits n = eExp n + 1

-- isPalindrome2 is the fastest and easiest to write
-- Why bother?

isPalindrome :: Integer -> Bool
isPalindrome n
  | n < 0     = False
  | otherwise = isPalindrome' (eExp n) n
  -- m is most significant digit
  -- l is least significant digit
  -- e is exponent
    where isPalindrome' e a
            | e <= 0 = True
            | a `div` (10^e) == l = isPalindrome' (e-2) (a `mod` 10^e `div` 10)
            | otherwise = False
              where m = mostSignificantDigit a
                    l = a `mod` 10

isPalindrome2 :: Integer -> Bool
isPalindrome2 n = n == r
  where r = (read . reverse . show) n


isPalindrome3 :: Integer -> Bool
isPalindrome3 n = n == r
  where r = mirror n
        mirror n = mirror' n 0
          where mirror' 0 b = b
                mirror' a b = mirror' q (10*b + r)
                  where (q, r) = a `divMod` 10
