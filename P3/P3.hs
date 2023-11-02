{-

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

-}

import Data.List

prime_factors :: Integer -> [Integer]

prime_factors 1 = []
prime_factors n
	| null factors = [n]
	| otherwise = prime_factors (n `div` (head factors)) ++ factors
	where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. (floor . sqrt . fromIntegral $ n)]

result = prime_factors 600851475143 !! 0