answer :: Integer
answer = head $ applyN 10000 sieve [2..]
-- answer = take 10001 (fix sieve [2..])

primes = sieve (2:[3,5..])
sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : (filter (\x -> (x < p^2) || (x `mod` p /= 0)) xs)
sieve []     = []

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)
