smallest_multiple :: Integer
smallest_multiple = foldr1 f [1..20]

f :: Integer -> Integer -> Integer
f m acc
  | acc `rem` m == 0 = acc
  | otherwise    = lcm acc m
