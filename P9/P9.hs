{-

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

-}

triples = [(1000-c-b,b,c) | c <- [1..1000], b <- [1..c], (1000-c-b)^2 + b^2 == c^2 && 0 < (1000-c-b) && (1000-c-b) < b && b < c]
multiply ((a,b,c):[]) = a * b * c
result = multiply triples