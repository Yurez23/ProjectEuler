{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

pythagoreanTriplets :: [(Integer, Integer, Integer)]
pythagoreanTriplets = [(a, b, 1000 - a - b) | b <- [1..499], a <- [1..b-1], 1000*a + 1000*b - a*b == 500000]

-- a^2 + b^2 == c^2
-- a + b + c == 1000
-- c == 1000 - a - b

-- a^2 + b^2 == (1000 - a - b)^2
-- a^2 + b^2 == 1000^2 + a^2 + b^2 - 2000*a - 2000*b + 2*a*b
-- 2000*a + 2000*b - 2*a*b == 1000000 
-- 1000*a + 1000*b - a*b == 500000

answer :: Integer
answer = (\(a, b, c) -> a * b * c) $ head pythagoreanTriplets