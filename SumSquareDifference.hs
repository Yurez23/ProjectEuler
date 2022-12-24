{-
Find the difference between the sum of the squares 
of the first one hundred natural numbers and the square of the sum.
-}

-- danger div, but still works
sumSquareDifference :: Integer -> Integer
sumSquareDifference n = (n^4) `div` 4 + (n^3) `div` 6 - (n^2) `div` 4 - n `div` 6

-- safe div
sumSquareDifference' :: Integer -> Integer
sumSquareDifference' n = (n * (n + 1) `div` 2) ^ 2 - (n * (n + 1) * (2 * n + 1) `div` 6)

answer :: Integer
answer = sumSquareDifference 100

-- for checking for correctness
check :: Integer -> Integer
check n = (sum [x | x <- [1..n]]) ^ 2 - sum [x^2 | x <- [1..n]]

{-
>>> map sumSquareDifference [1..10]
>>> map sumSquareDifference' [1..10]
>>> map check [1..10]
[0,4,22,70,170,350,644,1092,1740,2640]
[0,4,22,70,170,350,644,1092,1740,2640]
[0,4,22,70,170,350,644,1092,1740,2640]
-}
