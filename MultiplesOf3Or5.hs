{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

sumMultiples :: Integer -> Integer
sumMultiples n = sum [x | x <- [1 .. n-1], x `mod` 3 == 0 || x `mod` 5 == 0]

-- faster solution
sumMultiples' :: Integer -> Integer
sumMultiples' n = sum [3, 6 .. n-1] + sum [5, 10 .. n-1] - sum [15, 30 .. n-1]
