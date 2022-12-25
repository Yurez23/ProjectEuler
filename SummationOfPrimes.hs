{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}
import Data.List.Ordered ( minus, unionAll )

-- An honestly stolen solution from here:
-- https://wiki.haskell.org/Prime_numbers
-- https://wiki.haskell.org/Prime_numbers_miscellaneous#A_Tale_of_Sieves
primes :: [Integer]
primes = 2 : 3 : [5, 7 ..] `minus` unionAll [[p * p, p * p + 2 * p ..] | p <- tail primes]

answer :: Integer
answer = sum $ takeWhile (< 2000000) primes

