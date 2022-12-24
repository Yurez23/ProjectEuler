{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

nthPrime :: Int -> Integer
nthPrime n = primes !! (n - 1)
  where
    primes = 2 : filter isPrime [3, 5 ..]
    isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile (\x -> x * x <= n) primes

answer :: Integer
answer = nthPrime 10001