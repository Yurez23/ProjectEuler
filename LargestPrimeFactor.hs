{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}


-- simple solution
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = helper 2 n
  where
    helper d m
      | m < d * d = m
      | m `mod` d == 0 = helper d (m `div` d)
      | otherwise = helper (d + 1) m


-- a bit faster solution
leastFactor :: Integer -> Integer
leastFactor n = go 2
  where
    go d
      | d * d > n = n
      | n `mod` d == 0 = d
      | otherwise = go (d + 1)

largestPrimeFactor' :: Integer -> Integer
largestPrimeFactor' n
  | n == 1 = 1
  | otherwise =
    let f = leastFactor n
     in if f == n then n else largestPrimeFactor' (n `div` f)

{-
>>> largestPrimeFactor 13195
>>> largestPrimeFactor' 13195
29
29
>>> largestPrimeFactor 600851475143
>>> largestPrimeFactor' 600851475143
6857
6857
-}

answer :: Integer
answer = largestPrimeFactor' 600851475143