{-
2520 is the smallest number that can be divided 
by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

import Data.List (foldl')
import Data.Map (toList, empty, insertWith)

smallestMultiple :: Integer -> Integer
smallestMultiple n = foldl' lcm 1 [1..n]

answer :: Integer
answer = smallestMultiple 20