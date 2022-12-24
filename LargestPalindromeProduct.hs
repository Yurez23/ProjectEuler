{-
A palindromic number reads the same both ways. 
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

largestPalindrome' :: Integer
largestPalindrome' = maximum [x * y | x <- [100..999], y <- [x..999], isPalindrome (x * y)]
  where    
    isPalindrome n = show n == reverse (show n)