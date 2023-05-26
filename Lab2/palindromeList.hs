{--This program outputs an infinite increasing list 
of the number of nonzero palindromic numbers less 
than 10^n, where n starts from 1. Clearly, for n = 1 
these palindromes are the non-zero digits (i.e. a total of 9). 
For n = 2, it are the 1 digits palindromes together with the 
numbers 11, 22, 33, 44, 55, 66, 77, 88, 99. 
So, take 2 numPals should return [9,18].--}

count :: Integer -> Integer
count n = 9 * 10 ^ ((n - 1) `div` 2)

numPals :: [Integer]
numPals = f 1 0
   where 
      f n m = nums : f (n + 1) nums
       where nums = (count n) + m