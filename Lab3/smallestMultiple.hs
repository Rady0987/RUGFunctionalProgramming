{--This program returns the smallest number than can 
be divided by each of the numbers from 1 to n without 
any remainder. The program makes use of foldr.

Example:
1 `lcm` 2 `lcm` 3 `lcm` ... `lcm` 10 --}

leastCommonMultiple :: [Integer] -> Integer
leastCommonMultiple arr = foldr lcm 1 arr

smallestMultiple :: Integer -> Integer
smallestMultiple n = leastCommonMultiple [1..n]