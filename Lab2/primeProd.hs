import Data.List 
{--This program outputs an infinite increasing list of numbers n that are
the product of two primes. For example, the first 10 numbers are 
[4,6,9,10,14,15,21,22,25,26].--}

factors :: Integer -> [Integer]
factors n = [d | d <- [1 .. n `div` 2], n `mod` d == 0]

check :: Integer -> [Integer] -> Bool
check n [] = False
check n (x:xs)
   | x ^ 2 == n = True
   | x * (head xs) == n && (head xs) `mod` x > 0 = True
   | otherwise = False 

primeProducts :: [Integer]
primeProducts = f 4
   where 
      f n 
         | check n (tail (factors n)) = n : f (n + 1)
         | otherwise = f(n + 1)