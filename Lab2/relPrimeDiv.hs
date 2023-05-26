import Data.List 

{--This program outputs an infinite list of numbers n such
that n and d(n) are relatively prime. Let d(n) be the number 
of divisors of n, e.g. d(12) = 6 since the divisors of 12 
are 1,2,3,4,6,and 12.--}

factors :: Integer -> [Integer]
factors n = [d | d <- [1 .. n `div` 2], n `mod` d == 0] ++ [n]

relPrimeDiv :: [Integer]
relPrimeDiv = f 1 
   where
      f m
         | gcd (fromIntegral (length (factors m))) m == 1 = m : f (m + 1)
         | otherwise = f (m + 1)