{--This program returns the sum
of all prime numbers that are less or equal to n.--}


helper :: Integer -> Integer -> Bool
helper n i
   | n `mod` i == 0 && n /= 3 = False
   | i * i >= n = True
   |otherwise = helper n (i + 2)

prime :: Integer -> Bool
prime n 
   |n == 1 = False
   |n `mod` 2 == 0 && n > 2 = False
   |helper n 3 == False = False
   |otherwise = True

primes :: Integer -> [Integer]
primes n = [p | p <- [1 .. n], prime p]

sumPrimesTo :: Integer -> Integer
sumPrimesTo n = sum (primes n)
