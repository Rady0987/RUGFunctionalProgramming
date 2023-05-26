
{--This program outputs the infinite list of compsites 
(i.e. non-primes) that are divisible by any of the first 
n primes. For example take 10 (composites 5) = 
= [4,6,8,9,10,12,14,15,16,18].--}

helper :: Integer -> Integer -> Bool
helper n i
   | n `mod` i == 0 && n /= 3 = False
   | i * i >= n = True
   |otherwise = helper n (i + 2)

isPrime :: Integer -> Bool
isPrime n 
   |n == 1 = False
   |n `mod` 2 == 0 && n > 2 = False
   |helper n 3 == False = False
   |otherwise = True

primes :: [Integer]
primes = sieve [2..]
   where
      sieve :: [Integer] -> [Integer]
      sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

check :: [Integer] -> Integer -> Bool
check [] n = False
check (x:xs) n 
   | x == n = False 
   | n `mod` x == 0 && n /= x = True 
   | otherwise = check xs n

composites :: Int -> [Integer]
composites n = f 2 n
   where
      f m n
         | not (isPrime m) && check (take n primes) m = m : f (m + 1) n
         | otherwise = f (m + 1) n