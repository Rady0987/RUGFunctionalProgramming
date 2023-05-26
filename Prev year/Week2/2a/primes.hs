primes :: [Integer]
primes = 2: map (+1) (map (*2) (remove [1..]))

{--This function returns the list with numbers without 
those that do not meet the conditions--}
remove :: [Integer] -> [Integer]
remove (x:xs)
   | numValidation x 1 (div (x - 1) 3) == 1 = x : remove xs
   | otherwise = remove xs

{--This function checks if the input number is suitable for 
our list or not (i > j or n == formula)--}
numValidation :: Integer -> Integer -> Integer -> Integer
numValidation n i j
   | i > j                  = 1
   | i <= j && n == formula = 0
   | otherwise = numValidation n (i + 1) jNext
    where 
      jNext = (div (n - 1) (1 + 2 * (i + 1)))
      formula = i + j + 2 * i * j
       
