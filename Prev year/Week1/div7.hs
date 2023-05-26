{--As we cannot use mod function, we have 2 base cases and a recursive call, 
that counts the number of steps and makes use of special formula--}
div7 :: Integer -> Integer
div7 n 
   | n <= 7    = 0
   | n == 49   = 0
   | otherwise = 1 + div7 m
    where m = (div n 10) + 5 * (mod n 10)