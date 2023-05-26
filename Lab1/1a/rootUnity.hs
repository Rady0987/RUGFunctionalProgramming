
{--The function that uses the modular exponentiation to compute 'mod' 
for larger numbers--}
modularExp :: Integer -> Integer -> Integer -> Integer
modularExp b 0 m = 1
modularExp b e m 
   | e `mod` 2 == 0 = (x * x) `mod` m
   | otherwise = (b * x * x) `mod` m
    where x = modularExp b (e `div` 2) m 

isNthRootOfUnity :: Integer -> Integer -> Integer -> Bool
isNthRootOfUnity x n m
   | modularExp x n m == 1 = True
   | otherwise = False 