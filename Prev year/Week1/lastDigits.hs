{--The function that uses the modular exponentiation to compute 'mod' 
for larger numbers--}
modularExp :: Integer -> Integer -> Integer -> Integer
modularExp b 0 n = 1
modularExp b e n 
   | e `mod` 2 == 0 = (x * x) `mod` n
   | otherwise = (b * x * x) `mod` n
    where x = modularExp b (e `div` 2) n 

{--This function computes the power in a memory efficient way, if n is odd or even--}
power:: Integer -> Int -> Integer 
power x 1 = x
power 0 p = 0
power x p  
   | odd p  = x * power x (p - 1) 
   | even p = power x division * power x division
    where division = p `div`  2

{--This function transforms an integer to a list with that integer as an element--}
intList :: Integer -> [Integer] 
intList 0 = []
intList x = intList (x `div` 10) ++ [x `mod` 10]

{--This function calculates the sum of the list (a^a) --}
listSum :: Integer -> Int -> Integer
listSum length p = sum[modularExp a a (power 10 (p + 1)) | a <- [1..length]]

{--The main function that drops the first digits--}
lastDigits :: Integer -> Int -> [Integer] 
lastDigits n d
   | (n == 0) = [] 
   | otherwise = drop (length((intList (listSum n d))) - d ) (intList (listSum n d)) 