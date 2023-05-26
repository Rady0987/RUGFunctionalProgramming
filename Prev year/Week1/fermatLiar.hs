{--The main function--}
fermatLiar :: Integer -> Integer
fermatLiar b = liarChecker b (b + 1)

{--The recursive function that increments the potential 
liar by 1 and calls the corresponding functions--}
liarChecker :: Integer -> Integer -> Integer
liarChecker b n 
   | isComposite n 3 && modularExp b (n - 1) n == 1 = n
   | otherwise = liarChecker b (n + 1)

{--The recursive function that checks if a number is composite--}
isComposite :: Integer -> Integer -> Bool
isComposite n i 
   | (i * i) > n = False
   | n `mod` i == 0 = True
   | otherwise = isComposite n (i + 1)

{--The function that uses the modular exponentiation to compute 'mod' 
for larger numbers--}
modularExp :: Integer -> Integer -> Integer -> Integer
modularExp b 0 n = 1
modularExp b e n 
   | e `mod` 2 == 0 = (x * x) `mod` n
   | otherwise = (b * x * x) `mod` n
    where x = modularExp b (e `div` 2) n 
