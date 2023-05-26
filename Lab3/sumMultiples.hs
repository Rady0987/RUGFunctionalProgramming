{--Function mults takes a positive integer n as its input, 
and produces as its output the infinite list of multiples 
of n, so mults 3 should yield the infinite list 
[3,6,9,12,15,18,...]

Function multiples takes as its input a finite list of positive 
integers, and produces the infinite list of multiples of these 
integers, i.e. multiples [3,5] should yield 
[3,5,6,9,10,12,15,18,20,....]

Function concatSort takes 2 lists and sorts the elements of 
the second one in the first, in the program it is used in foldr, 
so it takes concatenates the lists of multiples and sorts their 
elements--}

mults :: Integer -> [Integer]
mults n = f n n
   where 
      f n m = m : f n (m + n)

concatSort :: [Integer] -> [Integer] -> [Integer]
concatSort [] ys = ys
concatSort xs [] = xs
concatSort (x:xs) (y:ys)
   | x > y = y : concatSort (x:xs) ys
   | x < y = x : concatSort xs (y:ys)
   | otherwise = x : concatSort xs ys

multiples :: [Integer] -> [Integer]
multiples xs = foldr concatSort [] (map mults xs)

multsum :: Integer -> [Integer] -> Integer
multsum n xs = sum (takeWhile (< n) (multiples xs))