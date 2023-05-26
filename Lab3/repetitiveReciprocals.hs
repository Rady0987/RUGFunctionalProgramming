{--This program given two arguments m and
n, returns the (smallest) number from the interval 
[m..n] that has the longest repetition β.
In general, the number 1/n can be written as 0.αβ∗, 
where α is the non-repeating prefix, and β is the repetitive part of
the decimal representation.--}

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

repetitionBeta :: Integer -> Integer
repetitionBeta n = f n 1
   where 
      f n i 
         | i == n = n - 1
         | (10 ^ i) `mod` n == 1 = i
         | otherwise = f n (i + 1) 

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort below ++ [x] ++ quickSort above
   where
      below = [y | y <- xs, y <= x]
      above = [y | y <- xs, x < y]

fermatTuples :: Integer -> Integer -> [(Integer, Integer)]
fermatTuples m n
   | m > n = []
   | prime m = [(repetitionBeta m, m)] ++ fermatTuples (m + 1) n
   | otherwise = fermatTuples (m + 1) n

longestRepetition :: Int -> Int -> Int
longestRepetition m n = fromInteger (snd (last (quickSort (fermatTuples m' n'))))
   where 
      m' = toInteger m
      n' = toInteger n

