{--This program outputs the number of happy numbers in the 
interval [a,b]--}

numDigits :: Integer -> [Integer]
numDigits n 
   | n < 10 = [n]
   | otherwise = numDigits (n `div` 10) ++ [n `mod` 10]

sumOfSquares :: [Integer] -> Integer
sumOfSquares (x:xs)
   | length xs == 0 = double
   | otherwise = sumOfSquares xs + double
   where double = x ^ 2
 
happyCheck :: Integer -> Bool
happyCheck 1 = True
happyCheck 7 = True
happyCheck 4 = False 
happyCheck n 
   | otherwise = happyCheck (sumOfSquares (numDigits n))

countHappyNumbers :: Integer -> Integer -> Int
countHappyNumbers a b 
   | a > b = 0
   | happyCheck a = nextNumber + 1
   | otherwise = nextNumber 
   where nextNumber = countHappyNumbers (a + 1) b
