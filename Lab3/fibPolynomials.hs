fibonacci :: Integer -> Integer
fibonacci n = f n 0 1
   where
      f 0 a b = a
      f n a b = f (n-1) b (a +b)

polynomialSum :: Integer -> Integer -> Integer -> Integer
polynomialSum n x i
   | i == n = polynom
   | otherwise = polynom + polynomialSum n x (i + 1)
   where
      polynom = (fibonacci i) * (x ^ i)


fibPolynomial :: Int -> Integer -> Integer 
fibPolynomial n x = polynomialSum (toInteger n) x 0

huinea :: Bool -> Int
huinea True = 1
huinea Flase = 0