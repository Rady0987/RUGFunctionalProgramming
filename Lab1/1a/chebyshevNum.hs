
chebyshev :: Integer -> Integer
chebyshev n = f n 0 2
   where 
      f 0 a b = a
      f n a b = f (n-1) b (4 * b - a)