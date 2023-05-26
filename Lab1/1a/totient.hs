{--This program returns the value of Euler’s totient function for a given n.--}

phi :: Integer -> Integer -> Integer
phi n p
   | n `mod` p /= 0 = n
   | otherwise = phi (n `div` p) p

euler :: Integer -> Integer -> Integer -> (Integer, Integer)
euler n p result
   | p * p <= n && n `mod` p == 0 = alabala
   | p * p <= n = euler n (p + 1) result
   | otherwise = (result, n)
   where alabala = euler (phi n p) (p + 1) (result - (result `div` p))

totient :: Integer -> Integer
totient n 
   | b > 1 = a - (a `div` b)
   | otherwise = a
   where result = euler n 2 n
         a = fst result
         b = snd result
