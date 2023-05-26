{--This is the linear method to compute fibonacci number 
with just a different general formula--}
altFib :: Integer -> Integer
altFib n = f n 0 1
   where 
      f 0 a b = a
      f n a b = f (n - 1) ((-1) ^ (n + 1) * b) (a + b)