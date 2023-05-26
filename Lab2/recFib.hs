fib :: Integer -> [Integer]
fib n = f n 1 1
   where
      f 0 a b = [a]
      f n a b = a : f (n-1) b (a+b)

helper :: [Integer] -> Integer -> Integer -> Integer -> [Integer]
helper (fib:fibbs) n i a 
   | n == i = [next]
   | otherwise =  next : (helper fibbs n (i + 1) next)
      where next = a + 2 * (fib - i)

prevElem :: [Integer] -> Integer -> Integer -> Integer -> Integer
prevElem (fib:fibbs) n i a
   | n == i = next
   | otherwise =  prevElem fibbs n (i + 1) next
      where next = a + 2 * (fib - i)

skip :: [Integer] -> Integer -> Integer -> [Integer]
skip (x:xs) n i 
   | n == i = xs
   | otherwise = skip xs n (i + 1)

recfibs :: Int-> Int -> [Integer]
recfibs m n
   | m == 0 = 1 : helper (tail (fib n')) n' 1 1
   | m == 1 = helper (tail (fib n')) n' 1 1
   | otherwise = helper (skip (fib n') m' 1) n' m' a
      where a = prevElem (tail (fib m')) (m'- 1) 1 1
            m' = fromIntegral m
            n' = fromIntegral n