
fiboSums :: Integer -> Integer -> Integer -> Integer -> Integer
fiboSums prevFib fib ways goal
   | goal == 0 = 1
   | fib > goal = 0
   | otherwise = fiboSums fib (fib + prevFib) ways  goal + fiboSums fib (fib + prevFib) (ways + 1) (goal - fib)


numberOfFiboSums :: Integer -> Integer
numberOfFiboSums n = fiboSums 1 1 0 n
