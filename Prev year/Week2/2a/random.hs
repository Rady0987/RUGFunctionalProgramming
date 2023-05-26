{--This function computes an infinite list with pseudo random numbers--}
random :: [Integer]
random = map f [0..]
   where 
      f 0 = 2773639
      f n = (25214903917 * f (n - 1) + 11) `mod` 2^45
