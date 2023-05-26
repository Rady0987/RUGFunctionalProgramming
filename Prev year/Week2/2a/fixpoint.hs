{--This is the main function that calls the helper 
one with the starting value and input function--}
fixpoint :: (Integer -> Integer) -> Integer
fixpoint f = helper f 0

{--This function increments x by 1 every iteration 
and checks recursively the truth state--}
helper :: (Integer -> Integer) -> Integer -> Integer
helper f x
   | f x == x = x
   | otherwise = helper f (x + 1)