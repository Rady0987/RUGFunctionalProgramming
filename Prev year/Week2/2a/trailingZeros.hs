{--This function computes an infinite list of number of 
zeros on the very right of the binary representation of n 
(i.e. the number of trailing zeros)--}

trailingZeros :: [Integer]
trailingZeros = tail seq 
   where 
      f (x:xs) = x : 0 : f xs
      seq = 0 : f (map (+1) seq)
      