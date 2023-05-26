{- Exercise 1: divisible by 7
 A helper function  steps is introduced having two parameters. The first is the number of steps,
 the second is n (the number to test) itself.
 -}
 
div7 :: Integer -> Integer
div7 n = steps 0 n
  where
    steps iter n
     | n <= 7 || n==49  = iter
     | otherwise        = steps (iter+1) (n `div` 10 + 5*(n `mod` 10))


--------------------------------------------------------------------------------------

{- Exercise 2: Alternating Fibonacci
   A helper function altfib with 4 arguments is used.
   The first being n, the second the corresponding sign (+1 or -1),
   the third a fourth are the previous fib numbers in the same style
   as standard fibonaci numbers are computed.
 -}
altFib :: Integer -> Integer
altFib x = altfib 0 1 0 1
  where
    altfib n sign a b
      | n == x    = a
      | otherwise = altfib (n+1) (-sign) (a+sign*b) a


--------------------------------------------------------------------------------------

{- Exercise 3: Fermat Liars
  Of course, you should never compute a^b fully if you need a^b `mod` m.
  E.g. for m=1000 you are only interested in the last 3 digits of a^b, so
  it is silly to compute something like 123^1000000 `mod` 100 by directly computing
  123^1000000. The following helper function expmod (modular exponentiation) is much much more efficient.
  Of course, we also need a helper function that checks whether a number is prime.
  Given these helper function, the function fermatliar is a direct implementation of the
  definition given in the exercise.
-}

expmod :: Integer -> Integer -> Integer -> Integer
expmod a 0 m = 1
expmod a n m
  | n `mod` 2 == 0   = expmod (a*a `mod` m) (n `div` 2) m
  | otherwise        = a*(expmod a (n-1) m) `mod` m

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = n>1 && n `mod` 2 == 1 && isP 3
  where
    isP d = d*d > n || (n `mod` d /=0 && isP (d+2))

fermatLiar :: Integer -> Integer
fermatLiar a = sfl (a+1)
  where
    sfl p
      | expmod a (p-1) p == 1 && (not.isPrime) p   = p
      | otherwise                                  = sfl (p+1)


--------------------------------------------------------------------------------------

{- Exercise 4: Prefix strings
  I introduce the helper function nop which has two arguments.
  The first argument is the number of bits that still need to be produced.
  The second argument is the surplus of zeros (#zeros-#ones) in the prefix thus far.
-}

numPref :: Int -> Int
numPref n = nop n 0
  where
    nop 0 _ = 1
    nop n 0 = nop (n-1) 1
    nop n m = nop (n-1) (m+1) + nop (n-1) (m-1)

--------------------------------------------------------------------------------------

{- Exercise 5: Last n digits
  The function expmod (from exercise 3) is very helpful in this exercise.
  Using this function, the implementation is straightforward. A bit of care is needed
  for leading zeros.
-}

intToList :: Integer -> [Integer]
intToList n = itl n []
  where itl n ds
         | n < 10    = n:ds
         | otherwise = itl (n `div` 10) ((n `mod` 10):ds)

trailingDigits :: Integer -> Int -> [Integer]
trailingDigits n m = intToList total
  where
    modulus = 10^m
    total = sum(map (\x -> expmod x x modulus) [1..n]) `mod` modulus

lastDigits  :: Integer -> Int -> [Integer]
lastDigits n m = replicate (m - length td) 0 ++ td
  where td = trailingDigits n m

--------------------------------------------------------------------------------------

{- Exercise 6: Polynomial Multiplication
 The solution is a direct implementtaion of long multiplication using vectors.
 Two helper functions are needed: addition of two vectors, and scaling a vector.
-}

polAdd :: [Integer] -> [Integer] -> [Integer]
polAdd [] bs = bs
polAdd as [] = as
polAdd (a:as) (b:bs) = (a+b):polAdd as bs

polScale :: Integer -> [Integer] -> [Integer]
polScale factor xs = map (*factor) xs

polMult :: [Integer] -> [Integer] -> [Integer]
polMult [0] _ = [0]
polMult _ [0] = [0]
polMult as bs = pm as bs
  where
    pm [] bs = []
    pm (a:as) bs = polAdd (polScale a bs) (0 : pm as bs)


--------------------------------------------------------------------------------------

{- Exercise 7: Minimal Steps
 This exercise is actually a graph search problem, where node n can go to the nodes n+1, 2*n, and 3*n.
 Hence, a breadth first search (BFS) is the canonical way to solve this problem using a FIFO
 queue. However, implementing the insert of an element x in a FIFO queue as fifo ++ [x] is
 quite inefficient (while x:fifo would implement a stack, not a FIFO).
 The solution to this problem is to have actually two queues: the current fringe (all elements have the same number
 of steps from the starting node) and the next layer. The order of the elements in that next layer is not
 important, so inserting an element can be performed at the front (i.e. constant time). Once the fringe is empty,
 this next layer serves as the fringe in the next iteration.
 Note that also a set of visited nodes is needed to avoid running in circles.
-}


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x < y     = x:y:ys
  | x == y    = y:ys
  | otherwise = y:(insert x ys)

member :: Ord a => a -> [a] -> Bool
member _ [] = False
member x (y:ys) = x==y || (x > y && member x ys)

minSteps :: Int -> Int -> Int
minSteps start goal = bfs 0 [start] [] []
  where
    bfs steps [] next visited = bfs (steps+1) next [] visited
    bfs steps (x:fringe) next visited
      | x == goal        = steps
      | member x visited = bfs steps fringe next visited
      | otherwise        = bfs steps fringe (enQueue x next) (insert x visited)
    enQueue x qs
      | 3*x <= goal = 3*x:2*x:x+1:qs
      | 2*x <= goal = 2*x:x+1:qs
      | otherwise  = x+1:qs
