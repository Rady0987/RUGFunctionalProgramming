import Data.Char
import Data.List
total :: (Integer -> Integer) -> (Integer -> Integer)
total f = (\n -> foldr (+) 0 (map f [0..n]))
-- map[1..3]
-- [f 1, f 2, f 3, f 4]

flipArgs :: (a -> b -> c) -> (b -> a -> c)
flipArgs f = (\x y -> f y x)

-- uncurry :: (x       -> y -> z) -> (x, y) -> z
-- ($)     :: (a -> b) -> a -> b

-- (a -> b, a) -> b
-- uncurry ($) ((+1, 4)) output 5

-- uncurry :: (x      -> y   -> z) -> (x, y) -> z
-- (:)     :: a       -> [a] -> [a]
 -- x <==> a
 -- y <==> [a]
 -- z <==> [a]

-- uncurry :: (x       -> y        -> z      ) -> (x, y) -> z
-- (.)     :: (b -> c) -> (a -> b) -> a -> c
 -- x <==> (b -> c)
 -- y <==> (a -> b)
 -- z <==> a -> c
 -- example : uncurry (.)) ((*2). (+1)) 1 <==> (*2) ((+1) 1) = (*2) 2 = 4

 -- uncurry :: (x            -> y      -> z) -> (x, y) -> z
 -- uncurry :: (a -> b -> c) -> (a, b) -> c
  -- x <==> (a -> b -> c) 
  -- y <==> (a, b) 
  -- z <==> c
  -- answer: (a -> b -> c, (a,b)) -> c

curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry3 f a b c = f(a, b, c)

uncurry :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry f (a, b, c) = f a b c



iter2 :: Int -> (a -> a) -> (a -> a)
iter2 n f = foldr (.) id (replicate n f)




fsplit :: [a] -> [[a]]
fsplit [] = []
fsplit (x:xs) = [x] : map (x:) (fsplit xs)

feat :: [a] -> [[a]]
feat [] = []
feat (x:xs) = xs : (feat xs)

woop :: [a] -> [([a], [a])]
woop str = ([], str) : zip (fsplit str) (feat str)

subLists :: [a] -> [[a]]
subLists [] = [[]]
subLists (x:xs) = [x:sub | sub <- subLists xs] ++ subLists xs

subSequences :: [a] -> [[a]]
subSequences xs = []:[take j (drop i xs) | i <- [0..len-1], j <- [1..len-i]]
   where len = length xs

factorials :: [Integer]
factorials = 1 : zipWith (+) factorials [1..]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factors :: Integer -> [Integer]
factors n = [d | d <- [1..n], n `mod` d == 0]

runningSum :: [Integer] -> [Integer]
runningSum xs = sumlist xs 0
   where sumlist [] a = []
         sumlist (x:xs) a = (a+x) : sumlist xs (a+x)

