
revsubs :: [a] -> [[a]]
revsubs [] = []
revsubs (x:xs) = prefixes xs [x] ++ revsubs xs

prefixes :: [a] -> [a] -> [[a]]
prefixes [] acc = [acc]
prefixes (x:xs) acc = acc:prefixes xs (x:acc)

alabala :: [Integer] -> [[Integer]]
alabala xs = [[x,y] | x <- xs, y <- (tail xs), (even x && odd y) || (odd x && even y)]

isEqual :: Eq b => (a -> b) -> (a -> b) -> [a] -> Bool
isEqual f g xs = and[(f x) == (g x) | x <- xs]

concat2 :: [[a]] -> [a]
concat2 = foldr (++) []

mulinceven :: [Integer] -> Integer
mulinceven = (foldr (*) 1).(map (+1)).filter (>= 4)

oddeven :: [(a,a)] -> [a]
oddeven xs = [if (even idx) then (fst x) else (snd x) | (x, idx) <- zip xs [0..]]

removeRepetition :: Eq a => [a] -> [a]
removeRepetition [] = []
removeRepetition (c:cs) = c:[ b | (a,b) <- zip (c:cs) cs, a /= b]

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = sublists xs ++ [x:sublist | sublist <- sublists xs]

multiples :: [Integer] -> [Integer]
multiples xs = foldr merge [] [[0,x..] | x <- xs]
   where
      merge xs [] = xs
      merge (x:xs) (y:ys)
         | x < y = x:merge xs (y:ys)
         | x > y = y:merge (x:xs) ys
         | otherwise = x:merge xs ys

powerSum :: Integer -> Integer -> Integer
powerSum n e = foldr (+) 0 (map (^ e) [1..n])

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f arr = foldr (\x xs -> if f x then x:xs else xs) [] arr

concat3 :: [[a]] -> [a]
concat3 xxs = [x | xs <- xxs, x <- xs]

listComp :: Integer -> Integer -> [(Integer,Integer)]
listComp n m = concat [[(x,y) | y <- [0..n]] | x <- [0..m]]

dotProduct :: [Integer] -> [Integer] -> Integer
dotProduct xs ys = sum [x*y | (x,y) <- zip xs ys]

pairs :: [(Integer, Integer)]
pairs = f 0
   where 
      f n = [(n, n + 1)] ++ f (n + 1)

pairs2 :: [(Integer, Integer)]
pairs2 = [(x, y) | (x, y) <- zip [0..] [1..]]

gendups :: [Int] -> [Int]
gendups xs = concat [replicate x x | x <- xs ]

repeat1 :: a -> [a]
repeat1 a = f a
   where 
      f a = [a] ++ f a

threshHoldPairs :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
threshHoldPairs n xs = filter check xs
   where 
      check x = if (fst x + snd x) > n then True else False 

mapUsingFoldr :: (a -> b) -> [a] -> [b]
mapUsingFoldr g xs = foldr((:).g) [] xs

partition :: Ord a => a -> [a] -> ([a], [a])
partition n xs = ([x | x <- xs, x <=n ], [x | x <- xs, x> n])

tripletSum :: Integer -> [(Integer, Integer, Integer)]
tripletSum n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a + b + c == n, a <= b && b <= c]

adjacentTriples :: [a] -> [(a,a,a)]
adjacentTriples xs = [(a,b,c) | ((a,b),c) <- zip (zip xs (tail xs)) (tail (tail xs))]

iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = g x
 where g x = x : g (f x)

tribonacci :: [Integer]
tribonacci =  0 : 1 : 2 : zipWith (+) (zipWith (+) tribonacci (tail tribonacci)) (tail (tail tribonacci))

aligned :: Eq a => [a] -> [a] -> Int
aligned xs ys = length (filter (\x -> if (fst x == snd x) then True else False) (zip xs ys))