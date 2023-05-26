mult :: [Integer] -> Integer
mult xs = foldr (*) 1 (map (+1) (filter (>=4) xs))

concat1 :: [[a]] -> [a]
concat1 xs = foldr (++) [] xs

primes = sieve [2..] 
   where sieve (p:xs) = p:sieve [x|x <- xs, x `mod` p > 0]

isPrime :: Int -> Bool
isPrime n 
   | n `elem` (take n primes) = True
   | otherwise = False

ones = 1:ones

evens = 0:zipWith (+) ones odds

odds = zipWith (+) ones evens


folgth xs = foldr (\_ -> (+1)) 0 xs

{--
aligned :: [char] -> [char] -> Int
aligned xs ys --} 

evenLists :: [Int] -> [Int]
evenLists xs = [a | a <- xs, a `mod` 2 == 0]

triples :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
triples xs ys zs = [(x,y,z) | (x,(y,z)) <- zip xs (zip ys zs)]

fibs :: [Int]
fibs = 0:1:[x + y | (x,y) <- zip fibs (tail fibs)]

natlists = [0]:map ((0:).(map (+1))) natlists