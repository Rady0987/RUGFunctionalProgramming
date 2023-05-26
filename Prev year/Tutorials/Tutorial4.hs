isFunny :: Integer -> Bool
isFunny n = or [(digSum - s)^s `mod` n == 0 | s <- combs]
   where
      digs = toDigits n
      digSum = sum(digs)
      combs = map sum (combinations digs)

choose :: Integer -> [Integer] -> [[Integer]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = [x:cs | cs <- choose (n - 1) xs] ++ choose n xs

combinations :: [Integer] -> [[Integer]]
combinations xs = choose 1 xs ++ choose 2 xs ++ choose 3 xs

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = x `mod` 10 : toDigits (x `div` 10)

---------------------------------------------

inverse :: [(a, b)] -> [(b, a)]
inverse ps = [(b, a) | (a, b) <- ps]

replicate1 :: Integer -> a -> [a]
replicate1 n x = [x | _ <- [1..n]] --list comprehension

doubleReverse :: [String] -> [String]
doubleReverse xs = reverse [reverse x | x <- xs]

powers :: Integer -> [Integer]
powers n = 1 : map (n * ) (powers n )

powers2 n = f n 0
   where f n m = n^m : f n (m + 1)

sega = 1 : 2 : zipWith (+) (map (*2) sega ) ((map (*3)) (tail sega))


pascalTriangle :: [[Integer]]
pascalTriangle :: iterate nextRow [1]
   where
      nextRow row = zipWith (+) ([0] ++ row ) (row ++ [0])

-----------------------------------------

