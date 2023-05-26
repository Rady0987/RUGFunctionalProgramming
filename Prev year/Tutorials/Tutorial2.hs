import Data.Char

sumFirstTwo :: [Int] -> Int
sumFirstTwo (x:y:_) = x + y
sumFirstTwo (x:_)   = x
sumFirstTwo []      = 0

and2 :: [Bool] -> Bool
and2 [] = True
and2 (b:bs) = b && (and2 bs)

or2 :: [Bool] -> Bool
or2 [] = False
or2 (b:bs) = b || (or2 bs)

and3 :: [Bool] -> Bool
and3 xs = foldr (&&) True xs

elemNum :: Integer -> [Integer] -> Integer
elemNum n [] = 0
elemNum a (x:xs)
   | a == x = 1 + elemNum a xs
   | otherwise = elemNum a xs


elemNum2 a xs = sum[1 | x <- xs, x == a]
-- Note that length [x | x <- xs, x==a] yields an Int
-- (length returns Int)

unique :: [Integer] -> [Integer]
unique xs = [x | x <-xs, elemNum x xs == 1]

iSort :: [Integer]->[Integer]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Integer->[Integer]->[Integer]
ins x [] = [x]
ins x (y:ys)
  | x == y = y:ys
  | x >= y = x:(y:ys)
  | otherwise = y : ins x ys

isSubStr :: String -> String -> Bool  
isSubStr [] _ = True
isSubStr _ [] = False --pattern is not processed to the end
isSubStr (x:xs) (y:ys)
   | x == y       = isSubStr xs ys
   | otherwise    = isSubStr (x:xs) ys

isSubSeq :: String -> String -> Bool
isSubSeq [] _ = True
isSubSeq _ [] = False
isSubSeq(x:xs) (y:ys)
   | x == y    = match || isSubSeq (x:xs) (ys)
   | otherwise = isSubSeq (x:xs) (ys)
   where match = unzip (zip (x:xs) (y:ys)) == ((x:xs), (x:xs))

isPalin :: String -> Bool
isPalin str = x == reverse x
   where x = [toLower c | c <- str, isAlpha c]

subst :: String -> String -> String -> String
subst _ _ [] = []
subst (x:xs) sub (y:ys)
   | match  = sub ++ (drop (length xs) ys)
   | otherwise = y : (subst (x:xs) sub ys)
  where match = unzip (zip (x:xs) (y:ys)) == ((x:xs), (x:xs))