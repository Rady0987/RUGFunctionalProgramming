import Data.Char

-- 1.
nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

nAnd2 :: Bool -> Bool -> Bool
nAnd2 False False = True
nAnd2 False True = True
nAnd2 True False = True
nAnd2 True True = False

nAnd3 :: Bool -> Bool -> Bool
nAnd3 True True = False
nAnd3 _    _ = True -- any input -> True

-- 2.
min2 :: Int -> Int -> Int 
min2 x y
  | x < y =x
  | otherwise = y

minThree :: Int -> Int -> Int -> Int
minThree z y x = min2 x (min2 y z)

-- 3.
charToNum :: Char -> Int
charToNum x
  | x < '0' = 0
  | x > '9' = 0
  | otherwise = ord x - ord '0'

-- 4.
numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
  | discr == 0 = 1
  | discr > 0 = 2
  | otherwise = 0
    where discr = b*b-4*c*a

-- 5.
numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c
  | a /= 0 = numberNDroots a b c
  | b /= 0 = 1
  | c == 0 = 3
  | otherwise = 0

-- 6.
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | nr == 0   = 0
  | nr == 3   = 0
  | otherwise = (-b - sqrt(b*b - 4*c*a))/(2*a)
    where nr  = numberRoots a b c

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | nr == 0   = 0
  | nr == 3   = 0
  | otherwise = (-b + sqrt(b * b - 4 * c * a)) / (2 * a)
    where nr  = numberRoots a b c

-- 7.
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n 
  | n < m = 0
  | n == m = n
  | otherwise = m * rangeProduct (m + 1) n

rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 m n 
  | n < m = 0
  | otherwise = product [m .. n]

-- 8.
fac :: Integer -> Integer
fac 0 = 1
fac x = rangeProduct 1 x

-- 9.
pow2 :: Integer -> Integer
pow2 m
  | m == 0         = 1
  | m `mod` 2 == 0 = sqr (pow2 (m `div` 2))
  | otherwise      = 2 * pow2 (m - 1)
    where sqr n = n * n

-- 10.
maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b
  | a > b     = (a, 1)
  | a == b    = (a, 2)
  | otherwise = (b, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs a b c
  | c < m     = (m, cnt)
  | c == m    = (m, cnt + 1)
  | otherwise = (c, 1)
   where (m, cnt) = maxOccurs a b 

-- 11.
doubleAll :: [Integer] -> [Integer]
doubleAll ls = [2 * x | x <- ls]

-- 12.
matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x | x <- xs, n == x]

isElementOf :: Integer -> [Integer] -> Bool
isElementOf n xs = matches n xs /= []