import Data.Char

{--This program takes a string and a key as input in order to encode or 
decode the string according to the Improved Caesar cipher, after each 
encoded/decoded letter, the alphabet is rotated again by key positions.--}

letterCipher :: Char -> Int -> Char -> Char
letterCipher c key op
   | op == 'e' = chr (a + ((ord c) - a - key) `mod` 26)
   | otherwise = chr (a + ((ord c) - a + key) `mod` 26)
   where a = (ord 'A')

helper :: Int -> String -> Int -> Char -> String
helper key (x:xs) i op
   | length xs == 0 && isAlpha x = [curLetter]
   | isAlpha x = [curLetter] ++ helper key xs (i + 1) op
   | otherwise = [' '] ++ helper key xs i op
   where curLetter = letterCipher x (key * i) op

cipherEncode :: Int -> String -> String
cipherEncode key str = helper key str 1 'e'

cipherDecode :: Int -> String -> String
cipherDecode key str = helper key str 1 'd'
