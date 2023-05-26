import Data.List

{--This program generates the list of all bitstrings with 
length n that satisfy the property that there are no more 
than two 0s or 1s are adjacent to each other. Moreover, 
the list is ordered in lexicographic increasing order.
(Slower than V2)--}

addZerOne :: Char -> [String] -> [String]
addZerOne c (xs:xxs) 
   | length xxs == 0 = [[c] ++ xs]
   | otherwise = [[c] ++ xs] ++ addZerOne c xxs 

helper :: Int -> Int -> [String] -> [String]
helper n i str
   | n == i = str
   | otherwise = helper n (i + 1) (filter takuzuCheck (addZerOne '0' str ++ addZerOne '1' str))

takuzuCheck :: String -> Bool
takuzuCheck str = not (isInfixOf "000" str) && not (isInfixOf "111" str)

takuzuStrings :: Int -> [String]
takuzuStrings n = helper n 1 ["0","1"]