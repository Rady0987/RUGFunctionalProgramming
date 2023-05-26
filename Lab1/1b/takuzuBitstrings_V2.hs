import Data.List

{--This program generates the list of all bitstrings with 
length n that satisfy the property that there are no more 
than two 0s or 1s are adjacent to each other. Moreover, 
the list is ordered in lexicographic increasing order.--}

addZerOne :: Char -> Int -> String -> String
addZerOne chr i (x:xs)
   | i == 0 = [chr] ++ xs
   | otherwise = [x] ++ addZerOne chr (i - 1) xs

helper :: Int -> Int -> String -> [String]
helper n i str 
   | i == n = [str] 
   | otherwise = helper n (i + 1) (addZerOne '0' i str) ++ helper n (i + 1) (addZerOne '1' i str) 

takuzuCheck :: String -> Bool
takuzuCheck str = not (isInfixOf "000" str) && not (isInfixOf "111" str)
  
createSample :: Int -> String
createSample n 
   | n == 0 = []
   | otherwise = createSample (n - 1) ++ ['0']

takuzuStrings :: Int -> [String]
takuzuStrings n = filter takuzuCheck (helper n 0 (createSample n))
