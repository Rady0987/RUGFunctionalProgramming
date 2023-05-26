addPolynom :: [Integer] -> [Integer] -> [Integer]
addPolynom xs ys
    | length xs == length ys = sum
    | length xs > length ys = sum ++ (drop (length (sum)) xs)
    | length xs < length ys = sum ++ (drop (length (sum)) ys)
     where sum = zipWith (+) xs ys
    

f :: [Integer] -> [Integer] -> [Integer]
f xs ys
    | all (== 0) xs || all (== 0) ys = [0]
    | otherwise = addPolynom (map (*(head xs)) ys) (0:(f (tail xs) ys))


polMult :: [Integer] -> [Integer] -> [Integer] -- Main function
polMult xs ys
   | length (reverse (f (reverse xs) (reverse ys))) > 1 && head (reverse (f (reverse xs) (reverse ys))) == 0 = tail (reverse (f (reverse xs) (reverse ys)))
   | otherwise = reverse (f (reverse xs) (reverse ys))
