{--This function adds each elements according to their index--}
addPolynom :: [Integer] -> [Integer] -> [Integer]
addPolynom xs ys
   | length xs == length ys = sum
   | length xs > length ys = sum ++ (drop (length (sum)) xs)
   | length xs < length ys = sum ++ (drop (length (sum)) ys)
    where sum = zipWith (+) xs ys
    
{--The helper function that iterates over the elements of the lists--}
multHelper :: [Integer] -> [Integer] -> [Integer]
multHelper xs ys
   | all (== 0) xs || all (== 0) ys = [0]
   | otherwise = addPolynom (map (*(head xs)) ys) (0:(multHelper (tail xs) ys))


{--The main multiplication function--}
polMult :: [Integer] -> [Integer] -> [Integer]
polMult xs ys
   | length(rev) > 1 && head(rev) == 0 = tail(rev)
   | otherwise = rev
    where rev = reverse (multHelper (reverse xs) (reverse ys))