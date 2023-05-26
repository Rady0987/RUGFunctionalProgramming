{--This function computes the
 composition of all the functions from a list--}
compose :: [(a -> a)] -> (a -> a)
compose = foldr (.) id