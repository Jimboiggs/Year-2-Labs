palindromeBy :: (a -> a -> Bool) -> [a] -> Bool 
palindromeBy f [] = True
palindromeBy f [x] = True
palindromeBy f (x : xs)
    | f x (last xs) = palindromeBy f (init xs)
    | otherwise = False