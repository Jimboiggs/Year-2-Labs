palindromeBy :: (a -> a -> Bool) -> [a] -> Bool 
palindromeBy f [] = True
palindromeBy f [x] = True
