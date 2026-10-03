singleton' :: a -> [a]
singleton'(a) = [a]

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate'(n-1) x

repeat' :: a -> [a]
repeat' x = x : repeat' x

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [x] = x
last' [_ : xs] -> last' xs