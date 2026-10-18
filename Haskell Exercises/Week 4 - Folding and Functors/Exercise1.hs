map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

all :: (a -> Bool) -> [a] -> Bool
all f xs = foldr (&&) True (map f xs)

any :: (a -> Bool) -> [a] -> Bool
any f xs = foldr (||) False (map f xs)

combine :: Eq a => a -> [[a]] -> [[a]]
combine x [] = [[x]]
combine x (g:gs)
    | x == head g = (x:g):gs
    | otherwise   = [x]:g:gs

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' xs = foldr combine [] xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
    | f x = x : takeWhile f xs
    | otherwise = []