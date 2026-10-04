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

(!!') :: [a] -> Int -> a
(!!') [x:xs] 0 = x
(!!') [x:xs] n = (!!') [xs] n-1

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' y (x:xs) = x : y : intersperse' y xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [x:xs] = x ++ concat'[xs]

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
    | y == x    = True
    | otherwise = elem' y xs
-- eq is defined for all types so no not type necessary

countInList' :: Eq a => a -> [a] -> Int
countInList' y [] = 0
countInList' y (x : xs)
    | y == x = 1 + countInList' y xs
    | y != x = 0 + countInList' y xs

deleteAll' :: Eq a => a -> [a] -> [a]
deleteAll' y [] = []
deleteAll' y (x:xs)
    | y == x = deleteAll' y xs
    | y != x = x : deleteAll' y xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (deleteAll' x xs)