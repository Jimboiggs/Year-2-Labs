getPoint :: Char -> Int
getPoint x
    | x == 'E' = 16
    | x == 'D' = 24
    | x == 'C' = 32
    | x == 'B' = 40
    | x == 'A' = 48
    | x == '*' = 8
    | otherwise = error "Invalid A level grade"

getPoints :: String -> Int
getPoints [] = 0
getPoints (x:xs) = getPoint x + getPoints xs

meetsOffer :: String -> Int -> Bool
meetsOffer s n
    | getPoints s >= n = True
    | otherwise = False