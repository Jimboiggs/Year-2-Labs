luhnDouble :: Int -> Int
luhnDouble n
    | 2 * n > 9 = 2 * n - 9
    | otherwise = 2 * n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d
    | (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0 = True
    | otherwise = False
