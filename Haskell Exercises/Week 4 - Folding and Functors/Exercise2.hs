dec2Int :: [Int] -> Int
dec2Int xs = foldl (\acc x -> (10 * acc) + x) 0 xs