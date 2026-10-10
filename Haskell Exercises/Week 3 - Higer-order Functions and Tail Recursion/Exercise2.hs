hamming :: Eq a => [a] -> [a] -> Int
hamming [] [] = 0
hamming [] (x : xs) = error "Lists have different lengths"
hamming (x : xs) [] = error "Lists have different lengths"
hamming (x:xs) (y:ys)
    | x == y = hamming xs ys
    | otherwise = 1 + hamming xs ys

hammingDistances :: Eq a => [a] -> [[a]] -> [Int]
hammingDistances x [] = []
hammingDistances x (y : ys) = hamming x y : hammingDistances x ys

allHammingDistances :: Eq a => [[a]] -> [Int]
allHammingDistances [] = []
allHammingDistances [x] = []
allHammingDistances (x :xs) = hammingDistances x xs ++ allHammingDistances xs

minHamming :: Eq a => [[a]] -> Int
minHamming [] = 0
minHamming [x] = error "Only one list"
minHamming x = minimum (allHammingDistances x)