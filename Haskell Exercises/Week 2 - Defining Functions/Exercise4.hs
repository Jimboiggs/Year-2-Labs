halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
    where
        half = length xs `div` 2