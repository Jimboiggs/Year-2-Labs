type Row = [Int]
type Matrix = [Row]

scaleMatrix :: Int -> Matrix -> Matrix
scaleMatrix y [] = []
scaleMatrix y (x:xs) = map (\n -> y * n) x : scaleMatrix y xs

combineHorizontal :: Matrix -> Matrix -> Matrix
combineHorizontal [] [] = []
combineHorizontal (x:xs) (y:ys) = (x ++ y) : combineHorizontal xs ys

combineMany :: [Matrix] -> Matrix
combineMany [] = []
combineMany xs = foldl combineHorizontal (head xs) (tail xs)

kroneckerTwo :: Matrix -> Matrix -> Matrix
kroneckerTwo [] _ = []
kroneckerTwo (x:xs) m2 = combineMany (map (\n -> scaleMatrix n m2) x) ++ kroneckerTwo xs m2

kronecker :: [Matrix] -> Matrix
kronecker [] = []
kronecker xs = foldl1 kroneckerTwo xs