import Data.List (permutations, transpose)

type Row a = [a]
type Grid a = [Row a]

makeGrid :: Int -> [Int] -> Grid Int
makeGrid _ [] = []
makeGrid size numbers = (take size numbers) : (makeGrid size (drop size numbers))

rowsSumTo :: Int -> Grid Int -> Bool
rowsSumTo _ [] = True
rowsSumTo target (x:xs)
    | sum x == target = rowsSumTo target xs
    | otherwise = False

columnsSumTo :: Int -> Grid Int -> Bool
columnsSumTo target numbers = rowsSumTo target (transpose numbers)

isMagic :: Int -> Grid Int -> Bool
isMagic target numbers
    | (rowsSumTo target numbers) && (columnsSumTo target numbers) = True
    | otherwise = False

allMagics :: Int -> Int -> [Int] -> [Grid Int]
allMagics _ _ [] = []
allMagics target size numbers = filter (isMagic target) ( map(makeGrid size) (permutations numbers))