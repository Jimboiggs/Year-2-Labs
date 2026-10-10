type Row a = [a]
type Grid a = [Row a]

vJoinGrids :: [Grid a] -> Grid a
vJoinGrids [] = []
vJoinGrids (x:xs) = x ++ vJoinGrids xs

hJoin :: [a] -> [a] -> [a]
hJoin [] [] = []
hJoin x y = x ++ y

twoGrids :: Grid a -> Grid a -> Grid a
twoGrids [] [] = []
twoGrids (x:xs) (y:ys) = hJoin x y : twoGrids xs ys

hJoinGrids :: [Grid a] -> Grid a
hJoinGrids [] = []
hJoinGrids [x] = [x]
hJoinGrids (x: y : xys) = twoGrids x y ++ hJoinGrids (y:xys)