import Data.Text.Internal.Fusion.Size (maxSize)
type Point = (Int, Int)
type Path = (Point, Point)
type Maze = [Path]
type Direction = Char

pathPoints :: Maze -> [Point]
pathPoints [] = []
pathPoints (x:xs) = [fst x] ++ [snd x] ++ pathPoints xs

maxPoint :: Maze -> Point
maxPoint maze = (maximum (map fst (pathPoints maze)), maximum (map snd (pathPoints maze)))

pointsInPath :: Path -> [Point]
pointsInPath (point1, point2)
    | snd point1 == snd point2 = map (\x -> (x, snd point1)) [min (fst point1) (fst point2) .. max (fst point1) (fst point2)]
    | otherwise = map (\y -> (fst point1, y)) [min (snd point1) (snd point2) .. max (snd point1) (snd point2)]

allMazePoints :: Maze -> [Point]
allMazePoints [] = []
allMazePoints (x:xs) = pointsInPath x ++ allMazePoints xs

west :: Path -> Point -> [Direction]
west (p1, p2) point
    | snd p1 == snd p2 && fst point > min (fst p1) (fst p2) = "W"
    | otherwise = ""

east :: Path -> Point -> [Direction]
east (p1, p2) point
    | snd p1 == snd p2 && fst point < max (fst p1) (fst p2) = "E"
    | otherwise = ""

north :: Path -> Point -> [Direction]
north (p1, p2) point
    | fst p1 == fst p2 && snd point > min (snd p1) (snd p2) = "N"
    | otherwise = ""

south :: Path -> Point -> [Direction]
south (p1, p2) point
    | fst p1 == fst p2 && snd point < max (snd p1) (snd p2) = "S"
    | otherwise = ""

connectionsInPath :: Path -> Point -> [Direction]
connectionsInPath path point =
    west path point ++ east path point
    ++ north path point ++ south path point

connectionsInMaze :: Maze -> Point -> [Direction]
connectionsInMaze [] _ = []
connectionsInMaze (x:xs) point = connectionsInPath x point ++ connectionsInMaze xs point

centreChar :: Maze -> Point -> Char
centreChar maze point
    | point `elem` (allMazePoints maze) = 'o'
    | otherwise = '.'

middleRow :: Maze -> Point -> String
middleRow maze point
    | 'W' `elem` connectionsInMaze maze point && 'E' `elem` connectionsInMaze maze point = "-" ++ [centreChar maze point] ++ "-"
    | 'W' `elem` connectionsInMaze maze point = "-" ++ [centreChar maze point] ++ "."
    | 'E' `elem` connectionsInMaze maze point = "." ++ [centreChar maze point] ++ "-"
    | otherwise = "." ++ [centreChar maze point] ++ "."

topRow :: Maze -> Point -> String
topRow maze point
    | 'N' `elem` connectionsInMaze maze point = ".|."
    | otherwise = "..."

bottomRow :: Maze -> Point -> String
bottomRow maze point
    | 'S' `elem` connectionsInMaze maze point = ".|."
    | otherwise = "..."

renderCell :: Maze -> Point -> [String]
renderCell maze point = [topRow maze point, middleRow maze point, bottomRow maze point]

renderGridRow :: Maze -> Int -> Int -> [String]
renderGridRow maze x yCoord =
    foldl1 (zipWith (++)) cells
    where
        rowPoints = [(xCoord, yCoord) | xCoord <- [0..x]]
        cells = map (renderCell maze) rowPoints

renderMaze :: [((Int, Int), (Int, Int))] -> [String]
renderMaze [] = []
renderMaze maze = concat [renderGridRow maze x yCoord | yCoord <- [0..y]]
    where
        (x, y) = maxPoint maze