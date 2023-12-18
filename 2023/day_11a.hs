import Advent (puzzleInput, puzzleInputTest)
import Data.List

type Point = (Int, Int)
type Galaxies = (Point, [Point])

parse :: String -> Galaxies
parse str = let
    charGrid = lines str
    width = length $ head charGrid
    height = length charGrid
    galaxyPoints = [(x, y) | (y, row)  <- zip [1..] charGrid,
                             (x, char) <- zip [1..] row,
                             char == '#']
    in ((width, height), galaxyPoints)

emptyRows :: Galaxies -> [Int]
emptyRows ((_, height), points) = [1..height] \\ occupiedRows where
    occupiedRows = map yCoord $ nubBy sameRow points
    yCoord (_, y) = y
    sameRow (_, y1) (_, y2) = y1 == y2

emptyCols :: Galaxies -> [Int]
emptyCols ((width, _), points) = [1..width] \\ occupiedCols where
    occupiedCols = map xCoord $ nubBy sameCol points
    xCoord (x, _) = x
    sameCol (x1, _) (x2, _) = x1 == x2

-- Map an original position to a new one post-expansion
positionMap :: Galaxies -> Point -> Point
positionMap galaxies (x, y) = (coordMap emptyCols x, coordMap emptyRows y) where
    coordMap empty coord = let
        empties = empty galaxies
        expansion = length $ takeWhile (< coord) empties
        in coord + expansion

expand :: Galaxies -> [Point]
expand galaxies@(_, points) = map (positionMap galaxies) points

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

pairs :: [a] -> [(a, a)]
pairs (x:xs) = map ((,) x) xs ++ pairs xs
pairs [] = []


solve = sum . map (uncurry dist) . pairs . expand . parse

main = puzzleInput "11" solve
test = puzzleInputTest "11" $ putStrLn . show . solve

