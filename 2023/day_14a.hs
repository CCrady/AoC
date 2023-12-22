import Advent (puzzleInput, puzzleInputTest, Index, Matrix, parseMatrix, cols)
import Data.Array

data Tile = Empty | Round | Cube deriving Eq

char2tile :: Char -> Tile
char2tile '.' = Empty
char2tile 'O' = Round
char2tile '#' = Cube

parse :: String -> Matrix Tile
parse = parseMatrix char2tile

colLoad :: Int -> [Tile] -> Int
colLoad height col = let
    tiles = zip [height, height-1 ..] col
    subsections [] = [[]]
    subsections ((_, Cube) : ts) = [] : subsections ts
    subsections (t : ts) = let
        (sub : subs) = subsections ts
        in (t : sub) : subs
    subsectionLoad ts@((row, _) : _) = descendingSum row
                                     $ length
                                     $ filter (\(_, t) -> t == Round) ts
    in sum $ map subsectionLoad $ filter (not . null) $ subsections tiles

descendingSum :: Int -> Int -> Int
descendingSum start num = let
    end = start - num
    tri n = n * (n + 1) `div` 2
    in tri start - tri end

solve str = let
    matrix = parse str
    (_, (_, height)) = bounds matrix
    in sum $ map (colLoad height) $ cols matrix
main = puzzleInput "14" solve
test = puzzleInputTest "14" $ putStrLn . show . solve

