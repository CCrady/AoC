import Advent (puzzleInput, puzzleInputTest, Index, Matrix, parseMatrix)
import Data.Array
import Data.List

numSpins = 1000000000

reverseArray :: Array Int a -> Array Int a
reverseArray arr = let
    bnds = bounds arr
    pastEnd = snd bnds + 1
    in ixmap bnds (\i -> pastEnd - i) arr

data Tile = Empty | Round | Cube deriving Eq
type Path = Array Int Index

parse :: String -> Matrix Tile
parse = parseMatrix char2tile where
    char2tile '.' = Empty
    char2tile 'O' = Round
    char2tile '#' = Cube

cols :: Matrix a -> [[(Index, a)]]
cols m = let
    (_, (width, height)) = bounds m
    in [[ ((x,y), m ! (x,y)) | y <- [1..height] ]
                             | x <- [1..width] ]

rows :: Matrix a -> [[(Index, a)]]
rows m = let
    (_, (width, height)) = bounds m
    in [[ ((x,y), m ! (x,y)) | x <- [1..width] ]
                             | y <- [1..height] ]

-- Given the output of cols or rows, find all the contiguous regions in which rounded rocks can
-- roll.
paths :: [[(Index, Tile)]] -> [Path]
paths = map subsection2path . filter (not . null) . concat . map subsections where
    subsections :: [(Index, Tile)] -> [[Index]]
    subsections [] = [[]]
    subsections ((_, Cube) : xs) = [] : subsections xs
    subsections ((loc, _) : xs) = let
        (sub : subs) = subsections xs
        in (loc : sub) : subs
    subsection2path :: [Index] -> Path
    subsection2path sub = listArray (1, length sub) sub

-- Given an input matrix and a path, output a list of updated associations for the path.
tiltPath :: Matrix Bool -> Path -> [(Index, Bool)]
tiltPath m path = let
    (_, size) = bounds path
    numRocks = length $ filter id $ map (m !) $ elems path
    newBools = replicate numRocks True ++ replicate (size - numRocks) False
    in if numRocks == 0
        then []
        else zip (elems path) newBools

tilt :: [Path] -> Matrix Bool -> Matrix Bool
tilt paths' m = m // (concat $ map (tiltPath m) paths')

-- Find a given rock configuration's load on the North support beam.
northLoad :: Matrix Bool -> Int
northLoad m = let
    (_, (_, height)) = bounds m
    assoc2load (_, False) = 0
    assoc2load ((_, y), True) = height - y + 1
    in sum $ map assoc2load $ assocs m

-- Given a function and an initial value, find a cycle in repeated applications. Returns the index
-- of the start of the cycle, the cycle itself, and the cycle length as a triple.
findCycle :: Eq a => (a -> a) -> a -> (Int, [a], Int)
findCycle f = findCycle' 0 [] where
    findCycle' acc xs x = case elemIndex x xs of
        Nothing -> findCycle' (acc+1) (x:xs) (f x)
        Just i  -> let
            cycleLength = i + 1
            cycle = reverse $ take cycleLength xs
            cycleStart = acc - cycleLength
            in (cycleStart, cycle, cycleLength)


-- for debugging purposes
showRounds :: Matrix Bool -> String
showRounds = unlines . map (map (\(_, x) -> if x then 'O' else '.')) . rows

solve str = let
    tileMatrix = parse str
    rollMatrix = fmap (== Round) tileMatrix
    northPaths = paths $ cols tileMatrix
    southPaths = map reverseArray northPaths
    westPaths = paths $ rows tileMatrix
    eastPaths = map reverseArray westPaths
    spin = tilt eastPaths
         . tilt southPaths
         . tilt westPaths
         . tilt northPaths
    (cycleStart, cycle, cycleLength) = findCycle spin rollMatrix
    in northLoad $ cycle !! ((numSpins - cycleStart) `mod` cycleLength)

main = puzzleInput "14" solve
test = puzzleInputTest "14" $ putStrLn . show . solve

