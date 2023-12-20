import Advent (puzzleInput, puzzleInputTest)
import Data.Array

type Index = (Int, Int)
type Grid = Array Index Bool

parseGrid :: String -> Grid
parseGrid str = let
    charGrid = lines str
    width = length $ head charGrid
    height = length charGrid
    assocs = [((x,y), char2bool char) | (y, line) <- zip [1..] charGrid,
                                        (x, char) <- zip [1..] line]
    char2bool '#' = True
    char2bool '.' = False
    in array ((1, 1), (width, height)) assocs

parseGrids :: String -> [Grid]
parseGrids str = let
    gridStrs = split str where
        split [] = [[]]
        split ('\n':'\n':rest) = [] : split rest
        split (c:cs) = let
            (head : tail) = split cs
            in (c : head) : tail
    in map parseGrid gridStrs


indicesEqual :: Grid -> [Index] -> [Index] -> Bool
indicesEqual grid is js = and $ zipWith (\i j -> grid ! i == grid ! j) is js

isRowMirrored :: Grid -> Int -> Bool
isRowMirrored grid rowNum = let
    (_, (width, height)) = bounds grid
    toRowIndices y = map (\x -> (x, y)) [1..width]
    aboveIndices = concat $ map toRowIndices [rowNum, pred rowNum .. 1]
    belowIndices = concat $ map toRowIndices [succ rowNum .. height]
    in indicesEqual grid aboveIndices belowIndices

isColMirrored :: Grid -> Int -> Bool
isColMirrored grid colNum = let
    (_, (width, height)) = bounds grid
    toColIndices x = map (\y -> (x, y)) [1..height]
    leftIndices  = concat $ map toColIndices [colNum, pred colNum .. 1]
    rightIndices = concat $ map toColIndices [succ colNum .. width]
    in indicesEqual grid leftIndices rightIndices

firstIndex :: Int -> [Bool] -> Maybe Int
firstIndex _ []         = Nothing
firstIndex i (True:_)   = Just i
firstIndex i (False:bs) = firstIndex (i+1) bs

-- The problem statement implies that each grid will only have a single line of symmetry
summarize :: Grid -> Int
summarize grid = let
    (_, (width, height)) = bounds grid
    areRowsMirrored = map (isRowMirrored grid) [1 .. height-1]
    areColsMirrored = map (isColMirrored grid) [1 .. width-1]
    mirroredRow = firstIndex 1 areRowsMirrored
    mirroredCol = firstIndex 1 areColsMirrored
    in case mirroredCol of
        Just n  -> n
        Nothing -> case mirroredRow of
            Just n  -> 100 * n
            Nothing -> error "no line of symmetry found!"


solve = sum . map summarize . parseGrids
main = puzzleInput "13" solve
test = puzzleInputTest "13" $ putStrLn . show . solve

