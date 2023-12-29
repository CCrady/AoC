import Advent (puzzleInput, puzzleInputTest, Direction (..), shift, invert)
import Data.List

parse :: String -> [(Direction, Int)]
parse str = [ readWords (words line) | line <- lines str ] where
    readWords [dirStr, lenStr, _] = (readDir dirStr, read lenStr)
    readDir "U" = North
    readDir "D" = South
    readDir "R" = East
    readDir "L" = West

-- Turn a clockwise loop counterclockwise, or vice versa.
-- This function is used solely for testing purposes.
flipLoop :: [(Direction, Int)] -> [(Direction, Int)]
flipLoop loop = let
    (x:xs) = map (\(dir, len) -> (invert dir, len)) $ reverse loop
    in xs ++ [x]

-- Find the horizontal segments of the given loop, and return them as (yPos, width) pairs. The width
-- is signed.
-- This function assumes that the first segment in the list is horizontal.
findHorizontals :: [(Direction, Int)] -> [(Int, Int)]
findHorizontals = findHorizontals' (0, 0) where
    findHorizontals' _ [] = []
    findHorizontals' loc ((dirH, lenH) : (dirV, lenV) : rest)
        | dirH `notElem` [East, West]   = error "Segment was vertical when it should've been horizontal"
        | dirV `notElem` [North, South] = error "Segment was horizontal when it should've been vertical"
        | otherwise = let
            nextLoc = shift dirV lenV $ shift dirH lenH loc
            (_, yPos) = loc
            width = case dirH of
                East -> lenH
                West -> - lenH
            in (yPos, width) : findHorizontals' nextLoc rest

findArea :: [(Direction, Int)] -> Int
findArea segments = let
    horizontals = findHorizontals segments
    rectangularArea = sum [ yPos * width | (yPos, width) <- horizontals ]
    eastSouthSegments = filter (\(dir, _) -> dir `elem` [East, South]) segments
    linearArea = sum [ len | (_, len) <- eastSouthSegments ]
    in abs rectangularArea + linearArea + 1


solve = findArea . parse
main = puzzleInput "18" solve
test1 = puzzleInputTest "18" $ putStrLn . show . solve
-- our solution works regardless of whether the input turns clockwise or counterclockwise
test2 = puzzleInputTest "18" $ putStrLn . show . findArea . flipLoop . parse

