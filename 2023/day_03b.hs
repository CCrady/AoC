import Advent
import Data.Char (isDigit)
import Data.List
import Data.Maybe (catMaybes)

type Position = (Int, Int)
type CharGrid = [[(Position, Char)]]
data Number = Number Int (Position, Int)

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

charPositions :: String -> CharGrid
charPositions str = let
    numberedLines = zip [0..] $ lines str
    in [[((x, y), c) | (x, c) <- zip [0..] l]
                     | (y, l) <- numberedLines]

-- This ought to be refactored to hell but it's good enough for AoC
nums :: CharGrid -> [Number]
nums = concat . map numsLine where
    numsLine charLine = let
        sansLeading = dropWhile (not . isDigit . snd) charLine
        in if null sansLeading then []
            else let
                (numChars, rest) = span (isDigit . snd) sansLeading
                numLen = length numChars
                numPosition = fst $ numChars !! 0
                numValue = read $ map snd numChars
                in Number numValue (numPosition, numLen) : numsLine rest

isAdjacent :: Number -> Position -> Bool
isAdjacent (Number _ ((numX, numY), numLen)) (symX, symY) =
    abs (symY - numY) <= 1 && symX >= numX - 1 && symX <= numX + numLen

stars :: CharGrid -> [Position]
stars = map fst . filter (\(_, c) -> c == '*') . concat

findGearVals :: CharGrid -> [Int]
findGearVals grid = let
    nums' = nums grid
    stars' = stars grid
    star2Val star = case filter (\num -> isAdjacent num star) nums' of
        [Number val1 _, Number val2 _] -> Just $ val1 * val2
        _ -> Nothing
    in catMaybes $ map star2Val stars'

main = puzzleInput "03" $ sum . findGearVals . charPositions

