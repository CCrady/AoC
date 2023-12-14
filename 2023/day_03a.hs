import Advent
import Data.Char (isDigit)
import Data.List

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

symbolLocations :: CharGrid -> [Position]
symbolLocations = map fst . filter (isSymbol . snd) . concat

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

-- This is super inefficient but w/e
findRelevantNums :: String -> [Number]
findRelevantNums str = let
    charGrid = charPositions str
    symbolLocations' = symbolLocations charGrid
    nums' = nums charGrid
    in filter (\num -> any (isAdjacent num) symbolLocations') nums'

sumNums :: [Number] -> Int
sumNums = sum . map (\(Number value _) -> value)

main = puzzleInput "03" $ sumNums . findRelevantNums

