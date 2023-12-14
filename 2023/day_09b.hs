import Advent
import Data.List

parse :: String -> [[Int]]
parse str = [[read word | word <- words line] | line <- lines str]

differentiate :: [Int] -> [Int]
differentiate (x:y:rest) = y - x : differentiate (y:rest)
differentiate [_] = []

differentiateToZero :: [Int] -> [[Int]]
differentiateToZero = takeWhile (not . all (== 0))
                    . iterate differentiate

extrapolateBack :: [Int] -> Int
extrapolateBack = foldr (-) 0 . map head . differentiateToZero


solve = sum . map extrapolateBack . parse
main = puzzleInput "09" solve

