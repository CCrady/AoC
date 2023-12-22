import Advent (puzzleInput, puzzleInputTest, split)
import Data.Char (ord)

hash :: String -> Int
hash = hash' 0 where
    hash' acc [] = acc
    hash' acc (c:cs) = let
        acc' = ((acc + ord c) * 17) `mod` 256
        in hash' acc' cs


solve = sum . map hash . split "," . concat . lines
main = puzzleInput "15" solve
test = puzzleInputTest "15" $ putStrLn . show . solve

