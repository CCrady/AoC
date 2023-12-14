import Advent
import Data.List

data Card = Card Int [Int] [Int]

parseCard :: String -> Card
parseCard str = let
    tokens = words str
    ((_ : cardStr : winningStrs), (_ : havingStrs)) = span (/= "|") tokens
    cardNum = read cardStr :: Int
    winningNums = map read winningStrs :: [Int]
    havingNums  = map read havingStrs  :: [Int]
    in Card cardNum winningNums havingNums

numWins :: Card -> Int
numWins (Card _ winningNums havingNums) = length $ filter (`elem` winningNums) havingNums

processWinnings :: [Int] -> [Int]
processWinnings = processWinnings' . map (\w -> (1, w)) where
    processWinnings' [] = []
    processWinnings' ((n, w) : cs) = n : processWinnings' (addCards n w cs)

    addCards :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
    addCards _        0       cs            = cs
    addCards numToAdd numToGo ((n, w) : cs) = (n + numToAdd, w) : addCards numToAdd (numToGo - 1) cs


solve = sum . processWinnings . map (numWins . parseCard) . lines

main = puzzleInput "04" solve

