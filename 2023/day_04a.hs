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

points :: Card -> Int
points (Card _ winningNums havingNums) = let
    numMatches = length $ filter (`elem` winningNums) havingNums
    in if numMatches == 0
        then 0
        else round $ 2 ** (fromIntegral numMatches - 1)

main = puzzleInput "04" $ sum . map (points . parseCard) . lines

