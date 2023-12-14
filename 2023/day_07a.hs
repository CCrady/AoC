import Advent
import Data.List
import Data.Maybe (fromJust)


ranks = "23456789TJQKA"
ordRank :: String -> [Int]
ordRank = map (fromJust . (`elemIndex` ranks))

data Type = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
            deriving (Eq, Ord)
ordType :: String -> Type
ordType str = case sort $ map length $ group $ sort str of
    [1, 1, 1, 1, 1] -> HighCard
    [1, 1, 1, 2] -> OnePair
    [1, 2, 2] -> TwoPair
    [1, 1, 3] -> ThreeKind
    [2, 3] -> FullHouse
    [1, 4] -> FourKind
    [5] -> FiveKind

ordHand str = (ordType str, ordRank str)


parseInput :: String -> [(String, Int)]
parseInput str = [(hand, read bid) | line <- lines str, let [hand, bid] = words line]

sortHands :: [(String, Int)] -> [(String, Int)]
sortHands = sortOn $ ordHand . fst

totalWinnings :: [(String, Int)] -> Int
totalWinnings = sum . zipWith (*) [1..] . map snd


solve = totalWinnings . sortHands . parseInput
main = puzzleInput "07" solve


