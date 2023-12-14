import Advent
import Data.List
import Data.Maybe (fromJust)


ranks = "J23456789TQKA"
ordRank :: String -> [Int]
ordRank = map (fromJust . (`elemIndex` ranks))

data Type = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
            deriving (Eq, Ord)
ordType :: String -> Type
ordType = ordType' where
    ordType' str = case groupHand str of
        [1, 1, 1, 1, 1] -> HighCard
        [2, 1, 1, 1] -> OnePair
        [2, 2, 1] -> TwoPair
        [3, 1, 1] -> ThreeKind
        [3, 2] -> FullHouse
        [4, 1] -> FourKind
        [5] -> FiveKind
    --
    groupHand str = if str == "JJJJJ" then [5] else let
        (jokers, rest) = partition (== 'J') str
        numJokers = length jokers
        groupedRest = sortBy (\x y -> compare y x)
                    $ map length
                    $ group $ sort rest
        (headGrouped : tailGrouped) = groupedRest
        in (numJokers + headGrouped) : tailGrouped

ordHand str = (ordType str, ordRank str)


parseInput :: String -> [(String, Int)]
parseInput str = [(hand, read bid) | line <- lines str, let [hand, bid] = words line]

sortHands :: [(String, Int)] -> [(String, Int)]
sortHands = sortOn $ ordHand . fst

totalWinnings :: [(String, Int)] -> Int
totalWinnings = sum . zipWith (*) [1..] . map snd


solve = totalWinnings . sortHands . parseInput
main = puzzleInput "07" solve


