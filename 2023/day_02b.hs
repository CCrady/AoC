import Advent
import Data.List
import Data.Maybe

-- The number of red, green, and blue cubes in a handful (respectively)
data Handful = Handful Int Int Int
-- The number of the game, and all the handfulls revealed during it
data Game = Game Int [Handful]


parseHandful :: String -> Handful
parseHandful str = let
    colors = map (split " ") $ split ", " str
    assocs = map (\[num, color] -> (color, read num :: Int)) colors
    colorAmount = fromMaybe 0 . flip lookup assocs
    in Handful (colorAmount "red") (colorAmount "green") (colorAmount "blue")

parseGame :: String -> Game
parseGame str = let
    sansGamePrefix = fromJust $ dropPrefix "Game" str
    (num, handfulsStr) = fromJust $ splitFirst ": " sansGamePrefix
    handfuls = map parseHandful $ split "; " handfulsStr
    in Game (read num) handfuls

minInBag :: Game -> Handful
minInBag (Game _ handfuls) = let
    red   = maximum $ map (\(Handful r _ _) -> r) handfuls
    green = maximum $ map (\(Handful _ g _) -> g) handfuls
    blue  = maximum $ map (\(Handful _ _ b) -> b) handfuls
    in Handful red green blue

gamePower :: Game -> Int
gamePower game = let
    Handful r g b = minInBag game
    in r * g * b

sumOfGamePowers :: [Game] -> Int
sumOfGamePowers = sum . map gamePower

main = puzzleInput "02" $ sumOfGamePowers . map parseGame . lines


