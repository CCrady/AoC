import Advent
import Data.Char (isAlpha)
import Data.List (cycle)
import qualified Data.Map.Strict as Map


parseDirections :: String -> [(a, a) -> a]
parseDirections = cycle . map dir2func where
    dir2func 'L' = fst
    dir2func 'R' = snd

parseNodes :: [String] -> Map.Map String (String, String)
parseNodes = Map.fromList . map parseLine where
    parseLine str = let
        (node, rest) = span isAlpha str
        (left, rest') = span isAlpha $ dropWhile (not . isAlpha) rest
        right = takeWhile isAlpha $ dropWhile (not . isAlpha) rest'
        in (node, (left, right))


countSteps :: Map.Map String (String, String) -> [(String, String) -> String] -> Int
countSteps nodes = countSteps' "AAA" 0 where
    countSteps' "ZZZ"    i _          = i
    countSteps' currNode i (dir:dirs) = let
        nextNode = dir $ nodes Map.! currNode
        in countSteps' nextNode (i+1) dirs

solve str = let
    (directionsStr : _ : nodeStrs) = lines str
    directions = parseDirections directionsStr
    nodes = parseNodes nodeStrs
    in countSteps nodes directions

main = puzzleInput "08" solve

