import Advent (puzzleInput, puzzleInputTest)
import Data.Char (isAlpha)
import Data.List (cycle)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map


-- the boolean represents whether the node is a final node
data Node = Node Bool (Node, Node)

parseDirections :: String -> [(a, a) -> a]
parseDirections = cycle . map dir2func where
    dir2func 'L' = fst
    dir2func 'R' = snd

parseLine :: String -> (String, (String, String))
parseLine str = let
    (node, rest) = span isAlpha str
    (left, rest') = span isAlpha $ dropWhile (not . isAlpha) rest
    right = takeWhile isAlpha $ dropWhile (not . isAlpha) rest'
    in (node, (left, right))

parseNodes :: [String] -> Node
parseNodes strs = nodeMap ! "AAA" where
    nodeMap :: Map.Map String Node
    nodeMap = Map.fromList $ map (parseNodeAssoc . parseLine) strs
    parseNodeAssoc node@(name, _) = (name, parseNode node)
    parseNode (name, (left, right)) = Node (name == "ZZZ") (nodeMap ! left, nodeMap ! right)


countSteps :: Node -> [(Node, Node) -> Node] -> Int
countSteps = countSteps' 0 where
    countSteps' acc (Node True _) _ = acc
    countSteps' acc (Node False choices) (dir:dirs) = countSteps' (acc+1) (dir choices) dirs

solve str = let
    (directionsStr : "" : nodeStrs) = lines str
    directions = parseDirections directionsStr
    node = parseNodes nodeStrs
    in countSteps node directions

main = puzzleInput "08" solve
test = puzzleInputTest "08a" $ putStrLn . show . solve

