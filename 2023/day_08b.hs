import Advent (puzzleInput, puzzleInputTest)
import Data.Char (isAlphaNum)
import Data.List (cycle, elemIndex)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map


data Node = Node Label (Node, Node)
type Label = (Char, Char, Char)
data Dir = DirLeft | DirRight deriving Eq
type Direction = (Int, Dir)

parseDirections :: String -> [Direction]
parseDirections = cycle . zip [1..] . map parseDir where
    parseDir 'L' = DirLeft
    parseDir 'R' = DirRight

applyDirection :: Direction -> Node -> Node
applyDirection (_, DirLeft)  (Node _ (left, _))  = left
applyDirection (_, DirRight) (Node _ (_, right)) = right

isEnding :: Node -> Bool
isEnding (Node (_, _, c) _) = c == 'Z'

parseLine :: String -> (String, (String, String))
parseLine str = let
    getName = span isAlphaNum . dropWhile (not . isAlphaNum)
    (name,  rest)  = getName str
    (left,  rest') = getName rest
    (right, _)     = getName rest'
    in (name, (left, right))

parseNodes :: [String] -> [Node]
parseNodes strs = Map.elems $ takeStarting nodeMap where
    nodeMap :: Map.Map String Node
    nodeMap = Map.fromList $ map (parseNodeAssoc . parseLine) strs
    parseNodeAssoc node@(name, _) = (name, parseNode node)
    parseNode (name, (left, right)) = Node (name2label name)
                                           (nodeMap ! left, nodeMap ! right)
    name2label [a, b, c] = (a, b, c)
    takeStarting = Map.filterWithKey (\[_, _, c] _ -> c == 'A')

countSteps :: [Direction] -> Node -> Int
countSteps = countSteps' 0 where
    countSteps' acc (d:ds) node
        | isEnding node = acc
        | otherwise     = countSteps' (acc+1) ds (applyDirection d node)


solve str = let
    (directionsStr : "" : nodeStrs) = lines str
    directions = parseDirections directionsStr
    nodes = parseNodes nodeStrs
    -- it just so happens that every ending node leads directly back to the start, so we can just
    -- take the LCM of all the numbers of steps
    in foldr1 lcm $ map (countSteps directions) nodes

main = puzzleInput "08" solve
test = puzzleInputTest "08b" $ putStrLn . show . solve

type State = (Label, Direction)
type StateHistory = [(State, Int)]


---- The following code was solely for exploratory purposes

-- Search through a StateHistory to find a given State. If it's found, return the list of step
-- numbers since then, and the list of step numbers before that; if not, return Nothing.
searchHistory :: State -> StateHistory -> Maybe ([Int], [Int])
searchHistory targetState history = case targetState `elemIndex` map fst history of
    Nothing -> Nothing
    Just i  -> Just $ splitAt (i+1) $ map snd history

-- Turn a result from searchHistory into an infinite list of step numbers.
cycleFromHistory :: State -> Int -> StateHistory -> Maybe [Int]
cycleFromHistory currState currStep history = case searchHistory currState history of
    Nothing -> Nothing
    Just (xs, ys) -> let
        toCycle = reverse xs
        intro = reverse ys
        cycleLength = currStep - head toCycle
        cycleWithShift is = is ++ cycleWithShift (map (+ cycleLength) is)
        in Just $ intro ++ cycleWithShift toCycle

-- Given a node and a list of directions, return the (infinite) list of step numbers on which a
-- ghost starting at the node is on an ending node.
endingSteps :: [Direction] -> Node -> [Int]
endingSteps = endingSteps' 0 [] where
    endingSteps' i history (d:ds) currNode = let
        Node currLabel _ = currNode
        currState = (currLabel, d)
        in case cycleFromHistory currState i history of
            -- if we've found a cycle
            Just is -> is
            -- if we haven't
            Nothing -> let
                nextNode = applyDirection d currNode
                nextHistory = if isEnding currNode
                    then (currState, i) : history
                    else history
                in endingSteps' (i+1) nextHistory ds nextNode

