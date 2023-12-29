import Advent (puzzleInput, puzzleInputTest, split)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Char (isAlpha)


data Part = Part {
    x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
} deriving (Show, Read)

parsePart :: String -> Part
-- very convenient!
parsePart str = read ("Part" ++ str)

getField :: Char -> Part -> Int
getField 'x' = x
getField 'm' = m
getField 'a' = a
getField 's' = s

partSum :: Part -> Int
partSum part = x part + m part + a part + s part

type Workflow = [Rule]
type Rule = (Part -> Bool, Result)
data Result = Accept | Reject | Switch Workflow

parseResult :: Map String Workflow -> String -> Result
parseResult _ "A" = Accept
parseResult _ "R" = Reject
parseResult workflows str = Switch $ workflows ! str

parseRule :: Map String Workflow -> String -> Rule
parseRule workflows str = case span (/= ':') str of
    (resultStr, []) -> (const True, parseResult workflows resultStr)
    (fieldChar : opChar : valStr, ':' : resultStr) -> let
        fieldGetter = getField fieldChar
        op = case opChar of
            '>' -> (>)
            '<' -> (<)
        val = read valStr
        predicate part = fieldGetter part `op` val
        result = parseResult workflows resultStr
        in (predicate, result)

parseWorkflow :: Map String Workflow -> String -> (String, Workflow)
parseWorkflow workflows str = let
    (name, rulesStr) = span isAlpha str
    ruleStrs = split "," $ init $ tail rulesStr
    rules = map (parseRule workflows) ruleStrs
    in (name, rules)

parse :: String -> (Workflow, [Part])
parse str = let
    (workflowStrs, [] : partStrs) = span (not . null) $ lines str
    -- lazy recursion is so cool
    workflows = Map.fromList $ map (parseWorkflow workflows) workflowStrs
    parts = map parsePart partStrs
    in (workflows ! "in", parts)


applyWorkflow :: Workflow -> Part -> Result
applyWorkflow ((predicate, result) : rest) part = if predicate part
    then result
    else applyWorkflow rest part

accepted :: Workflow -> Part -> Bool
accepted workflow part = case applyWorkflow workflow part of
    Accept -> True
    Reject -> False
    Switch newWorkflow -> accepted newWorkflow part


solve str = let
    (workflow, parts) = parse str
    accepteds = filter (accepted workflow) parts
    in sum $ map partSum accepteds
main = puzzleInput "19" solve
test = puzzleInputTest "19" $ putStrLn . show . solve

