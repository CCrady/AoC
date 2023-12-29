import Advent (puzzleInput, puzzleInputTest, split)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Char (isAlpha)


type Interval = (Int, Int)

size :: Interval -> Int
size (start, end) = end - start

splitInterval :: Int -> Interval -> (Interval, Interval)
splitInterval x (lo, hi) = ((lo, min x hi), (max x lo, hi))

data PartInterval = PartInterval {
    x :: Interval,
    m :: Interval,
    a :: Interval,
    s :: Interval
} deriving (Show, Read)

-- Split the PartInterval into two at the given value of the given field.
splitPartInterval :: Char -> Int -> PartInterval -> (PartInterval, PartInterval)
splitPartInterval 'x' i p = let (lo, hi) = splitInterval i (x p) in (p {x=lo}, p {x=hi})
splitPartInterval 'm' i p = let (lo, hi) = splitInterval i (m p) in (p {m=lo}, p {m=hi})
splitPartInterval 'a' i p = let (lo, hi) = splitInterval i (a p) in (p {a=lo}, p {a=hi})
splitPartInterval 's' i p = let (lo, hi) = splitInterval i (s p) in (p {s=lo}, p {s=hi})

filterPartInterval :: PartInterval -> Maybe PartInterval
filterPartInterval p@PartInterval{ x=x', m=m', a=a', s=s' }
    | size x' > 0 && size m' > 0 && size a' > 0 && size s' > 0 = Just p
    | otherwise = Nothing

numParts :: PartInterval -> Int
numParts partInt = product [ size (field partInt) | field <- [x,m,a,s] ]

type Workflow = [Rule]
type Rule = (Predicate, Result)
type Predicate = PartInterval -> (Maybe PartInterval, Maybe PartInterval)
data Result = Accept | Reject | Switch Workflow

parseResult :: Map String Workflow -> String -> Result
parseResult _ "A" = Accept
parseResult _ "R" = Reject
parseResult workflows str = Switch $ workflows ! str

parsePredicate :: String -> Predicate
parsePredicate (fieldChar : opChar : valStr) p = let
    val = read valStr :: Int
    splitVal = val + case opChar of
        '>' -> 1
        '<' -> 0
    (lo, hi) = splitPartInterval fieldChar splitVal p
    (pass, fail) = case opChar of
        '>' -> (hi, lo)
        '<' -> (lo, hi)
    in (filterPartInterval pass, filterPartInterval fail)

parseRule :: Map String Workflow -> String -> Rule
parseRule workflows str = case span (/= ':') str of
    (resultStr, []) -> let
        predicate p = (Just p, Nothing)
        result = parseResult workflows resultStr
        in (predicate, result)
    (predicateStr, ':' : resultStr) -> let
        predicate = parsePredicate predicateStr
        result = parseResult workflows resultStr
        in (predicate, result)

parseWorkflow :: Map String Workflow -> String -> (String, Workflow)
parseWorkflow workflows str = let
    (name, rulesStr) = span isAlpha str
    ruleStrs = split "," $ init $ tail rulesStr
    rules = map (parseRule workflows) ruleStrs
    in (name, rules)

parse :: String -> Workflow
parse str = let
    workflowStrs = takeWhile (not . null) $ lines str
    workflows = Map.fromList $ map (parseWorkflow workflows) workflowStrs
    in workflows ! "in"


applyResult :: Result -> PartInterval -> [PartInterval]
applyResult Accept p = [p]
applyResult Reject p = []
applyResult (Switch workflow) p = applyWorkflow workflow p

-- Given a Workflow and a PartInterval, return the set of PartIntervals that are accepted according
-- to the workflow.
applyWorkflow :: Workflow -> PartInterval -> [PartInterval]
applyWorkflow ((predicate, result) : rest) p = let
    (pass, fail) = predicate p
    passAccepted = case pass of
        Nothing -> []
        Just p' -> applyResult result p'
    failAccepted = case fail of
        Nothing -> []
        Just p' -> applyWorkflow rest p'
    in passAccepted ++ failAccepted


startingPartInterval = PartInterval {
    x = (1, 4001),
    m = (1, 4001),
    a = (1, 4001),
    s = (1, 4001)}

solve str = let
    workflow = parse str
    accepteds = applyWorkflow workflow startingPartInterval
    in sum $ map numParts accepteds
main = puzzleInput "19" solve
test = puzzleInputTest "19" $ putStrLn . show . solve

