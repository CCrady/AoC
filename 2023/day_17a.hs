import Advent (puzzleInput, puzzleInputTest,
    Matrix, Index, parseMatrix,
    Direction(..), nudge, turnCW, turnCCW,
    OrderOn(..), orderWrap)
import Data.List
import qualified Data.Array as Arr
import Data.Array (Array, (!), (//))
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.Trans.State.Strict


newtype HeatLoss = HeatLoss Int deriving (Eq, Show)
instance Ord HeatLoss where
    compare (HeatLoss (-1)) (HeatLoss (-1)) = EQ
    compare (HeatLoss (-1)) _               = GT
    compare _               (HeatLoss (-1)) = LT
    compare (HeatLoss x)    (HeatLoss y)    = compare x y
(HeatLoss x) +++ y = HeatLoss (x + y)
unreached = HeatLoss (-1)
data Momentum = Momentum Direction Int deriving (Eq, Ord, Arr.Ix, Show)
type LocationMomentum = (Index, Momentum)

type Atlas = Matrix Int
type LossArray = Array LocationMomentum HeatLoss
type Frontier = Set LocationMomentum

type DijkstraState = State (LossArray, Frontier)


parse :: String -> Atlas
parse = parseMatrix (\c -> read [c])


turn :: (Direction -> Direction) -> LocationMomentum -> LocationMomentum
turn whichWay (loc, Momentum dir _) = let
    newDir = whichWay dir
    newLoc  = nudge newDir loc
    in (newLoc, Momentum newDir 1)

goStraight :: LocationMomentum -> LocationMomentum
goStraight (loc, Momentum dir inertia) = let
    newLoc = nudge dir loc
    newInertia  = inertia + 1
    in (newLoc, Momentum dir newInertia)

reachableFrom :: LocationMomentum -> LossArray -> [LocationMomentum]
reachableFrom lm lossArray = let
    newLms = [goStraight lm,
              turn turnCCW lm,
              turn turnCW lm]
    bnds = Arr.bounds lossArray
    in filter (Arr.inRange bnds) newLms


isWinning :: LocationMomentum -> Atlas -> Bool
isWinning (loc, _) atlas = let
    (_, bottomRight) = Arr.bounds atlas
    in loc == bottomRight


pushFrontier :: [LocationMomentum] -> DijkstraState ()
pushFrontier newLms = do
    (lossArray, frontier) <- get
    let newFrontier = Set.union frontier $ Set.fromList newLms
    put (lossArray, newFrontier)

popFrontier :: DijkstraState (LocationMomentum, HeatLoss)
popFrontier = do
    (lossArray, frontier) <- get
    let (OrderOn heatLoss popped) = minimum $ map (orderWrap (lossArray !)) $ Set.toList frontier
    let newFrontier = Set.delete popped frontier
    put (lossArray, newFrontier)
    return (popped, heatLoss)

dijkstra :: Atlas -> DijkstraState HeatLoss
dijkstra atlas = do
    (searchLm, currentLoss) <- popFrontier
    (lossArray, frontier) <- get
    let nextNeighbors = reachableFrom searchLm lossArray
    let newAssocs = map f nextNeighbors where
        f lm@(loc, _) = (lm, currentLoss +++ (atlas ! loc))
    let improvedAssocs = filter f newAssocs where
        f (lm, heatLoss) = heatLoss < lossArray ! lm
    let newLossArray = lossArray // improvedAssocs
    put (newLossArray, frontier)
    pushFrontier $ map fst improvedAssocs
    if isWinning searchLm atlas
        then return currentLoss
        else dijkstra atlas

emptyLossArray :: Atlas -> LossArray
emptyLossArray atlas = let
    (lo, hi) = Arr.bounds atlas
    bnds = ((lo, Momentum North 1), (hi, Momentum West 3))
    in Arr.listArray bnds $ repeat unreached

startState :: Atlas -> (LossArray, Frontier)
startState atlas = let
    northStart = ((1, 1), Momentum North 1)
    westStart  = ((1, 1), Momentum West  1)
    lossArray = emptyLossArray atlas // [
        (northStart, HeatLoss 0),
        (westStart,  HeatLoss 0)]
    frontier = Set.fromList [northStart, westStart]
    in (lossArray, frontier)


solve str = let
    atlas = parse str
    initialState = startState atlas
    in evalState (dijkstra atlas) initialState
main = puzzleInput "17" solve
test = puzzleInputTest "17" $ putStrLn . show . solve

