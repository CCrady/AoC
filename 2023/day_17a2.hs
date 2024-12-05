-- The previous version of this used immutable data structures and the State monad, which was
-- horrendously slow. Here it's been rewritten to use mutable arrays and the ST monad instead, for
-- an enormous speedup (when compiler optimizations are turned on). It could presumably be made even
-- faster by using a mutable heap instead of an immutable set, but that would violate the 'no
-- libraries' rule I set for myself for AoC 2023.
--
-- On my laptop, with ghc -O9, working on my puzzle input:
--   day_17a:  3m40s
--   day_17a2: 10s

import Advent (puzzleInput, puzzleInputTest,
    Matrix, Index, parseMatrix,
    Direction(..), nudge, turnCW, turnCCW,
    OrderOn(..))
import qualified Data.Array as Arr
import Data.Array.MArray
import Data.Array.ST (STArray)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.ST
import Data.STRef


-- Represents the total amount of heat we have to lose in order to reach a given LM
newtype HeatLoss = HeatLoss Int deriving (Eq, Show)
instance Ord HeatLoss where
    compare (HeatLoss (-1)) (HeatLoss (-1)) = EQ
    compare (HeatLoss (-1)) _               = GT
    compare _               (HeatLoss (-1)) = LT
    compare (HeatLoss x)    (HeatLoss y)    = compare x y
(HeatLoss x) +++ y = HeatLoss (x + y)
unreached = HeatLoss (-1)
data Momentum = Momentum Direction Int deriving (Eq, Ord, Arr.Ix, Show)
-- A state that a crucible can be in
type LocationMomentum = (Index, Momentum)

-- The input map
type Atlas = Matrix Int
type Bounds = (LocationMomentum, LocationMomentum)
-- Mutable array containing the best known heat losses to each possible crucible state so far
type LossArray s = STArray s LocationMomentum HeatLoss
-- Mutable set containing all the crucible states that are reachable from a known state but not yet
-- set in stone
type FrontierRef s = STRef s (Set.Set LocationMomentum)


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

reachableFrom :: LocationMomentum -> Bounds -> [LocationMomentum]
reachableFrom lm bnds = let
    newLms = [goStraight lm,
              turn turnCCW lm,
              turn turnCW lm]
    in filter (Arr.inRange bnds) newLms

isWinning :: LocationMomentum -> Bounds -> Bool
isWinning (loc, _) (_, (bottomRight, _)) = loc == bottomRight


-- Update the frontier to contain the given LMs
pushFrontier :: [LocationMomentum] -> FrontierRef s -> ST s ()
pushFrontier newLms frontierRef = modifySTRef frontierRef $ Set.union (Set.fromList newLms)

-- Pop the LM with the least heat loss from the frontier, and return it and the total heat loss in
-- getting there
popFrontier :: LossArray s -> FrontierRef s -> ST s (LocationMomentum, HeatLoss)
popFrontier lossArray frontierRef = do
    frontier <- readSTRef frontierRef
    let frontierLms = Set.toList frontier
    orderOns <- forM frontierLms $ \lm -> do
        heatLoss <- readArray lossArray lm
        return $ OrderOn heatLoss lm
    let (OrderOn heatLoss popped) = minimum orderOns
    let newFrontier = Set.delete popped frontier
    writeSTRef frontierRef newFrontier
    return (popped, heatLoss)

dijkstra :: Atlas -> Bounds -> LossArray s -> FrontierRef s -> ST s HeatLoss
dijkstra atlas bnds lossArray frontierRef = do
    (searchLm, currentLoss) <- popFrontier lossArray frontierRef
    let nextNeighbors = reachableFrom searchLm bnds
    -- list of associations between an LM and the total heat loss we'd get from moving there from
    -- the current LM
    let newAssocs = map f nextNeighbors where
        f lm@(loc, _) = (lm, currentLoss +++ (atlas Arr.! loc))
    -- just those LM-HeatLoss associations where the heat loss is better than the previous best
    improvedAssocs <- let
        f :: LossArray s -> (LocationMomentum, HeatLoss) -> ST s Bool
        f lossArray (lm, heatLoss) = fmap (heatLoss <) (readArray lossArray lm)
        in filterM (f lossArray) newAssocs
    -- update lossArray with the new bests
    forM_ improvedAssocs $ uncurry (writeArray lossArray)
    -- if we've improved the heat loss at a particular LM then at some point we'll need to update
    -- its neighbors accordingly
    let toPush = map fst improvedAssocs
    pushFrontier toPush frontierRef
    -- base case & tail recursion
    if isWinning searchLm bnds
        then return currentLoss
        else dijkstra atlas bnds lossArray frontierRef


lossArrayBounds :: Atlas -> Bounds
lossArrayBounds atlas = let
    (lo, hi) = Arr.bounds atlas
    in ((lo, Momentum North 1), (hi, Momentum West 3))

getEmptyLossArray :: Atlas -> Bounds -> ST s (LossArray s)
getEmptyLossArray atlas bnds = newListArray bnds $ repeat unreached

-- Set up a LossArray and a FrontierRef corresponding to the initial state where we start in the
-- northwest corner
getStartState :: Atlas -> Bounds -> ST s (LossArray s, FrontierRef s)
getStartState atlas bnds = do
    let northStart = ((1, 1), Momentum North 1)
    let westStart  = ((1, 1), Momentum West  1)
    lossArray <- getEmptyLossArray atlas bnds
    writeArray lossArray northStart (HeatLoss 0)
    writeArray lossArray westStart  (HeatLoss 0)
    let frontier = Set.fromList [northStart, westStart]
    frontierRef <- newSTRef frontier
    return (lossArray, frontierRef)


solve str = runST $ do
    let atlas = parse str
    let bnds = lossArrayBounds atlas
    (lossArray, frontierRef) <- getStartState atlas bnds
    dijkstra atlas bnds lossArray frontierRef
main = puzzleInput "17" solve
test = puzzleInputTest "17" $ putStrLn . show . solve

