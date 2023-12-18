import Advent
import Data.Array
import Data.Maybe (fromJust)
import Data.List


type Pipe = [Direction]
data Direction = North | South | East | West deriving (Eq, Ord, Show)
type Index = (Int, Int)
type Heading = (Index, Direction)
type PipeGrid = Array Index Pipe

invert :: Direction -> Direction
invert North = South
invert South = North
invert East  = West
invert West  = East

nudge :: Direction -> Index -> Index
nudge North (x, y) = (x, y-1)
nudge South (x, y) = (x, y+1)
nudge East  (x, y) = (x+1, y)
nudge West  (x, y) = (x-1, y)

char2pipeMap =
    [ ('|', [North, South])
    , ('-', [East, West])
    , ('L', [North, East])
    , ('J', [North, West])
    , ('7', [South, West])
    , ('F', [South, East])
    , ('.', [])
    , ('S', [])
    ]

char2pipe :: Char -> Pipe
char2pipe = fromJust . flip lookup char2pipeMap

pipe2char :: Pipe -> Char
pipe2char = fromJust . flip lookup (map (\(x, y) -> (y, x)) char2pipeMap)

showPipeGrid :: PipeGrid -> String
showPipeGrid grid = let
    (_, (width, height)) = bounds grid
    groupN _ [] = []
    groupN n xs = let
        (curr, rest) = splitAt n xs
        in curr : groupN n rest
    transpose = ixmap ((1, 1), (height, width)) (\(x, y) -> (y, x))
    in unlines
     $ groupN height
     $ elems
     $ transpose
     $ fmap pipe2char grid

parse :: String -> (PipeGrid, Index)
parse str = let
    charGrid = lines str
    charAssocs = [((x, y), char) | (y, line) <- zip [1..] charGrid,
                                   (x, char) <- zip [1..] line    ]
    height = length charGrid
    width = length $ head charGrid
    charArray = array ((1, 1), (width, height)) charAssocs
    (startLoc, _) = fromJust $ find (\(_, t) -> t == 'S') charAssocs
    pipeArray = fmap char2pipe charArray
    startValue = let
        newLoc dir = nudge dir startLoc
        isInBounds (x, y) = x >= 1 && y >= 1 && x <= width && y <= height
        dirsToCheck = filter (\dir -> isInBounds $ newLoc dir) [North, South, East, West]
        isConnected dir = elem (invert dir) $ pipeArray ! newLoc dir
        in filter isConnected dirsToCheck
    in (pipeArray // [(startLoc, startValue)], startLoc)


followPath :: PipeGrid -> Heading -> Heading
followPath grid (loc, dir) = let
    loc' = nudge dir loc
    dirs' = grid ! loc'
    dir' = (\[x] -> x) $ delete (invert dir) dirs'
    in (loc', dir')

loopLength :: PipeGrid -> Index -> Int
loopLength grid startLoc = loopLength' 1 (startLoc, startDir) where
    startDir = head $ grid ! startLoc
    loopLength' acc heading = let
        newHeading@(newLoc, _) = followPath grid heading
        in if newLoc == startLoc
            then acc
            else loopLength' (acc+1) newHeading


solve = (`div` 2) . uncurry loopLength . parse

main = puzzleInput "10" solve

