import Advent (puzzleInput, puzzleInputTest)
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


followPath :: PipeGrid -> Heading -> (Heading, Pipe)
followPath grid (loc, dir) = let
    loc' = nudge dir loc
    dirs' = grid ! loc'
    dir' = (\[x] -> x) $ delete (invert dir) dirs'
    in ((loc', dir'), dirs')

loopPath :: PipeGrid -> Index -> [(Index, Pipe)]
loopPath grid startLoc = startTile : loopPath' (startLoc, startDir) where
    startPipe@(startDir : _) = grid ! startLoc
    startTile = (startLoc, startPipe)
    loopPath' heading = let
        (newHeading@(newLoc, _), newPipe) = followPath grid heading
        in if newLoc == startLoc
            then []
            else (newLoc, newPipe) : loopPath' newHeading

-- Like groupBy, but compares each adjacent pair of elements
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' _ [x] = [[x]]
groupBy' f (x:y:rest) = let
    (yGroup:restGroup) = groupBy' f (y:rest)
    in if f x y
        then (x : yGroup) : restGroup
        else [x] : (yGroup : restGroup)

-- TODO: rewrite this so that it doesn't duplicate work, making the program run in a reasonable
-- amount of time
isInside :: [(Index, Pipe)] -> Index -> Bool
isInside loop loc@(locX, locY) = let
    isPartOfLoop = loc `elem` map fst loop
    loop' = sort $ filter toLeftOfLoc loop where
        toLeftOfLoc ((x, y), _) = x <= locX && y == locY
    segments = groupBy' areConnected loop' where
        -- We can ignore a lot of extra checks because loop' is a single row sorted from West to
        -- East. Also note that if the two tiles are adjacent and the left one has an East
        -- connection then the right one must also have a West connection.
        areConnected ((x1, _), p1) ((x2, _), _) = x2 - x1 == 1 && East `elem` p1
    -- Does the given segment cross the horizontal, i.e. does it connect the area above with the
    -- area below. Note that for a crossing segment, either a) the segment is a single vertical
    -- pipe, or b) the leftmost pipe connects to the East, the rightmost pipe connects to the West,
    -- and their vertical connections are opposite.
    isSegmentCrossing segment = let
        isSingleton [_] = True
        isSingleton _   = False
        (_, leftmostPipe)  = head segment
        (_, rightmostPipe) = last segment
        in isSingleton segment || null (intersect leftmostPipe rightmostPipe)
    crossingSegments = filter isSegmentCrossing segments
    -- The loop always separates the inside from the outside, so every time we cross it we change
    -- from inside to outside or vice-versa.
    in not isPartOfLoop && odd (length crossingSegments)

numInside :: [(Index, Pipe)] -> [Index] -> Int
numInside loop = length . filter (isInside loop)


solve str = let
    (grid, startLoc) = parse str
    (_, (width, height)) = bounds grid
    loop = loopPath grid startLoc
    in numInside loop [(x, y) | y <- [1..height], x <- [1..width]]

test str = let
    (grid, startLoc) = parse str
    (_, (width, height)) = bounds grid
    loop = loopPath grid startLoc
    showBool True = 'I'
    showBool False = '.'
    showGrid = [[showBool $ isInside loop (x, y) | x <- [1..width]] | y <- [1..height]]
    in unlines showGrid

main = puzzleInput "10" solve
testInput = puzzleInputTest "10" $ putStrLn . test

