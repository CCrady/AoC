import Advent (puzzleInput,
               puzzleInputTest,
               Matrix,
               Index,
               parseMatrix,
               rows,
               Direction (..),
               nudge,
               inBounds)
import Data.Array
import Data.List (nub, sort)

type Contraption = Matrix Char
type Beams = Matrix [Direction]

parseContraption :: String -> Contraption
parseContraption = parseMatrix id

startState = ((0, 0), East)

nextStates :: Contraption -> (Index, Direction) -> [(Index, Direction)]
nextStates contraption (loc, dir) = zipWith nudge' nextDirs (repeat loc) where
    nudge' dir loc = (nudge dir loc, dir)
    nextDirs = case contraption ! loc of
        '.'  -> [dir]
        '/'  -> case dir of
            North -> [East]
            East  -> [North]
            South -> [West]
            West  -> [South]
        '\\' -> case dir of
            North -> [West]
            East  -> [South]
            South -> [East]
            West  -> [North]
        '|'  -> case dir of
            North -> [North]
            East  -> [North, South]
            South -> [South]
            West  -> [North, South]
        '-'  -> case dir of
            North -> [East, West]
            East  -> [East]
            South -> [East, West]
            West  -> [West]

addBeam :: (Index, Direction) -> Beams -> Beams
addBeam (loc, dir) beams = let
    currDirs = beams ! loc
    newDirs = nub $ dir : currDirs
    in beams // [(loc, newDirs)]

findEnergized :: Contraption -> Beams
findEnergized contraption = propagate startBeams [startState] where
    bnds@(_, (width, height)) = bounds contraption
    startState = ((1, 1), East)
    startBeams = listArray bnds $ replicate (width * height) []
    --
    propagate :: Beams -> [(Index, Direction)] -> Beams
    propagate = foldr propagate' where
        propagate' :: (Index, Direction) -> Beams -> Beams
        propagate' state beams = let
            beams' = addBeam state beams
            valid (loc, dir) = inBounds bnds loc
                            && dir `notElem` (beams ! loc)
            states' = filter valid $ nextStates contraption state
            in propagate beams' states'

countEnergized :: Beams -> Int
countEnergized = length . filter (not . null) . elems

showBeams :: Beams -> String
showBeams = unlines . map (map dirs2char) . rows where
    dirs2char dirs = case sort dirs of
        [] -> '.'
        [North] -> '^'
        [East] -> '>'
        [South] -> 'v'
        [West] -> '<'
        [North, South] -> '|'
        [East, West] -> '-'
        _ -> '+'


solve = countEnergized . findEnergized . parseContraption
main = puzzleInput "16" solve
test = puzzleInputTest "16" $ putStrLn . test' . findEnergized . parseContraption where
    test' beams = show (countEnergized beams) ++ "\n\n" ++ showBeams beams

