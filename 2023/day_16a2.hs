import Advent (puzzleInput,
               puzzleInputTest,
               Matrix,
               Index,
               parseMatrix,
               rows,
               Direction (..),
               nudge,
               inBounds,
               invert)
import Data.Array
import qualified Data.Set as Set

type Contraption = Matrix Char
type Beams = Matrix (Set.Set Direction)
type Heading = (Index, Direction)

parseContraption :: String -> Contraption
parseContraption = parseMatrix id

startState = ((0, 0), East)

nextStates :: Contraption -> Heading -> [Heading]
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

addBeams :: Beams -> [Heading] -> Beams
addBeams = accum (flip Set.insert)

findEnergized :: Contraption -> Beams
findEnergized contraption = propagate startState startBeams where
    bnds@(_, (width, height)) = bounds contraption
    inBounds' = inBounds bnds
    startState = ((1, 1), East)
    startBeams = listArray bnds $ replicate (width * height) Set.empty
    -- Given the current configuration of illuminated tiles and a heading, return the updated
    -- configuration after light has been cast from the heading.
    propagate :: Heading -> Beams -> Beams
    propagate heading@(loc, dir) beams = let
        (emptyLocs, maybeHitLoc) = castRay heading beams
        illuminatedLocs = case maybeHitLoc of
            Nothing     -> emptyLocs
            Just hitLoc -> hitLoc : emptyLocs
        beamUpdate = map (\l -> (l, dir)) illuminatedLocs
        beams' = addBeams beams beamUpdate
        nextHeadings = case maybeHitLoc of
            Nothing     -> []
            Just hitLoc -> nextStates contraption (hitLoc, dir)
        in foldr propagate beams' nextHeadings
    -- Cast a ray from a heading inside the contraption. Return the list of indices that the ray
    -- passed through, and the index that the ray hit (if any).
    castRay :: Heading -> Beams -> ([Index], Maybe Index)
    castRay (startLoc, dir) beams = let
        isLocEmpty loc = inBounds' loc && contraption ! loc == '.'
        isOutOfBounds = not $ inBounds' startLoc
        -- this could use cleaning up but ehhhhhhhh
        isRedundant = isLocEmpty startLoc
            && (not $ null $ Set.intersection (Set.fromList [dir, invert dir]) (beams ! startLoc))
        rayLocs = iterate (nudge dir) startLoc
        (emptyLocs, hitLoc : _) = span isLocEmpty rayLocs
        includeHit = inBounds' hitLoc
                  && dir `notElem` beams ! hitLoc
        maybeHitLoc = if includeHit then Just hitLoc else Nothing
        in if isOutOfBounds || isRedundant
            then ([], Nothing)
            else (emptyLocs, maybeHitLoc)

countEnergized :: Beams -> Int
countEnergized = length . filter (not . null) . elems

showBeams :: Beams -> String
showBeams = unlines . map (map dirs2char) . rows where
    dirs2char dirs = case Set.elems dirs of
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

