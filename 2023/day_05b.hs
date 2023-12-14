import Advent
import Data.List
import Data.Maybe (catMaybes)

data NumRange = NumRange Int Int deriving (Show, Eq, Ord)

rStart :: NumRange -> Int
rStart (NumRange s _) = s

rEnd :: NumRange -> Int
rEnd (NumRange s n) = s + n

fromStartEnd :: Int -> Int -> NumRange
fromStartEnd s e = NumRange s (e - s)

hasElements :: NumRange -> Bool
hasElements (NumRange _ n) = n > 0

intersectRange :: NumRange -> NumRange -> NumRange
intersectRange x y = let
    maxStart = max (rStart x) (rStart y)
    minEnd = min (rEnd x) (rEnd y)
    in fromStartEnd maxStart minEnd

areAdjacent :: NumRange -> NumRange -> Bool
areAdjacent x y = let
    (NumRange _ n) = intersectRange x y
    in n >= 0

areOverlapping :: NumRange -> NumRange -> Bool
areOverlapping x y = hasElements $ intersectRange x y

-- Does y contain x?
isInside :: NumRange -> NumRange -> Bool
isInside x y = rStart x >= rStart y && rEnd x <= rEnd y

normalizeSet :: [NumRange] -> [NumRange]
normalizeSet = catenate . sort . filter hasElements where
    catenate (x:y:rest)
        | areAdjacent x y = let
            minStart = min (rStart x) (rStart y)
            maxEnd = max (rEnd x) (rEnd y)
            merged = fromStartEnd minStart maxEnd
            in catenate (merged : rest)
        | otherwise       = x : catenate (y:rest)
    catenate xs = xs

unionSets :: [[NumRange]] -> [NumRange]
unionSets = normalizeSet . concat

-- Take the intersection of the set with the range
limitSet :: NumRange -> [NumRange] -> [NumRange]
limitSet l = filter hasElements . map (intersectRange l)

excludeRange :: NumRange -> NumRange -> [NumRange]
excludeRange x y
    | not $ areOverlapping x y = [x]
    | x `isInside` y           = []
    | rEnd x <= rEnd y         = [fromStartEnd (rStart x) (rStart y)]
    | rStart x >= rStart y     = [fromStartEnd (rEnd y) (rEnd x)]
    | otherwise                = [fromStartEnd (rStart x) (rStart y),
                                  fromStartEnd (rEnd y) (rEnd x)]

-- Do a set difference of xs \ ys.
excludeSet :: [NumRange] -> [NumRange] -> [NumRange]
excludeSet xs ys = excludeSet' (normalizeSet xs) (normalizeSet ys) where
    excludeSet' [] _  = []
    excludeSet' xs [] = xs
    excludeSet' (x:xs) (y:ys)
        | areOverlapping x y = let
            xs' = excludeRange x y ++ xs
            in excludeSet' xs' (y:ys)
        | x < y = x : excludeSet' xs (y:ys)
        | x > y = excludeSet' (x:xs) ys


data MapRange = MapRange Int Int Int deriving (Show)

parseMapRange :: String -> MapRange
parseMapRange str = let
    [dest, source, len] = map read $ words str
    in MapRange dest source len

preimage :: MapRange -> NumRange
preimage (MapRange _ start len) = NumRange start len

-- Apply the given MapRange to the set. Return a pair (s', sRest) where s' is the resulting set that
-- you get after applying the the MapRange to just the portion of the set that it affects, and sRest
-- is the rest of the set that was outside MapRange's range.
applyMapRange :: [NumRange] -> MapRange -> ([NumRange], [NumRange])
applyMapRange set mr = let
    (MapRange dest source _) = mr
    nudgeNumRange (NumRange start amt) = NumRange (start - source + dest) amt
    preimage' = preimage mr
    mapped = map nudgeNumRange $ limitSet preimage' set
    unmapped = excludeSet set [preimage']
    in (mapped, unmapped)

parseMap :: String -> [MapRange]
parseMap = map parseMapRange . tail . lines

applyMap :: [NumRange] -> [MapRange] -> [NumRange]
applyMap set = normalizeSet . applyMap' set where
    applyMap' s [] = s
    applyMap' s (mr:mrs) = let
        (sMapped, sRest) = applyMapRange s mr
        in sMapped ++ applyMap' sRest mrs


parseSeeds :: String -> [NumRange]
parseSeeds = parseSeeds' . map read . tail . words where
    parseSeeds' [] = []
    parseSeeds' (x:y:rest) = NumRange x y : parseSeeds' rest


solve str = let
    (seedsStr : mapStrs) = split "\n\n" str
    seeds = parseSeeds seedsStr
    maps = map parseMap mapStrs
    locations = foldl' applyMap seeds maps
    in (\(NumRange s _) -> s) minimum locations

main = puzzleInput "05" solve

