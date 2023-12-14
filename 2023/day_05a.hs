import Advent
import Data.List

data MapRange = MapRange Int Int Int

parseMapRange :: String -> MapRange
parseMapRange str = let
    [dest, source, len] = map read $ words str
    in MapRange dest source len

applyMapRange :: Int -> MapRange -> Maybe Int
applyMapRange n (MapRange dest source len)
    | n < source || n >= source + len = Nothing
    | otherwise = Just (n - source + dest)


parseMap :: String -> [MapRange]
parseMap = map parseMapRange . tail . lines

applyMap :: Int -> [MapRange] -> Int
applyMap n (r:rs) = case applyMapRange n r of
    Just n' -> n'
    Nothing -> applyMap n rs
applyMap n [] = n

applyMaps :: Int -> [[MapRange]] -> Int
applyMaps = foldl' applyMap


parseSeeds :: String -> [Int]
parseSeeds = map read . tail . words


solve str = let
    (seedsStr : mapStrs) = split "\n\n" str
    seeds = parseSeeds seedsStr
    maps = map parseMap mapStrs
    locations = map (\s -> applyMaps s maps) seeds
    in minimum locations

main = puzzleInput "05" solve

