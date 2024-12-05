module Advent
( puzzleInput
, puzzleInputTest
, dropPrefix
, splitFirst
, split
, Index
, Matrix
, parseMatrix
, rows
, cols
, Direction (..)
, shift
, nudge
, invert
, turnCW
, turnCCW
, inBounds
, OrderOn (..)
, orderWrap
, orderUnwrap
, orderOn
) where

import System.IO
import Data.List
import Data.Array


puzzleInput :: Show a => String -> (String -> a) -> IO ()
puzzleInput num func = do
    let fileName = "input_" ++ num ++ ".txt"
    file <- openFile fileName ReadMode
    contents <- hGetContents file
    putStrLn $ show $ func contents
    hClose file

puzzleInputTest :: String -> (String -> IO ()) -> IO ()
puzzleInputTest num func = do
    let fileName = "test_" ++ num ++ ".txt"
    file <- openFile fileName ReadMode
    contents <- hGetContents file
    func contents
    hClose file


-- Given a prefix and a string, drop the prefix from the start of the string. Return Nothing if the
-- string does not begin with the prefix.
dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix []     str    = Just str
dropPrefix _      []     = Nothing
dropPrefix (p:ps) (c:cs) = if p == c
    then dropPrefix ps cs
    else Nothing

-- Given a delimiter and a string, split the string at the first occurrence of the delimiter. Return
-- Nothing if the delimiter is not found.
splitFirst :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitFirst delim []         = Nothing
splitFirst delim str@(c:cs) = case dropPrefix delim str of
    Just rest -> Just ([], rest) -- if we found our delimiter
    Nothing -> case splitFirst delim cs of -- if we didn't find our delimiter
        Nothing          -> Nothing
        Just (pfx, rest) -> Just (c:pfx, rest)

-- Given a delimiter and a string, split the string at each occurrence of the delimiter. Note that,
-- in its current form, this will result in an infinite list of empty strings when the delimiter is
-- an empty string.
-- 'split d' is the inverse function of 'intercalate d'.
split :: Eq a => [a] -> [a] -> [[a]]
split delim str = case splitFirst delim str of
        Nothing          -> [str]
        Just (pfx, rest) -> pfx : split delim rest


type Index = (Int, Int)
type Matrix a = Array Index a

parseMatrix :: (Char -> a) -> String -> Matrix a
parseMatrix f str = let
    charGrid = lines str
    width = length $ head charGrid
    height = length charGrid
    assocs = [((x, y), f char) | (y, line) <- zip [1..] charGrid,
                                 (x, char) <- zip [1..] line]
    in array ((1, 1), (width, height)) assocs

rows :: Matrix a -> [[a]]
rows m = let
    (_, (width, height)) = bounds m
    in [[m ! (x, y) | x <- [1..width]]
                    | y <- [1..height]]

cols :: Matrix a -> [[a]]
cols m = let
    (_, (width, height)) = bounds m
    in [[m ! (x, y) | y <- [1..height]]
                    | x <- [1..width]]

data Direction = North | South | East | West deriving (Show, Eq, Ord, Ix)

shift :: Direction -> Int -> Index -> Index
shift North d (x, y) = (x, y - d)
shift South d (x, y) = (x, y + d)
shift East  d (x, y) = (x + d, y)
shift West  d (x, y) = (x - d, y)

nudge :: Direction -> Index -> Index
nudge = flip shift 1

invert :: Direction -> Direction
invert North = South
invert South = North
invert East  = West
invert West  = East

turnCW :: Direction -> Direction
turnCW North = East
turnCW East  = South
turnCW South = West
turnCW West  = North

turnCCW :: Direction -> Direction
turnCCW North = West
turnCCW West  = South
turnCCW South = East
turnCCW East  = North

inBounds :: (Index, Index) -> Index -> Bool
inBounds ((xLo, yLo), (xHi, yHi)) (x, y) = xLo <= x && x <= xHi
                                        && yLo <= y && y <= yHi


data OrderOn i a = OrderOn i a
instance Eq i => Eq (OrderOn i a) where
    (OrderOn i1 _) == (OrderOn i2 _) = i1 == i2
instance Ord i => Ord (OrderOn i a) where
    compare (OrderOn i1 _) (OrderOn i2 _) = compare i1 i2

orderWrap f x = OrderOn (f x) x
orderUnwrap (OrderOn _ x) = x

orderOn :: Ord i => (a -> i) -> ([OrderOn i a] -> OrderOn i a) -> [a] -> a
orderOn wrap f = orderUnwrap . f . map (\x -> OrderOn (wrap x) x)

