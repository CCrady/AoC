module Advent (puzzleInput, dropPrefix, splitFirst, split) where
import System.IO
import Data.List


puzzleInput :: Show a => String -> (String -> a) -> IO ()
puzzleInput num func = do
    let fileName = "input_" ++ num ++ ".txt"
    file <- openFile fileName ReadMode
    contents <- hGetContents file
    putStrLn $ show $ func contents
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


