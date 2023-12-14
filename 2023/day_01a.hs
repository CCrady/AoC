import System.IO
import Data.List
import Data.Char (isDigit)
import Data.Maybe

line2Num :: String -> Int
line2Num str = let
    firstDigit = fromJust $ find isDigit str
    lastDigit = fromJust $ find isDigit $ reverse str
    in read [firstDigit, lastDigit]

lines2Sum :: String -> Int
lines2Sum = sum . map line2Num . lines

main = do
    inFile <- openFile "input_01.txt" ReadMode
    inContents <- hGetContents inFile
    putStrLn $ show $ lines2Sum inContents
    hClose inFile

