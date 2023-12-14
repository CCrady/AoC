import System.IO
import Data.List
import Data.Maybe (fromJust)

digitNames = [
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9"]
digitAssocs = zip digitNames ([0..9] ++ [0..9])
digit2Int = fromJust . flip lookup digitAssocs

firstDigit :: String -> Int
firstDigit str = case find (flip isPrefixOf str) digitNames of
    Just digit -> digit2Int digit
    Nothing    -> firstDigit $ tail str

lastDigit :: String -> Int
lastDigit str = case find (flip isSuffixOf str) digitNames of
    Just digit -> digit2Int digit
    Nothing    -> lastDigit $ init str

line2Int :: String -> Int
line2Int str = 10 * (firstDigit str) + (lastDigit str)

lines2Sum :: String -> Int
lines2Sum = sum . map line2Int . lines

main = do
    inFile <- openFile "input_01.txt" ReadMode
    inContents <- hGetContents inFile
    putStrLn $ show $ lines2Sum inContents
    hClose inFile

