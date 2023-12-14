import Advent

data Race = Race Int Int

parseRace :: String -> Race
parseRace str = let
    [timeStr, distStr] = lines str
    time = read $ concat $ tail $ words timeStr
    dist = read $ concat $ tail $ words distStr
    in Race time dist


-- A binary search or newton's method would have a better complexity class but for such small
-- values it's unnecessary
minWinningHold :: Race -> Int
minWinningHold (Race time dist) = head $ dropWhile (not . isWinningHold) $ [0..time] where
    isWinningHold n = n * (time - n) > dist

numWinningHolds :: Race -> Int
numWinningHolds race@(Race time dist) = let
    minWin = minWinningHold race
    maxWin = time - minWin
    in maxWin - minWin + 1


solve = numWinningHolds . parseRace
main = puzzleInput "06" solve

