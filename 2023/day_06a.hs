import Advent

data Race = Race Int Int

parseRaces :: String -> [Race]
parseRaces str = let
    [timeStr, distStr] = lines str
    times = map read $ tail $ words timeStr
    dists = map read $ tail $ words distStr
    in zipWith Race times dists


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


solve = product . map numWinningHolds . parseRaces
main = puzzleInput "06" solve

