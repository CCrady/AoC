import Advent (puzzleInput, puzzleInputTest, split)
import Data.Array
import Data.List
import Data.Char (ord, isAlpha)
import Data.Function (on)

type Label = String
type FocalLength = Int
type Lens = (Label, FocalLength)
type Instruction = State -> State
type Box = [Lens]
type State = Array Int Box

hash :: Label -> Int
hash = hash' 0 where
    hash' acc [] = acc
    hash' acc (c:cs) = let
        acc' = ((acc + ord c) * 17) `mod` 256
        in hash' acc' cs

applyDash :: Label -> Box -> Box
-- this is a kinda janky way to delete the first lens with the given label
applyDash label = deleteBy ((==) `on` fst) (label, 0)

applyEquals :: Lens -> Box -> Box
applyEquals lens@(label, _) = applyEquals' where
    applyEquals' [] = [lens]
    applyEquals' (lens'@(label', _) : rest)
        | label' == label = lens  : rest
        | otherwise       = lens' : applyEquals' rest

parseInstruction :: String -> Instruction
parseInstruction str state = let
    (label, op : arg) = span isAlpha str
    boxNum = hash label
    boxTransformation = case op of
        '-' -> applyDash label
        '=' -> applyEquals (label, read arg)
    box = state ! boxNum
    in state // [(boxNum, boxTransformation box)]

parseInstructions :: String -> [Instruction]
parseInstructions = map parseInstruction . split "," . concat . lines

applyInstructions :: State -> [Instruction] -> State
applyInstructions = foldl' (flip ($))

startState :: State
startState = listArray (0, 255) $ replicate 256 []

statePower :: State -> Int
statePower = sum . map boxPower . assocs where
    boxPower (i, ls) = (i+1)
                     * (sum $ zipWith (*) [1..] $ map snd ls)


solve = statePower . applyInstructions startState . parseInstructions
main = puzzleInput "15" solve
test = puzzleInputTest "15" $ putStrLn . show . solve

