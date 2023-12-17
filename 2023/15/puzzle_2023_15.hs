module Puzzle_2023_15 where

import Automation (submitAnswer)
import Parsing
import Data.Char (ord)
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M

-- data types
data Lens = Lens { _label :: String
                 , _focal :: Int} deriving (Eq, Ord, Show)

data Step = Step { _lens :: String
                 , _op :: Operation} deriving (Eq, Ord, Show)

data Operation = Replace | Fill Int deriving (Eq, Ord, Show)

type Boxes = Map Int [Lens]

-- parsing
inputP :: Parser [String]
inputP = sepBy (some characterP) (char ',')

stepsP :: Parser [Step]
stepsP = sepBy stepP (char ',')

stepP :: Parser Step
stepP = do
    lens <- some letterChar
    op <- replaceP <|> fillP
    return $ Step lens op

replaceP :: Parser Operation
replaceP = char '-' >> return Replace

fillP :: Parser Operation
fillP = do
    char '='
    focal <- natural
    return $ Fill focal


characterP :: Parser Char
characterP = alphaNumChar <|> char '=' <|> char '-'

-- functions
hash :: String -> Int
hash s = hash' 0 s
  where hash' :: Int -> String -> Int
        hash' i [] = i
        hash' i (c:cs) = hash' i' cs
          where i' = mod (17 * (i + (ord c))) 256


fillBoxes :: [Step] -> Boxes
fillBoxes steps = foldl operator emptyBoxes steps
  where emptyBoxes = M.fromList $ zip [0..255] (repeat [])


operator :: Boxes -> Step -> Boxes
operator boxes (Step s Replace) = replacer boxes s
operator boxes (Step s (Fill n)) = filler boxes s n


replacer :: Boxes -> String -> Boxes
replacer boxes s = M.insert h replaced boxes
  where h = hash s
        extant = boxes M.! h
        replaced = filter (\l -> _label l /= s) extant


filler :: Boxes -> String -> Int -> Boxes
filler boxes s n = M.insert h filled boxes
  where h = hash s
        extant = boxes M.! h
        filled = fill extant (Lens s n)

fill :: [Lens] -> Lens -> [Lens]
fill [] lens = [lens]
fill (l:ls) lens
    | _label l == _label lens = lens : ls
    | otherwise               = l : fill ls lens


configPower :: Map Int [Lens] -> Int
configPower boxes = sum . map boxPower $ M.toList boxes

boxPower :: (Int, [Lens]) -> Int
boxPower (box, lenses) = sum . map (uncurry $ lensPower box) $ zip [0..] lenses

lensPower :: Int -> Int -> Lens -> Int
lensPower box pos lens = (box + 1) * (pos + 1) * (_focal lens)

-- mains

mainA :: IO ()
mainA = do
    (Just input) <- parseInput inputP "15/input.txt"
    let answer = sum . map hash $ input
    print answer
    -- result <- submitAnswer 2023 15 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just steps) <- parseInput stepsP "15/input.txt"
    let answer = configPower . fillBoxes $ steps
    print answer
    -- result <- submitAnswer 2023 15 2 answer
    -- print result
    return ()
