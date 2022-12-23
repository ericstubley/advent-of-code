module Puzzle_2022_22 where

import Automation (submitAnswer)
import Parsing
import Utilities (CyclicEnum(..))
import Grid (Direction(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.RWS.Class

-- data types
data Instruction = TurnR | TurnL | Forward Int deriving (Eq, Ord, Show)

data Domain = Wrap | Open | Wall deriving (Eq, Ord, Show)

type Coord = (Int, Int)

type Board = Map Coord Domain

type Instructions = [Instruction]

-- because it's position and velocity..(direction)
type Quantum = (Coord, Direction)

-- parsing
inputP :: Parser (Board, Instructions)
inputP = do
    domains <- some (rowP <* newline)
    newline
    instructions <- instructionsP
    return (createBoard domains, instructions)


rowP :: Parser [Domain]
rowP = some domainP


domainP :: Parser Domain
domainP = (char ' ' >> return Wrap)
      <|> (char '.' >> return Open)
      <|> (char '#' >> return Wall)


instructionsP :: Parser Instructions
instructionsP = some instructionP


instructionP :: Parser Instruction
instructionP = (Forward <$> natural)
           <|> (char 'R' >> return TurnR)
           <|> (char 'L' >> return TurnL)


createBoard :: [[Domain]] -> Board
createBoard domains = M.filter (/= Wrap) . M.fromList $ indexedDomains where
    indexedColumns = map (zip [1..]) domains
    indexedDomains = concat $ zipWith (\r col -> map (\t -> ((r, fst t), snd t)) col) [1..] indexedColumns



-- functions
extract :: Quantum -> Int
extract ((r, c), d) = 1000*r + 4*c + m
  where m = case d of
                North -> 3
                East  -> 0
                South -> 1
                West  -> 2


end :: Board -> Instructions -> Quantum
end board instructions = fst $ execRWS (follow instructions) board (start board)



follow :: MonadRWS Board () Quantum m => Instructions -> m ()
follow [] = return ()
follow (i:is) = case i of
                TurnR -> modify (\q -> (fst q, csucc $ snd q)) >> follow is
                TurnL -> modify (\q -> (fst q, cpred $ snd q)) >> follow is
                (Forward n) -> step n >> follow is


step :: MonadRWS Board () Quantum m => Int -> m ()
step 0 = return ()
step n = do
    ((r', c'), f) <- quantumAhead
    domain <- reader $ M.findWithDefault Wrap (r', c')
    case domain of
        Wrap -> error $ "Coordinate ahead should never be a Wrap" ++ show ((r', c'), f)
        Open -> put ((r', c'), f) >> step (n-1)
        Wall -> return ()


-- check if the simple version is in the map
-- if not, wrap
quantumAhead :: MonadRWS Board () Quantum m => m Quantum
quantumAhead = do
    ((r, c), f) <- get
    let (dr, dc) = stepDir f
    let (r', c') = (r + dr, c + dc)
    domain <- reader $ M.findWithDefault Wrap (r', c')
    case domain of
        Open -> return ((r', c'), f)
        Wall -> return ((r', c'), f)
        -- Wrap -> do
        --     (r'', c'') <- wrap
        --     return ((r'', c''), f)
        Wrap -> netWrap



stepDir :: Direction -> Coord
stepDir d = case d of
                North -> (-1, 0)
                East  -> (0, 1)
                South -> (1, 0)
                West  -> (0, -1)

-- rules for wrapping occur here
wrap :: MonadRWS Board () Quantum m => m Coord
wrap = do
    ((r, c), f) <- get
    case f of 
        North -> reader $ fst . M.findMax . M.filterWithKey (\k a -> snd k == c)
        East  -> reader $ fst . M.findMin . M.filterWithKey (\k a -> fst k == r)
        South -> reader $ fst . M.findMin . M.filterWithKey (\k a -> snd k == c)
        West  -> reader $ fst . M.findMax . M.filterWithKey (\k a -> fst k == r)
-- if you're going north and you hit a wrap, find the biggest row and same column



start :: Board -> Quantum
start board = (coord, East) where
    coord = fst . M.findMin . M.filterWithKey (\k a -> fst k == 1) $ board


-- part B functions start here
-- now we wrap not according to this as a fundamental domain, but instead
-- as the net of a cube

-- assuming the net fits into a 3x4 square of faces
-- the faceSize is then 1/3 of the smaller of width and height
faceSize :: Board -> Int
faceSize board = div (min height width) 3 where
    height = maximum . map fst $ M.keys board
    width = maximum . map snd $ M.keys board

--  1234567890123456
-- 1        ...#
-- 2        .#..
-- 3        #...
-- 4        ....
-- 5...#.......#
-- 6........#...
-- 7..#....#....
-- 8..........#.
-- 9        ...#....
-- 0        .....#..
-- 1        .#......
-- 2        ......#.

netAheadTest :: Quantum -> Quantum
netAheadTest ((r, c), f)
    | r == 1  &&            f == North = ((5, 13 - c), South)
    | r == 5  && c <= 4  && f == North = ((1, 13 - c), South)
    | r == 5  && c >  4  && f == North = ((c - 4, 9), East)
    | r <= 4  && c == 9  && f == West  = ((5, r + 4), South)
    | r == 9  &&            f == North = ((21 - c, 12), West)
    | r > 4   && c == 12 && f == East  = ((9, 21 - r), South)
    | r == 8  && c <= 4  && f == South = ((12, 13 - c), North)
    | r == 12 && c <= 12 && f == South = ((8, 13 - c), North)
    | r == 8  && c > 4   && f == South = ((17 - c, 9), East)
    | r <= 12 && c == 9  && f == West  = ((8, 17 - r), North)
    | r == 12 && c >  12 && f == South = ((21 - c, 1), East)
    |            c == 1  && f == West  = ((12, 21 - r), North)
    | r <= 4  && c == 12 && f == East  = ((17 - r, 16), West)
    |            c == 16 && f == East  = ((17 - r, 12), West)
    | otherwise = ((r + dr, c + dc), f)
      where
        (dr, dc) = stepDir f

--     001
--     050
--     111
--
-- 001  12
-- 051  3
-- 101 45
-- 151 6

-- 1 south to 3 north
-- 1 east  to 2 west
-- 3 south to 5 north
-- 4 east  to 5 west
-- 4 south to 6 north

-- 2 south to 3 west
-- 5 south to 6 east
-- 3 west  to 4 north
-- 1 west  to 4 west
-- 1 north to 6 west
-- 2 east  to 5 east
-- 2 north to 6 south


netAhead :: Quantum -> Quantum
netAhead ((r, c), f)
    | r == 50  &&             f == South = ((c - 50, 100), West)   -- 2 3
    | r <= 100 && c == 100 && f == East  = ((50, r + 50), North)
    | r == 150 &&             f == South = ((c + 100, 50), West)   -- 5 6
    |             c == 50  && f == East  = ((150, r - 100), North) 
    | r >= 51  && c == 51  && f == West  = ((101, r - 50), South)  -- 3 4
    | r == 101 &&             f == North = ((c + 50, 51), East)
    | r <= 50  && c == 51  && f == West  = ((151 - r, 1), East)    -- 1 4
    | r <= 150 && c == 1   && f == West  = ((151 - r, 51), East) 
    | r == 1   && c <= 100 && f == North = ((c + 100, 1), East)    -- 1 6
    | r >= 151 && c == 1   && f == West  = ((1, r - 100), South)
    |             c == 150 && f == East  = ((151 - r, 100), West)  -- 2 5
    | r >= 101 && c == 100 && f == East  = ((151 - r, 150), West)
    | r == 1   && c >= 101 && f == North = ((200, c - 100), North) -- 2 6
    | r == 200 &&             f == South = ((1, c + 100), South)
    | otherwise                          = ((r + dr, c + dc), f)
      where (dr, dc) = stepDir f


-- NW should stay the same
-- SE should stay the same
-- NE should be reversing
-- SW should be reversing

-- NN should be reversing
-- SS should be reversing
-- EE should be reversing
-- WW should be reversing

-- NS should stay the same
-- EW should stay the same


-- north, west fixed should end in 01 or 51
-- south, east fixed should end in 00 or 50


netWrap :: MonadRWS Board () Quantum m => m Quantum
netWrap = netAhead <$> get
-- netWrap = netAheadTest <$> get

-- mains

mainA :: IO ()
mainA = do
    (Just (board, path)) <- parseInput inputP "22/input.txt"
    let answer = 0
    -- let answer = extract (end board path)
    print answer
    -- result <- submitAnswer 2022 22 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (board, path)) <- parseInput inputP "22/input.txt"
    let answer = extract (end board path)
    print answer
    -- result <- submitAnswer 2022 22 2 answer
    -- print result
    return ()
