module Puzzle_2020_12 where

import Automation (submitAnswer)
import Parsing
import Grid (Direction(..), dirVec, reverseDir)
import Utilities (CyclicEnum(..))


-- data types
data Action = N | E | S | W | L | R | F deriving (Eq, Ord, Show)

data Command = Command {action :: Action, value :: Int}
    deriving (Eq, Ord, Show)

data Ship = Ship 
    { pos :: (Int, Int)
    , dir :: Direction}
    deriving (Eq, Ord, Show)

data Complex a = Complex a a 

instance Num a => Num (Complex a) where
    (+) (Complex a b) (Complex c d) = Complex (a+c) (b+d)
    (-) (Complex a b) (Complex c d) = Complex (a-c) (b-d)
    (*) (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + b*c)
    fromInteger n = Complex (fromInteger n) (fromInteger 0)
    abs = undefined
    signum = undefined

data Relative = Relative
    { pos' :: Complex Int
    , waypoint :: Complex Int}

-- parsing
commandsP :: Parser [Command]
commandsP = sepBy commandP newline

commandP :: Parser Command
commandP = Command <$> actionP <*> integer

actionP :: Parser Action
actionP = (char 'N' >> return N) <|>
          (char 'E' >> return E) <|>
          (char 'S' >> return S) <|>
          (char 'W' >> return W) <|>
          (char 'L' >> return L) <|>
          (char 'R' >> return R) <|>
          (char 'F' >> return F)

-- functions

instruction :: Ship -> Command -> Ship
instruction ship (Command a v) = 
    case a of
        N -> move ship v (dirVec North)
        E -> move ship v (dirVec East)
        S -> move ship v (dirVec South)
        W -> move ship v (dirVec West)
        L -> turn ship $ (div v 90)*(-1)
        R -> turn ship (div v 90)
        F -> move ship v (dirVec $ dir ship)


move :: Ship -> Int -> (Int, Int) -> Ship
move ship l (dx, dy) = ship {pos = (sx + l*dx, sy + l*dy)}
  where (sx, sy) = pos ship


turn :: Ship -> Int -> Ship
turn ship n = case compare n 0 of
                EQ -> ship 
                GT -> turn (ship {dir = csucc (dir ship)}) (n-1)
                LT -> turn ship (n+4)


instructions :: Ship -> [Command] -> Ship
instructions = foldl instruction


finalPosition :: [Command] -> (Int, Int)
finalPosition commands = pos ship'
  where ship = Ship (0, 0) East
        ship' = instructions ship commands


-- relative waypoint versions of everything
i :: Complex Int
i = Complex 0 1

relative :: Relative -> Command -> Relative
relative ship (Command a v) =
    case a of
        N -> alter ship v (Complex 0 1)
        E -> alter ship v (Complex 1 0)
        S -> alter ship v (Complex 0 (-1))
        W -> alter ship v (Complex (-1) 0)
        L -> rotate ship (div v 90)
        R -> rotate ship $ (div v 90)*(-1)
        F -> relMove ship v


alter :: Relative -> Int -> Complex Int -> Relative
alter ship n direction = ship {waypoint = waypoint'}
  where waypoint' = (waypoint ship) + (Complex n 0)*direction


rotate :: Relative -> Int -> Relative
rotate ship n = case compare n 0 of
                    EQ -> ship
                    GT -> ship {waypoint = (waypoint ship) * (i^n)}
                    LT -> ship {waypoint = (waypoint ship) * ((-i)^(-n))}


relMove :: Relative -> Int -> Relative
relMove ship n = ship {pos' = newPos}
  where newPos = (pos' ship) + (Complex n 0)*(waypoint ship)


relatives :: Relative -> [Command] -> Relative
relatives = foldl relative


finalRelative :: [Command] -> (Int, Int)
finalRelative commands = (u, v)
  where ship = Relative (Complex 0 0) (Complex 10 1)
        ship' = relatives ship commands
        (Complex u v) = pos' ship'

-- mains

mainA :: IO ()
mainA = do
    (Just commands) <- parseInput commandsP "12/input.txt"
    let (fx, fy) = finalPosition commands
    let answer = abs fx + abs fy
    print answer
    -- result <- submitAnswer 2020 12 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just commands) <- parseInput commandsP "12/input.txt"
    let (fx, fy) = finalRelative commands
    let answer = abs fx + abs fy
    print answer
    -- result <- submitAnswer 2020 12 2 answer
    -- print result
    return ()
