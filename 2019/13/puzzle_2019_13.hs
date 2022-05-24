module Puzzle_2019_13 where

import Automation (submitAnswer)
import Intcode
import Parsing
import Control.Monad.IO.Class
import Data.Conduino
import Data.Map.Strict (Map)
import qualified Data.Conduino.Lift as L
import qualified Data.Map.Strict as M


data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq, Ord, Show, Enum)
type ScreenMap = Map (Int, Int) Tile



-- quick hack through part a
blockCount :: [Int] -> Int
blockCount = length . (filter (==Block)) . M.elems . tileMap


tileMap :: [Int] -> ScreenMap
tileMap xs = go xs M.empty where
    go (x:y:t:xs) m = case (x, y) of 
                        (-1, 0) -> go xs m
                        (_ , _) -> go xs (M.insert (x, y) (toEnum t) m) 
    go [] m         = m
    go (x:y:[]) m   = error "Malformed ouptut"
    go (x:[])   m   = error "Malformed output"


-- the basic loop function will be squeezePipe (program) -> print output -> 
-- either ask for input and continue or terminate

displayTile :: Tile -> Char
displayTile t = case t of
                    Empty  -> ' '
                    Wall   -> '#'
                    Block  -> 'X'
                    Paddle -> 'T'
                    Ball   -> '@'



printableScreen :: ScreenMap -> [String]
printableScreen sm = lls where
    xs = map fst $ M.keys sm
    ys = map snd $ M.keys sm
    (lx, ux) = (minimum xs, maximum xs)
    (ly, uy) = (minimum ys, maximum ys)
    ls y = [M.findWithDefault Empty (x, y) sm | x <- [lx..ux]]
    lls = [map displayTile (ls y) | y <- [ly..uy]]


extractScore :: [Int] -> Int
extractScore (x:y:z:xs)
    | (-1, 0) == (x, y) = z
    | otherwise         = extractScore xs
extractScore _ = 0


printScreen :: ScreenMap -> Int -> [Int] -> IO (ScreenMap, Int)
printScreen sm score output = do 
    let score' = max score (extractScore output)
    putStrLn $ "Score: " ++ show score' 
    let sm' = M.union (tileMap output) sm
    mapM_  putStrLn $ printableScreen sm'
    return (sm', score')


runGame :: Program -> IO ()
runGame program = gameLoop M.empty 0 (intcodePipe program) where
    gameLoop :: (Monad m, MonadIO m) 
             => ScreenMap -> Int -> Pipe Int Int () m a -> m ()
    gameLoop sm score p = do
        (output, result) <- squeezePipe p
        (sm', score') <- liftIO $ printScreen sm score output
        case result of
            (Left cont) -> do
                i <- liftIO $ joystick
                gameLoop sm' score' (cont i)
            (Right val) -> return ()


joystick :: IO Int
joystick = do
    putStrLn "Joystick <_>"
    j <- getLine
    case j of
       "a" -> return (-1)
       "d" -> return 1
       _   -> return 0


runNoMovement :: Program -> IO Int
runNoMovement program = hackLoop M.empty 0 (intcodePipe program) where
    hackLoop :: (Monad m, MonadIO m) 
             => ScreenMap -> Int -> Pipe Int Int () m a -> m Int
    hackLoop sm score p = do
        (output, result) <- squeezePipe p
        (sm', score') <- liftIO $ printScreen sm score output
        case result of
            (Left cont) -> hackLoop sm' score' (cont 0)
            (Right val) -> return score'




mainA :: IO ()
mainA = do
    (Just breakout) <- parseInput programP "13/input.txt"
    let (_, output) = runProgram breakout []
    let answer = blockCount output
    print answer
    -- result <- submitAnswer 2019 13 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just breakout) <- parseInput programP "13/sneakier_input.txt"
    let freeBreakout = 2 : tail breakout
    score <- runNoMovement freeBreakout
    let answer = score
    print answer
    -- result <- submitAnswer 2019 13 2 answer
    -- print result
    return ()
