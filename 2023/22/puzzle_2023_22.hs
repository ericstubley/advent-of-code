module Puzzle_2023_22 where

import Automation (submitAnswer)
import Parsing
import Utilities (partitionM, whileM)

import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Set (Set)
import Lens.Micro.Platform

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
type Block = (Int, Int, Int)
type Brick = (Block, Block)

data Env = Env { _settled :: Set Block
               , _falling :: [Brick]
               , _fallen :: [Brick] } 
                    deriving (Eq, Ord, Show)

makeLenses ''Env

-- parsing
bricksP :: Parser [Brick]
bricksP = sepBy brickP newline

brickP :: Parser Brick
brickP = do
    a <- natural
    char ','
    b <- natural 
    char ','
    c <- natural
    char '~'
    d <- natural
    char ','
    e <- natural
    char ','
    f <- natural
    return ((a, b, c), (d, e, f))

-- functions

-- what is the simulation for blocks to fall?
-- do one pass through the list of falling blocks and try to settle them
-- if none settle do a fall through

-- has the dust settled?
dusted :: MonadState Env m => m Bool
dusted = gets $ not . null . _falling 


-- keep doing a settle and fall until there's nothing falling anymore
simulate :: MonadState Env m => m ()
simulate = whileM (simul >> dusted)


-- keep settling until we can't anymore, then fall
simul :: MonadState Env m => m ()
simul = (whileM settle) >> fall


fall :: MonadState Env m => m ()
fall = modify (falling %~ (map down))


down :: Brick -> Brick 
down brick = brick & _1 . _3 -~ 1 & _2 . _3 -~ 1


settle :: MonadState Env m => m Bool
settle = do
    candidates <- gets _falling
    (process, keep) <- partitionM stable candidates
    mapM convert process
    modify $ falling .~ keep
    modify $ fallen %~ (++) process
    return $ not . null $ process


stable :: MonadState Env m => Brick -> m Bool
stable brick = do
    let grounded = (brick ^. _1 . _3) == 1
    structure <- gets _settled
    let touching = not 
                 . S.disjoint structure 
                 . S.fromList 
                 . blocks 
                 . down 
                 $ brick
    return (grounded || touching)

stableWithRemoval :: MonadState Env m => Brick -> m Bool
stableWithRemoval brick = do
    let grounded = (brick ^. _1 . _3) == 1
    structure <- gets _settled
    let structure' = S.difference structure (S.fromList $ blocks brick)
    let touching = not 
                 . S.disjoint structure'
                 . S.fromList 
                 . blocks 
                 . down 
                 $ brick
    return (grounded || touching)


-- add to the settled set of blocks
convert :: MonadState Env m => Brick -> m ()
convert brick = modify (settled %~ S.union add)
  where add = S.fromList $ blocks brick


blocks :: Brick -> [Block]
blocks ((a, b, c), (d, e, f))
    | a /= d = [(x, b, c) | x <- [a..d]]
    | b /= e = [(a, y, c) | y <- [b..e]]
    | c /= f = [(a, b, z) | z <- [c..f]]
    | otherwise = [(a,b,c)]


removable :: Set Block -> [Brick] -> Brick -> Bool
removable base bs brick = and $ evalState (mapM stableWithRemoval bs') env
  where base' = S.difference base (S.fromList $ blocks brick)
        bs' = filter (/= brick) bs
        env = Env base' [] []


jenga :: [Brick] -> Int
jenga bricks = length . filter (removable base graves) $ graves
  where (Env base _ graves) = execState simulate (Env S.empty bricks [])


-- part b functions
fallCount :: Set Block -> [Brick] -> Brick -> Int
fallCount base bs brick = length . _falling $ execState (simul) env
  where base' = S.difference base (S.fromList $ blocks brick)
        bs' = filter (/= brick) bs
        env = Env S.empty bs' []


solveB :: [Brick] -> Int
solveB bricks = sum . map (fallCount base graves) $ graves
  where (Env base _ graves) = execState simulate (Env S.empty bricks [])



-- mains

mainA :: IO ()
mainA = do
    (Just bricks) <- parseInput bricksP "22/input.txt"
    let answer = jenga bricks
    print answer
    -- result <- submitAnswer 2023 22 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just bricks) <- parseInput bricksP "22/input.txt"
    let answer = solveB bricks
    print answer
    -- result <- submitAnswer 2023 22 2 answer
    -- print result
    return ()
