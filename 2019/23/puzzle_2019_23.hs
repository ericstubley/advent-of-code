module Puzzle_2019_23 where

import Automation (submitAnswer)
import Intcode
import Parsing
import Control.Monad.State
import Data.Conduino
import Data.Sequence (Seq(..))
import Data.Void (Void)
import qualified Data.Conduino.Combinators as C
import qualified Data.Conduino.Lift as L
import qualified Data.Sequence as Sq


-- data types

data Packet = EmptyPacket | Packet Int Int Int deriving (Eq, Ord, Show)

type Queue = Seq Packet

data Resources = Resources {queue :: Queue, nat :: Packet, ycount :: Count}
    deriving (Eq, Ord, Show)


data Count = None | Once | Twice deriving (Eq, Ord, Show, Enum, Bounded)

-- functions

controller :: MonadState Resources m => Pipe () Packet Void m ()
controller = do
    q <- gets queue
    n <- gets nat
    c <- gets ycount
    case (q, n) of
        (Empty, Packet _ _ _) -> do
            modify (\r -> r {ycount = succ c})
            yield n
            controller
        (Empty, EmptyPacket) -> do
            modify (\r -> r {ycount = None})
            yield n
            controller
        ((Packet i x y) :<| q', _) -> do
            modify (\r -> r {queue = q'})
            yield (Packet i x y)
            controller


preprocessor :: Int -> Pipe Packet Int u m u
preprocessor ident = do 
    yield ident 
    awaitForever (packetPicker ident)


packetPicker :: Int -> Packet -> Pipe Packet Int u m ()
packetPicker ident packet = do
    case packet of
        EmptyPacket -> yield (-1)
        (Packet i x y) -> if ident == i
                            then yield x >> yield y
                            else yield (-1)


postprocessorA :: MonadState Resources m => Pipe Int Void u m Int
postprocessorA = do
    ident <- await
    x <- await
    y <- await 
    case (ident, x, y) of
        (Just 255, Just x', Just y') -> return y'
        (Just i, Just x', Just y')   -> do 
            q <- gets queue
            modify $ \r -> r {queue = (q Sq.|> (Packet i x' y'))}
            postprocessorA
        (_, _, _)                    -> error "post processor failure"


postprocessorB :: MonadState Resources m => Pipe Int Void u m Int
postprocessorB = do
    c <- gets ycount
    packet <- gets nat
    if c == Twice 
        then case packet of 
                EmptyPacket -> return (-1)
                (Packet _ _ y) -> return y
        else do
            mident <- await
            mx <- await
            my <- await 
            case (packet, mident, mx, my) of
                (Packet _ _ y', Just 255, Just x, Just y) -> if y == y'
                    then do
                        modify $ \r -> r {nat = (Packet 0 x y)}
                        postprocessorB
                    else do
                        modify $ \r -> r {nat = (Packet 0 x y), ycount = None}
                        postprocessorB
                (EmptyPacket, Just 255, Just x, Just y) -> do
                    modify $ \r -> r {nat = (Packet 0 x y), ycount = None}
                    postprocessorB
                (_, Just i, Just x, Just y) -> do 
                    q <- gets queue
                    modify $ \r -> r {queue = q Sq.|> (Packet i x y)}
                    postprocessorB
                (_, _, _, _) -> error "post processor failure"


nodeA :: MonadState Resources m => Program -> Int -> Pipe Packet Void u m Int
nodeA program ident = preprocessor ident
                   .| intcodePipe program
                   .| postprocessorA


clusterA :: MonadState Resources m => Program -> Pipe Packet Void u m Int
clusterA program = foldl1 altSink $ map (nodeA program) [0..49]


networkA :: Program -> Int
networkA program = runPipePure $ L.evalStateP initRes pipeline 
  where pipeline = controller .| clusterA program
        initRes = Resources Empty EmptyPacket None



nodeB :: MonadState Resources m => Program -> Int -> Pipe Packet Void u m Int
nodeB program ident = preprocessor ident
                   .| intcodePipe program
                   .| postprocessorB


clusterB :: MonadState Resources m => Program -> Pipe Packet Void u m Int
clusterB program = foldl1 altSink $ map (nodeB program) [0..49]


networkB :: Program -> Int
networkB program = runPipePure $ L.evalStateP initRes pipeline 
  where pipeline = controller .| clusterB program
        initRes = Resources Empty EmptyPacket None





-- mains


mainA :: IO ()
mainA = do
    (Just program) <- parseInput programP "23/input.txt"
    let answer = networkA program
    print answer
    -- result <- submitAnswer 2019 23 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just program) <- parseInput programP "23/input.txt"
    let answer = networkB program
    -- let answer = 0
    print answer
    -- result <- submitAnswer 2019 23 2 answer
    -- print result
    return ()
