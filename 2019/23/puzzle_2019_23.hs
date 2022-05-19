module Puzzle_2019_23 where

import Automation (submitAnswer)
import Intcode
import Parsing
import Control.Monad.State
import Data.Conduino
import Data.Sequence (Seq(..), (|>))
import Data.Void (Void)
import qualified Data.Conduino.Combinators as C
import qualified Data.Conduino.Lift as L
import qualified Data.Sequence as Sq


-- data types

data Packet = EmptyPacket 
            | Packet Int Int Int deriving (Eq, Ord, Show)

type Queue = Seq Packet


data Resources = Resources
    { queue :: Queue
    , nat :: Packet
    , prev :: Maybe Int}
    deriving (Eq, Ord, Show)

-- functions

controller :: MonadState Resources m => Pipe () Packet Void m ()
controller = C.repeatM $ do
    q <- gets queue
    n <- gets nat
    case q of
        Empty      -> return n
        (p :<| q') -> (modify $ \r -> r {queue = q'}) >> return p


preprocessor :: Int -> Pipe Packet Int u m u
preprocessor ident = do 
    yield ident 
    awaitForever (packetPicker ident)


packetPicker :: Int -> Packet -> Pipe Packet Int u m ()
packetPicker ident packet = do
    case (packet, ident) of
        (EmptyPacket, _)    -> yield (-1)
        (Packet 255 x y, 0) -> yield x >> yield y
        (Packet i x y, _)   -> if i == ident
            then yield x >> yield y
            else return ()


postprocessor :: MonadState Resources m => Pipe Int Void u m a
postprocessor = do
    ident <- await
    a <- await
    b <- await 
    case (ident, a, b) of
        (Just 255, Just x, Just y) -> do
            modify $ \r -> r {nat = Packet 255 x y}
            postprocessor
        (Just i,   Just x, Just y) -> do 
            q <- gets queue
            modify $ \r -> r {queue = q |> Packet i x y}
            postprocessor
        (_, _, _)              -> error "post processor failure"


node :: MonadState Resources m => Program -> Int -> Pipe Packet Void u m Int
node program ident = preprocessor ident
                  .| intcodePipe program
                  .| postprocessor


cluster :: MonadState Resources m => Program -> Pipe Packet Void u m Int
cluster program = foldl1 altSink $ map (node program) [0..49]


snooperA :: MonadState Resources m => Pipe Packet Void u m Int
snooperA = do
    p <- await
    case p of
        Just (Packet 255 _ y) -> return y
        _ -> snooperA


networkA :: Program -> Int
networkA program = runPipePure $ L.evalStateP initRes pipeline where
    initRes = Resources Empty EmptyPacket Nothing
    pipeline = controller 
            .| altSink snooperA (cluster program)


snooperB :: MonadState Resources m => Pipe Packet Void u m Int
snooperB = do
    p <- await
    py <- gets prev
    case (p, py) of
        (Just (Packet 255 _ y), Just y') -> if y == y' 
            then return y
            else do
                modify $ \r -> r {prev = Just y}
                snooperB
        (Just (Packet 255 _ y), _) -> do 
                modify $ \r -> r {prev = Just y}
                snooperB
        (_, _) -> snooperB


networkB :: Program -> Int
networkB program = runPipePure $ L.evalStateP initRes pipeline where
    initRes = Resources Empty EmptyPacket Nothing
    pipeline = controller 
            .| altSink snooperB (cluster program)


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
    print answer
    -- result <- submitAnswer 2019 23 2 answer
    -- print result
    return ()
