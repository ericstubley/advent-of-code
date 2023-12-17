module Puzzle_2023_16 where

import Automation (submitAnswer)
import Parsing
import Grid (Direction(..))
import Data.Massiv.Array (Array, Ix2(..), Sz(..), (<!), (!>))
import qualified Data.Massiv.Array as A
import Control.Monad.Reader
import Data.Set (Set(..))
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Sq
import qualified Data.Set as St

-- data types
type Array2 a = A.Array A.U A.Ix2 a
type Env = Array2 Char
type Queue = Seq (Ix2, Direction)
type Seen = Set (Ix2, Direction)


-- parsing
contraptionP :: Parser Env
contraptionP = A.fromLists' A.Seq <$> sepBy (some squareP) newline

squareP :: Parser Char
squareP = char '.' <|> char '|' <|> char '-' <|> char '/' <|> char '\\'

-- functions
-- set of seen things
-- queue of things to process
illuminate :: MonadReader Env m => (Ix2, Direction) -> m Seen
illuminate start = illuminate' St.empty $ Sq.singleton start
  where illuminate' :: MonadReader Env m
                    => Seen
                    -> Queue
                    -> m Seen
        illuminate' seen Sq.Empty = return seen
        illuminate' seen ((ix, d) :<| queue) = do
            if St.member (ix, d) seen
                then illuminate' seen queue
                else do
                    oob <- not <$> reader (\env -> A.isSafeIndex (A.size env) ix)
                    if oob
                        then illuminate' seen queue
                        else do
                            let seen' = St.insert (ix, d) seen
                            paths <- follow (ix, d)
                            let queue' = queue >< (Sq.fromList paths)
                            illuminate' seen' queue'


follow :: MonadReader Env m => (Ix2, Direction) -> m [(Ix2, Direction)]
follow (ix, d) = do
    c <- reader (\env -> A.index' env ix)
    let ds = directions c d
    return $ map (\d -> (move ix d, d)) ds




-- if | and d = 
directions :: Char -> Direction -> [Direction]
directions c d
    | c == '|'  && d == West  = [North, South]
    | c == '|'  && d == East  = [North, South]
    | c == '-'  && d == North = [East, West]
    | c == '-'  && d == South = [East, West]
    | c == '/'  && d == North = [East]
    | c == '/'  && d == East  = [North]
    | c == '/'  && d == South = [West]
    | c == '/'  && d == West  = [South]
    | c == '\\' && d == North = [West]
    | c == '\\' && d == East  = [South]
    | c == '\\' && d == South = [East]
    | c == '\\' && d == West  = [North]
    | otherwise               = [d]


move :: Ix2 -> Direction -> Ix2
move (i :. j) North = (i-1) :. j
move (i :. j) East  = i :. (j+1)
move (i :. j) South = (i+1) :. j
move (i :. j) West  = i :. (j-1)


illuminated :: Env -> Int
illuminated env = St.size . St.map fst . runReader (illuminate (0:.0, East)) $ env

lumens :: Env -> (Ix2, Direction) -> Int
lumens env start = St.size . St.map fst . runReader (illuminate start) $ env

maxLumens :: Env -> Int
maxLumens env = maximum $ map (lumens env) boundaries
  where (Sz (i:.j)) = A.size env
        top   = map (\col -> (0 :. col,South)) [0..(j-1)]
        left  = map (\row -> (row :. 0, East)) [0..(i-1)]
        bot   = map (\col -> ((i-1) :. col, North)) [0..(j-1)]
        right = map (\row -> (row :. (j-1), West)) [0..(i-1)]
        boundaries = top ++ left ++ bot ++ right
-- mains

mainA :: IO ()
mainA = do
    (Just env) <- parseInput contraptionP "16/input.txt"
    let answer = illuminated env
    print answer
    -- result <- submitAnswer 2023 16 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just env) <- parseInput contraptionP "16/input.txt"
    let answer = maxLumens env
    print answer
    -- result <- submitAnswer 2023 16 2 answer
    -- print result
    return ()
