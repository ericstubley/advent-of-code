module Puzzle_2023_20 where

import Automation (submitAnswer)
import Parsing
import Control.Applicative (liftA2)
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..), (><))

import Lens.Micro.Platform

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq

-- data types
data Env = Env { _modules :: Map String Module
               , _queue :: Seq Pulse
               , _low :: Int
               , _high :: Int } deriving (Eq, Ord, Show)


data Module = Module { _name :: String
                     , _outputs :: [String]
                     , _guts :: Guts} deriving (Eq, Ord, Show)

data Guts = Button
          | Broadcaster
          | Flipflop Bool
          | Conjunction (Map String Energy)
          | Output deriving (Eq, Ord, Show)


data Pulse = Pulse { _source :: String
                   , _dest :: String
                   , _energy :: Energy } deriving (Eq, Ord, Show)

data Energy = Low | High deriving (Eq, Ord, Show)

makeLenses ''Env
makeLenses ''Module
makeLenses ''Pulse

-- parsing
modulesP :: Parser [Module]
modulesP = sepBy moduleP newline

moduleP :: Parser Module
moduleP = broadcasterP <|> flipflopP <|> conjunctionP

broadcasterP :: Parser Module
broadcasterP = do
   string "broadcaster" 
   pointerP
   os <- outputP
   return $ Module "broadcaster" os Broadcaster

flipflopP :: Parser Module
flipflopP = do
    char '%'
    n <- nameP
    pointerP
    os <- outputP
    return $ Module n os (Flipflop False)

conjunctionP :: Parser Module
conjunctionP = do
    char '&'
    n <- nameP
    pointerP
    os <- outputP
    return $ Module n os (Conjunction M.empty)

outputP :: Parser [String]
outputP = sepBy nameP (string ", ")

nameP :: Parser String
nameP = some letterChar

pointerP :: Parser String
pointerP = string " -> "

-- functions
buildEnv :: [Module] -> Env
buildEnv ms = Env moduleState Sq.empty 0 0
  where blank = M.union (M.fromList $ zip (map _name ms) ms) defaultModules
        moduleState = updateConjunctions ms blank

-- for each module in the list go through the outputs
-- for any that are conjunction, add m False to the map
updateConjunctions :: [Module] -> Map String Module -> Map String Module
updateConjunctions [] m = m
updateConjunctions (md:ms) m = updateConjunctions ms (addOutput md m)


-- for each output
-- do the operation of 
addOutput :: Module -> Map String Module -> Map String Module
addOutput modul m = foldl (\acc o -> setConjunction (_name modul) o acc) m (_outputs modul)

-- start end
-- if end is a conjunction add it to the list
setConjunction :: String -> String -> Map String Module -> Map String Module
setConjunction start end m
    | isConjunction modul = M.insert end modul' m
    | otherwise = m
      where modul = m M.! end
            (Conjunction bits) = _guts modul
            modul' = modul & guts .~ Conjunction (M.insert start Low bits)



isConjunction :: Module -> Bool
isConjunction (Module _ _ (Conjunction x)) = True
isConjunction _ = False


defaultModules :: Map String Module
defaultModules = M.fromList [ ("button", Module "button" ["broadcaster"] Button)
                            , ("output", Module "output" [] Output)
                            , ("rx", Module "rx" [] (Flipflop False))] -- puzzle input has this against spec!


-- add a single button push pulse to the queue
push :: MonadState Env m => m ()
push = do
    enQueue
    whileM tick


enQueue :: MonadState Env m => m ()
enQueue = do
    let pulse = Pulse "button" "broadcaster" Low
    collect [pulse]


whileM :: Monad m => m Bool -> m ()
whileM a = do
    result <- a
    if result then whileM a else return ()

repeatM :: (Applicative m) => Int -> m a -> m [a]
repeatM n a = loop n
    where loop n
            | n <= 0    = pure []
            | otherwise = liftA2 (:) a (loop (n - 1))


tick :: MonadState Env m => m Bool
tick = inspect >>= handle >>= collect >> continue

-- remove from the queue, update the count, pass it on
inspect :: MonadState Env m => m Pulse
inspect = do
    q <- gets (^. queue)
    let (pulse :<| q') = q
    modify $ queue .~ q'
    if pulse ^. energy == Low
        then modify $ low +~ 1
        else modify $ high +~ 1
    return pulse

-- update the module state based on the pulse
-- pass off any emitted pulses
handle :: MonadState Env m => Pulse -> m [Pulse]
handle pulse = do
    modul <- gets (^. modules . to (M.! (pulse ^. dest)))
    let modul' = execute pulse modul
    let pulses = fire pulse modul'
    modify $ modules %~ M.insert (modul' ^. name) modul'
    return pulses


-- add the pulses into the queue
collect :: MonadState Env m => [Pulse] -> m ()
collect pulses = modify $ queue %~ (\q -> q >< (Sq.fromList pulses))


continue :: MonadState Env m => m Bool
continue = gets (^. queue . to Sq.null . to not)


-- non stateful pulse update
-- when do things happen?
-- Low pulse and Flipflop
execute :: Pulse -> Module -> Module
execute pulse modul = 
    case (_guts modul, _energy pulse) of
         (Broadcaster, e)   -> modul
         (Output, _)        -> modul
         (Flipflop b, High) -> modul
         (Flipflop b, Low)  -> modul & guts .~ Flipflop (not b)
         (Conjunction m, e) -> modul & guts .~ 
                                Conjunction (M.insert (_source pulse) e m)


fire :: Pulse -> Module -> [Pulse]
fire pulse modul =
    case (_guts modul, _energy pulse) of
        (Broadcaster, e)   -> construct e modul
        (Output, _)        -> []
        (Flipflop b, High) -> []
        (Flipflop b, Low)  -> construct (if b then High else Low) modul
        (Conjunction m, e) -> construct (if all (==High) (M.elems m) 
                                            then Low
                                            else High) modul

construct :: Energy -> Module -> [Pulse]
construct e m = map (\n -> Pulse (_name m) n e) $ _outputs m


solveA :: [Module] -> Int
solveA ms = (final ^. low) * (final ^. high)
  where env = buildEnv ms
        final = execState (repeatM 1000 push) env


-- part b functions
-- probably not feasible to just keep repeating

-- solveB :: [Module] -> Int
-- solveB ms = evalState (countM (push >> terminate)) env
--   where env = buildEnv ms


terminate :: MonadState Env m => m Bool
terminate = do
    m <- gets (^. modules . to (M.! "rx"))
    let (Flipflop hit) = _guts m
    return hit


countM :: MonadState Env m => m Bool -> m Int
countM a = go 1
  where go n = do
            result <- a
            if result 
                then return n
                else go (n+1)


binaryStringToInt :: String -> Int
binaryStringToInt bs = foldl (\acc i -> 2*acc + (digitToInt i)) 0 bs


solveB :: Int
solveB = foldl1 lcm (map binaryStringToInt manualInputs)


manualInputs :: [String]
manualInputs = [ "111101111111"
               , "111110010101"
               , "111101011011"
               , "111101000011"]



-- mains

mainA :: IO ()
mainA = do
    (Just ms) <- parseInput modulesP "20/input.txt"
    let answer = solveA ms
    print answer
    -- result <- submitAnswer 2023 20 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    -- (Just ms) <- parseInput modulesP "20/input.txt"
    let answer = solveB
    print answer
    -- result <- submitAnswer 2023 20 2 answer
    -- print result
    return ()
