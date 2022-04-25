{-# LANGUAGE TemplateHaskell #-}

module Puzzle_2018_24 where

import Control.Lens
import Data.List (partition, foldl1')
import Data.Maybe
import Data.Set (Set)
import Data.Sort
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S
import System.IO
import Automation (submitAnswer)

-- data types
type Parser = Parsec Void String
data Faction = Immune | Infection deriving (Eq, Show)
data DamageType = Cold | Fire | Slashing | Bludgeoning | Radiation deriving (Eq, Ord, Show)
data Damage = Damage {_val :: Int, _dtype :: DamageType} deriving (Eq, Show)

makeLenses ''Damage

data Group = Group
    { _units :: Int
    , _hp :: Int
    , _initiative :: Int
    , _ident :: Int
    , _target :: Int
    , _selected :: Bool
    , _faction :: Faction
    , _damage :: Damage
    , _immunities :: Set DamageType
    , _weaknesses :: Set DamageType} deriving (Eq, Show)


makeLenses ''Group



-- parsing
parseInput :: String -> IO [Group]
parseInput filename = do
    raw <- readFile filename
    let (Right groups) = runParser inputP "" raw
    return $ setIDs groups


setIDs :: [Group] -> [Group]
setIDs groups = map (\x -> set ident (fst x) (snd x)) $ zip [0..] groups


inputP :: Parser [Group]
inputP = do
    string "Immune System:\n"
    immunes <- manyTill (groupWithEndP Immune) (string "\nInfection:\n")
    infections <- factionP Infection
    return $ immunes ++ infections


groupWithEndP :: Faction -> Parser Group
groupWithEndP fact = do
    g <- groupP fact
    char '\n'
    return g


factionP :: Faction -> Parser [Group]
factionP fact = groupP fact `sepBy` char '\n'

-- 3266 units each with 6166 hit points (immune to cold, 
-- slashing; weak to radiation) with an attack that does 17 
-- bludgeoning damage at initiative 18

groupP :: Faction -> Parser Group
groupP fact = do
    unitParsed <- L.decimal
    string " units each with "
    hpParsed <- L.decimal
    string " hit points "
    (immunesParsed, weaknessesParsed) <- modifiersWrapP
    string "with an attack that does "
    damageValParsed <- L.decimal
    char ' '
    damageTypeParsed <- typeP
    let damageParsed = Damage damageValParsed damageTypeParsed
    string " damage at initiative "
    initParsed <- L.decimal
    let g = Group unitParsed hpParsed initParsed 0 0 False fact damageParsed (S.fromList immunesParsed) (S.fromList weaknessesParsed)
    return g


modifiersWrapP :: Parser ([DamageType], [DamageType])
modifiersWrapP = option ([], []) modifiersP where
    modifiersP :: Parser ([DamageType], [DamageType])
    modifiersP = do
        char '('
        (immunesParsed, weaknessesParsed) <- weakOrBothP <|> immuneOrBothP
        string ") "
        return (immunesParsed, weaknessesParsed)


immuneOrBothP :: Parser([DamageType], [DamageType])
immuneOrBothP = do
    immunesParsed <- immuneP
    weaknessesParsed <- option [] (string "; " >> weakP)
    return (immunesParsed, weaknessesParsed)


weakOrBothP :: Parser([DamageType], [DamageType])
weakOrBothP = do
    weaknessesParsed <- weakP
    immunesParsed <- option [] (string "; " >> immuneP)
    return (immunesParsed, weaknessesParsed)


immuneP :: Parser [DamageType]
immuneP = do
    string "immune to "
    dtypes <- typeListP
    return dtypes


weakP :: Parser [DamageType]
weakP = do
    string "weak to "
    dtypes <- typeListP
    return dtypes


typeListP :: Parser [DamageType]
typeListP = typeP `sepBy` string ", "


typeP :: Parser DamageType
typeP = coldP <|> fireP <|> slashingP <|> bludgeoningP <|> radiationP


coldP :: Parser DamageType
coldP = do
    string "cold"
    return Cold

fireP :: Parser DamageType
fireP = do
    string "fire"
    return Fire

slashingP :: Parser DamageType
slashingP = do
    string "slashing"
    return Slashing

bludgeoningP :: Parser DamageType
bludgeoningP = do
    string "bludgeoning"
    return Bludgeoning

radiationP :: Parser DamageType
radiationP = do
    string "radiation"
    return Radiation


-- auxiliary functions
totalUnits :: [Group] -> Int
totalUnits groups = sum $ map (view units) groups


maxWith :: Ord b => (a -> b) -> [a] -> a
maxWith f ls = foldl1' (\x y -> if f x >= f y then x else y) ls



-- ordering functions
targetOrder :: Group -> Group -> (Int, Int, Int, Int)
targetOrder attacker defender = (f1, f2, f3, f4)
  where
    f1 = if defender^.selected then 0 else 1
    f2 = computeDamage attacker defender
    f3 = effectivePower defender
    f4 = defender^.initiative


selectOrder :: Group -> (Int, Int)
selectOrder selector = (effectivePower selector, selector^.initiative)


attackOrder :: Group -> Int
attackOrder attacker = attacker^.initiative


-- damage computation functions
effectivePower :: Group -> Int
effectivePower group = (group^.damage.val) * (group^.units)


computeDamage :: Group -> Group -> Int
computeDamage attacker defender
    | immune    = 0
    | weak      = 2*power
    | otherwise = power
      where
        attackType = attacker^.damage.dtype
        immune = S.member attackType $ defender^.immunities
        weak = S.member attackType $ defender^.weaknesses
        power = effectivePower attacker


-- do the damage
-- set attacked flag to true
dealDamage :: Group -> Group -> Group
dealDamage attacker defender = set units survivors defender
      where
        totalDamage = computeDamage attacker defender
        casualties = div totalDamage $ defender^.hp
        survivors = max 0 (defender^.units - casualties)


-- main control functions
simulateBattle :: [Group] -> IO (Maybe [Group])
simulateBattle groups = let 
    numImmune = length $ filter (\g -> g^.faction == Immune) groups
    numInfect = length $ filter (\g -> g^.faction == Infection) groups
    immuneUnits = totalUnits $ filter (\g -> g^.faction == Immune) groups
    infectUnits = totalUnits $ filter (\g -> g^.faction == Infection) groups
    in do
    -- print ("Total Immune Units: " ++ (show immuneUnits) ++ " Total Infection Units: " ++ (show infectUnits))
    -- print $ map (\x -> (x^.ident, x^.target)) groups
    -- print $ map (\x -> (x^.ident, x^.units, x^.faction)) groups
    if numImmune == 0 || numInfect == 0
        then 
            return $ Just groups
        else do 
            let groups' = battleRound groups
            if groups == groups'
                then return Nothing
                else simulateBattle groups'
            

battleRound :: [Group] -> [Group]
battleRound groups = sortOn (view ident) (cleanupPhase . attackPhase . selectionPhase $ groups)


-- phase functions
selectionPhase :: [Group] -> [Group]
selectionPhase groups = selectionPhaseHelper orderedGroups groups
  where
    orderedGroups = reverse $ sortOn selectOrder groups
    selectionPhaseHelper :: [Group] -> [Group] -> [Group]
    selectionPhaseHelper [] targets = []
    selectionPhaseHelper (g:gs) targets = g' : selectionPhaseHelper gs (selected : observers)
      where
        (g', selected) = selectTarget g targets
        (targetLs, observers) = partition (\x -> x^.ident == selected^.ident) targets


selectTarget :: Group -> [Group] -> (Group, Group)
selectTarget selector targets 
    | targeted^.selected                    = (set target (-1) selector, targeted)
    | computeDamage selector targeted == 0  = (set target (-1) selector, targeted)
    | otherwise = (set target (targeted^.ident) selector, set selected True targeted)
      where
        factionTargets = filter (\g -> selector^.faction /= g^.faction) targets
        targeted = maxWith (targetOrder selector) factionTargets


-- place in attack order
-- each attack has to update one of the groups
-- do it with a fold?
-- units don't attack if their health <= 0 or if their target is -1
-- problem currently bc need to combine
attackPhase :: [Group] -> [Group]
attackPhase groups = attackHelper 0 orderedGroups
  where
    orderedGroups = reverse $ sortOn attackOrder groups
    attackHelper :: Int -> [Group] -> [Group]
    attackHelper n battlefield
        | n == length battlefield   = battlefield
        | otherwise                 = attackHelper (n+1) (attack attacker battlefield)
            where attacker = battlefield !! n


attack :: Group -> [Group] -> [Group]
attack attacker battlefield
    | attacker^.units <= 0      = battlefield
    | attacker^.target == -1    = battlefield
    | otherwise                 = before ++ [damaged] ++ (tail after)
      where
        targetID = attacker^.target
        before = takeWhile (\g -> g^.ident /= targetID) battlefield
        after = dropWhile (\g -> g^.ident /= targetID) battlefield
        damaged = dealDamage attacker (head after)


-- filter out the dead units
-- reset the selected values
cleanupPhase :: [Group] -> [Group]
cleanupPhase groups = map (\g -> set selected False g) $ filter (\g -> g^.units > 0) groups



-- part b functions
victor :: [Group] -> IO (Maybe Faction)
victor groups = do
    result <- simulateBattle groups
    case result of
        Nothing         -> return Nothing
        (Just groups')  -> return $ Just ((head groups') ^. faction)
    


boost :: Int -> [Group] -> [Group]
boost buff groups = buffedImmunes ++ infects
  where    
    (immunes, infects) = partition (\x -> x^.faction == Immune) groups
    buffedImmunes = map (\x -> set (damage.val) (x^.damage.val + buff) x) immunes


boostUB :: Int -> [Group] -> IO Int
boostUB n groups = do
    putStrLn $ "Trying " ++ (show n) ++ " as upper bound"
    v <- victor $ boost n groups
    if v == (Just Immune)
        then do 
            putStrLn $ "Found an upper bound: victory at " ++ (show n)
            return n
        else boostUB (n+1024) groups


binarySearchBoost :: Int -> Int -> [Group] -> IO Int
binarySearchBoost lb ub groups = do
    print $ show (lb, ub)
    if ub - lb < 2 
        then do
            vl <- victor $ boost lb groups
            vu <- victor $ boost ub groups
            case (vl, vu) of
                (Just Immune, _)    -> (print lb) >> return lb
                (_, Just Immune)    -> (print ub) >> return ub
                (_, _)              -> (print "fail") >> return (-1)
        else do
            let mid = div (lb + ub) 2
            v <- victor $ boost mid groups
            case v of -- maintain upper bound as a known win
                Just Immune -> binarySearchBoost lb mid groups
                Just Infection -> binarySearchBoost mid ub groups -- assume lose/win is monotonic
                Nothing     -> binarySearchBoost (lb+1) ub groups


linearSearchBoost :: Int -> [Group] -> IO Int
linearSearchBoost n groups = do
    putStrLn $ "Trying with a boost of " ++ (show n)
    v <- victor $ boost n groups
    if v == (Just Immune)
        then do
            putStrLn $ "Victory with a boost of " ++ (show n)
            return n
        else linearSearchBoost (n+1) groups





-- mains

mainA :: IO ()
mainA = do
    groups <- parseInput "input.txt"
    (Just finalGroups) <- simulateBattle groups
    let answer = totalUnits finalGroups
    print answer
    -- result <- submitAnswer 2018 24 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    groups <- parseInput "input.txt"
    ub <- boostUB 0 groups
    best <- binarySearchBoost 0 ub groups
    (Just finalGroups) <- simulateBattle (boost best groups)
    let answer = totalUnits finalGroups
    print answer
    -- result <- submitAnswer 2018 24 2 answer
    -- print result
    return ()
