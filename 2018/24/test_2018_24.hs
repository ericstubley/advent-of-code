import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Sort (sortOn)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S

import Test.HUnit
import System.IO
import Puzzle_2018_24

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO [Group]
parsedInput = parseInput "test_input.txt"

testSimpleGroupParse = TestCase (do
    let gs = "3266 units each with 6166 hit points with an attack that does 17 bludgeoning damage at initiative 18"
    let g = Group 3266 6166 18 0 0 False Immune (Damage 17 Bludgeoning) S.empty S.empty
    let (Right parsedG) = runParser (groupP Immune) "" gs
    let gs' = "3266 units each with 6166 hit points (immune to cold; weak to radiation) with an attack that does 17 bludgeoning damage at initiative 18"
    let g' = Group 3266 6166 18 0 0 False Immune (Damage 17 Bludgeoning) (S.singleton Cold) (S.singleton Radiation)
    let (Right parsedG') = runParser (groupP Immune) "" gs'
    g @=? parsedG
    g' @=? parsedG')


testGroupParse = TestCase (do
    let gs = "3266 units each with 6166 hit points (immune to cold, slashing; weak to radiation) with an attack that does 17 bludgeoning damage at initiative 18"
    let g = Group 3266 6166 18 0 0 False Immune (Damage 17 Bludgeoning) (S.fromList [Cold, Slashing]) (S.fromList [Radiation])
    let (Right parsedG) = runParser (groupP Immune) "" gs
    g @=? parsedG)

testParse = TestCase (do
    groups <- parseInput "test_input.txt"
    let n = length groups
    -- putStrLn ""
    -- print $ groups !! 0
    -- print $ groups !! 1
    -- print $ groups !! 2
    -- print $ groups !! 3
    4 @=? n)

testRemaining = TestCase (do
    groups <- parsedInput
    (Just groups') <- simulateBattle groups
    5216 @=? totalUnits groups')


testBattleInDepth = TestCase (do
    groups0 <- parsedInput
    let groups1 = battleRound groups0
    let groups2 = battleRound groups1
    let groups3 = battleRound groups2
    let groups4 = battleRound groups3
    let groups5 = battleRound groups4
    let groups6 = battleRound groups5
    let groups7 = battleRound groups6
    let groups8 = battleRound groups7
    [905, 797, 4434] @=? (map (_units) groups1)
    [761, 793, 4434] @=? (map (_units) groups2)
    [618, 789, 4434] @=? (map (_units) groups3)
    [475, 786, 4434] @=? (map (_units) groups4)
    [333, 784, 4434] @=? (map (_units) groups5)
    [191, 783, 4434] @=? (map (_units) groups6)
    [49, 782, 4434] @=? (map (_units) groups7)
    [782, 4434] @=? (map (_units) groups8)
    )


testBoostedRemaining = TestCase (do
    groups <- parsedInput
    (Just groups') <- simulateBattle (boost 1570 groups)
    51 @=? totalUnits groups')


testBinarySearch = TestCase (do
    groups <- parsedInput
    ub <- boostUB 0 groups
    best <- binarySearchBoost 0 ub groups
    1570 @=? best)


testLinearSearch = TestCase (do
    groups <- parsedInput
    best <- linearSearchBoost 0 groups
    1570 @=? best)

testA = TestList
    [ TestLabel "Simple Group Parsing" testSimpleGroupParse
    , TestLabel "Group Parsing" testGroupParse
    , TestLabel "Parse Test" testParse
    , TestLabel "Remaining Units" testRemaining
    , TestLabel "In Depth Battle" testBattleInDepth]

testB = TestList
    [ TestLabel "Boosted Groups Remaining" testBoostedRemaining
    , TestLabel "Binary Search" testBinarySearch
    , TestLabel "Linear Search" testLinearSearch]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()