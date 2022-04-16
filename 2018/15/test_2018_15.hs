import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A

import Test.HUnit
import System.IO
import Puzzle_2018_15

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


testTurnOrder = TestCase (do
    bf <- parseInput "test_inputs/turn_order.txt"
    [(1:.2), (1:.4), (2:.1), (2:.3), (2:.5), (3:.2), (3:.4)] @=? turnOrder bf)

testMove1 = TestCase (do
    bfPre  <- parseInput "test_inputs/move_1_pre.txt"
    bfPost <- parseInput "test_inputs/move_1_post.txt"
    bfPost @=? (fst $ move  bfPre (1:.1))
    (bfPost, Ongoing) @=? turn bfPre (1:.1))

testMove2 = TestCase (do
    bfPre  <- parseInput "test_inputs/move_2_pre.txt"
    bfPost <- parseInput "test_inputs/move_2_post.txt"
    (bfPost, Ongoing) @=? turn bfPre (1:.2))

testMove3Deeper = TestCase (do
    bf1 <- parseInput "test_inputs/move_3_1.txt"
    let bf2 = modifyField bf1 (3:.4) (UnitTile (Unit 197 3 Elf))
    bf2 @=? attack bf1 (2:.4) (3:.4)
    -- bf2 @=? move bf1 (2:.4)
    (bf2, Ongoing) @=? turn bf1 (2:.4))

testMove3 = TestCase (do
    bf0 <- parseInput "test_inputs/move_3_0.txt"
    bf1 <- parseInput "test_inputs/move_3_1.txt"
    bf2 <- parseInput "test_inputs/move_3_2.txt"
    bf3 <- parseInput "test_inputs/move_3_3.txt"
    bf4 <- parseInput "test_inputs/move_3_4.txt"
    bf1 @=? (resetHealth . fst . battleRound $ bf0)
    bf2 @=? (resetHealth . fst . battleRound $ bf1)
    bf3 @=? (resetHealth . fst . battleRound $ bf2)
    bf4 @=? (resetHealth . fst . battleRound $ bf3))

testOutcomeDeeper = TestCase (do
    bf <- parseInput "test_inputs/outcome_1.txt"
    resetFinal <- parseInput "test_inputs/outcome_1_final.txt"
    let bf1 = nRounds bf 1
    let bf2 = nRounds bf 2
    let bf23 = nRounds bf 23
    let bf47 = nRounds bf 47
    [200, 197, 197, 200, 197, 197] @=? hpExtractor bf1
    [200, 200, 188, 194, 194, 194] @=? hpExtractor bf2
    [200, 200, 131, 131, 131] @=? hpExtractor bf23
    [200, 131, 59, 200] @=? hpExtractor bf47)

testOutcome = TestCase (do
    bf1 <- parseInput "test_inputs/outcome_1.txt"
    bf2 <- parseInput "test_inputs/outcome_2.txt"
    bf3 <- parseInput "test_inputs/outcome_3.txt"
    bf4 <- parseInput "test_inputs/outcome_4.txt"
    bf5 <- parseInput "test_inputs/outcome_5.txt"
    bf6 <- parseInput "test_inputs/outcome_6.txt"
    let bfs = [bf1, bf2, bf3, bf4, bf5, bf6]
    [27730, 36334, 39514, 27755, 28944, 18740] @=? map outcome bfs)


testNoDeathsOutcome = TestCase (do
    bf1 <- parseInput "test_inputs/outcome_1.txt"
    -- bf2 <- parseInput "test_inputs/outcome_2.txt"
    bf3 <- parseInput "test_inputs/outcome_3.txt"
    bf4 <- parseInput "test_inputs/outcome_4.txt"
    bf5 <- parseInput "test_inputs/outcome_5.txt"
    bf6 <- parseInput "test_inputs/outcome_6.txt"
    let bfs = [bf1, bf3, bf4, bf5, bf6]
    [4988, 31284, 3478, 6474, 1140] @=? map noDeathsOutcome bfs)


testA = TestList
    [ TestLabel "Turn Order" testTurnOrder
    , TestLabel "Move 1" testMove1
    , TestLabel "Move 2" testMove2
    , TestLabel "Move 3 Deeper" testMove3Deeper
    , TestLabel "Move 3" testMove3
    , TestLabel "Outcome Deeper" testOutcomeDeeper
    , TestLabel "Outcome" testOutcome]

testB = TestList
    [ TestLabel "No Deaths Outcome" testNoDeathsOutcome]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()