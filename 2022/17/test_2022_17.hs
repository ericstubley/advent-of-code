import Test.HUnit
import Puzzle_2022_17
import Parsing (parseInput)
import Control.Monad.State

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testBasic = TestCase $ do
    (Just jets) <- parseInput jetsP "17/test_input.txt"
    let ic = initialChamber jets
    print $ _current $ execState push ic


testHeight = TestCase $ do
    (Just jets) <- parseInput jetsP "17/test_input.txt"
    1 @=? heightAfter jets 1 
    4 @=? heightAfter jets 2
    6 @=? heightAfter jets 3 
    7 @=? heightAfter jets 4 
    3068 @=? heightAfter jets 2022 


testFiddle = TestCase $ do
    (Just jets) <- parseInput jetsP "17/test_input.txt"
    print $ evalState detectCycle (initialChamber jets)
    -- let jets = [L]
    -- let (Chamber rp rs js c) = initialChamber jets
    -- let ic = Chamber rp (cycle [Horizontal]) js c
    -- 1 @=? (evalState cycleLength ic)
    -- 1 @=? (evalState (computeHeight 1) ic)
    -- 1000000000000 @=? heightAfterLong jets 1000000000000


testLongHeight = TestCase $ do
    (Just jets) <- parseInput jetsP "17/test_input.txt"
    print $ (evalState detectCycle (initialChamber jets))
    1514285714288 @=? heightAfterLong jets 1000000000000 


testA = TestList
    [ testBasic
    , testHeight]

testB = TestList
    [ testFiddle
    , testLongHeight]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()