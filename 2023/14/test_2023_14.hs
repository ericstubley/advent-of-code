import Test.HUnit
import Puzzle_2023_14
import Parsing (parseInput)
import Control.Monad.State
import Grid (printMap)
import Data.List (transpose)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testCollect = TestCase $ do
    (Just platform) <- parseInput platformP "14/test_input.txt"
    [34, 27, 17, 10, 8, 7, 7, 14, 0, 12] @=? ((map collect) . columns $ platform)

testStressA = TestCase $ do
    (Just pform) <- parseInput pformP "14/test_input.txt"
    136 @=? stressTestA pform

testManual = TestCase $ do
    (Just pform) <- parseInput pformP "14/test_input.txt"
    print "------------" 
    printMap (transpose pform)
    let pform' = evalState (run >> get) pform
    let pform'' = last $ evalState (repeatM 1 (run >> get)) pform
    pform' @=? pform''
    -- print "------------" 
    -- printMap (transpose pform')
    -- print "------------" 
    -- printMap (transpose pform'')


testRun = TestCase $ do
    (Just pform) <- parseInput pformP "14/test_input.txt"
    print "------------" 
    printMap (transpose pform)
    print "------------" 
    printMap (transpose . last $ evalState (repeatM 1 (run >> get)) pform)
    print "------------" 
    printMap (transpose . last $ evalState (repeatM 2 (run >> get)) pform)
    print "------------" 
    printMap (transpose . last $ evalState (repeatM 3 (run >> get)) pform)
    print "------------" 

testStressB = TestCase $ do
    (Just pform) <- parseInput pformP "14/test_input.txt"
    64 @=? stressTestB 1000000000 pform

testA = TestList
    [ TestLabel "Collect a column" testCollect
    , TestLabel "Stress test A" testStressA]

testB = TestList
    [ TestLabel "Manual flipping" testManual
    , TestLabel "Spin cycle" testRun
    , TestLabel "Stress test B" testStressB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()