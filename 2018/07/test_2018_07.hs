import Test.HUnit
import System.IO
import Puzzle_2018_07

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO Graph
parsedInput = do
    graph <- parseInput "test_input.txt"
    return graph

testTopSort = TestCase (do
    graph <- parsedInput
    "CABDFE" @=? topologicalSort graph)

testTimeToCompletion = TestCase (do
    graph <- parsedInput
    let testTimer = timeToCompletion 2 0
    6 @=? (testTimer $ graphFromEdges [('C', 'A'), ('B', 'D')])
    15 @=? testTimer graph)

testA = TestList
    [ TestLabel "Top Sort" testTopSort]

testB = TestList
    [ TestLabel "Time to finish" testTimeToCompletion]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()