import Test.HUnit
import System.IO
import Puzzle_2018_05

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testReact = TestCase (do
    polymer <- parseInput "test_input.txt"
    let stablePolymer = stabilize polymer
    10 @=? (length stablePolymer)
    "dabCBAcaDA" @=? stablePolymer)

testShortestReaction = TestCase (do
    polymer <- parseInput "test_input.txt"
    let shortestStable = shortestReaction polymer
    "daDA" @=? shortestStable) 

testUnits = TestCase (do
    polymer <- parseInput "test_input.txt"
    let pUnits = units polymer
    "dabc" @=? pUnits)

testShortenedPolymers = TestCase (do
    polymer <- parseInput "test_input.txt"
    let shorts = shortenedPolymers polymer
    ["abAcCaCBAcCcaA", "dbcCCBcCcD", "daAcCaCAcCcaDA", "dabAaBAaDA"] @=? shorts)

testA = TestList
    [TestLabel "Reaction test" testReact]

testB = TestList
    [ TestLabel "Shortest test" testShortestReaction
    , TestLabel "Units" testUnits
    , TestLabel "Shortened List" testShortenedPolymers]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()