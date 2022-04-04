import Test.HUnit
import System.IO
import Puzzle_2018_04

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO [WallEntry]
parsedInput = do
    ws <- parseInput "test_input.txt"
    return ws


testSleepiestGuard = TestCase (do
    ws <- parsedInput
    let sg = sleepiestGuard ws
    Just 10 @=? sg)

testSleepiestMinute = TestCase (do
    ws <- parsedInput
    let gs = filter (\x -> ident x == Just 10) ws
    let (sm, _) = sleepiestMinute gs
    24 @=? sm)

testTimeAsleep = TestCase (do
    ws <- parsedInput
    let gs = filter (\x -> ident x == Just 10) ws
    let ta = timeAsleep gs
    50 @=? ta )

testOverallMinute = TestCase (do
    ws <- parsedInput
    let (sg, sm) = overallSleepiestGuardMinutePair ws
    99 @=? sg
    45 @=? sm)


testA = TestList
    [ TestLabel "Sleepiest Guard" testSleepiestGuard
    , TestLabel "Sleepiest Minute" testSleepiestMinute
    , TestLabel "Time Asleep" testTimeAsleep]

testB = TestList
    [ TestLabel "Sleepiest Overall" testOverallMinute]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()