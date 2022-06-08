import Test.HUnit
import Puzzle_2020_13
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testBus :: Test
testBus = TestCase $ do
    (Just (time, buses)) <- parseInput scheduleP "13/test_input.txt"
    59 @=? earliestBus time buses
    5 @=? waitingTime time 59


testCrt :: Test
testCrt = TestCase $ do
    (221, 102) @=? crt (17, 0) (13, 11)
    (4199, 3417) @=? crt (221, 102) (19, 16)
    (4199, 3417) @=? foldl1 crt [(17, 0), (13, 11), (19, 16)]


testContests :: Test
testContests = TestCase $ do
    (Just contests) <- parseInput testConP "13/test_contests.txt"
    [1068781, 3417, 754018, 779210, 1261476, 1202161486] @=? map winner contests


testA = TestList
    [ TestLabel "Bus bus bus" testBus]

testB = TestList
    [ TestLabel "CRT" testCrt
    , TestLabel "Contests" testContests]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()