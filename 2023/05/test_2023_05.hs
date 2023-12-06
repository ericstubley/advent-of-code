import Test.HUnit
import Puzzle_2023_05
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testPiecewise = TestCase $ do
    let f = builder 50 98 2
    let resultF = [0..97] ++ [50, 51]
    resultF @=? map f [0..99]

    let g = builder 52 50 48
    let resultG = [0..49] ++ [52..99] ++ [98, 99]
    resultG @=? map g [0..99]

    let m = getPiecewise $ (Piecewise f) <> (Piecewise g)
    let resultM = [0..49] ++ [52..99] ++ [50, 51]
    resultM @=? map m [0..99]

testClosest = TestCase $ do
    (Just (seeds, locator)) <- parseInput inputP "05/test_input.txt"
    [82, 43, 86, 35] @=? map locator seeds

testBigBrain = TestCase $ do
    (Just (seeds, locator)) <- parseInput inputP "05/test_input.txt"
    46 @=? bigBrain seeds locator

testProcess = TestCase $ do
    (Just (seeds, stages)) <- parseInput inputBP "05/test_input.txt"
    46 @=? (minimum . (map _start) $ process seeds stages)

testApply = TestCase $ do
    True @=? True

testPush = TestCase $ do
    let seed = Interval 79 14
    let build = Builder 50 (Interval 98 2)
    [Interval 79 14] @=? push [build] seed

    let build' = Builder 52 (Interval 50 48)
    [Interval 81 14] @=? push [build'] seed



testA = TestList
    [ TestLabel "Piecewise direct construction" testPiecewise
    , TestLabel "Closest seed" testClosest]

testB = TestList
    [ TestLabel "Big brain part B solution" testBigBrain
    , TestLabel "Process" testProcess
    , TestLabel "Apply" testApply
    , TestLabel "Push" testPush]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()