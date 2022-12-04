import Test.HUnit
import Puzzle_2022_04
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testContainment = TestCase $ do
    (Just assignments) <- parseInput assignmentsP "04/test_input.txt"
    let cs = map containment assignments
    [False, False, False, True, True, False] @=? cs 


testOverlap = TestCase $ do
    (Just assignments) <- parseInput assignmentsP "04/test_input.txt"
    let os = map overlap assignments
    [False, False, True, True, True, True] @=? os 



testA = TestList
    [ TestLabel "Containment" testContainment]

testB = TestList
    [ TestLabel "Overlap" testOverlap]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()