import Test.HUnit
import Puzzle_2021_05
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testSegment = TestCase $ do
    let segment = ((1, 1), (3, 3))
    [(1, 1), (2, 2), (3, 3)] @=? points segment 


testOverlapsA = TestCase $ do
    (Just segments) <- parseInput segmentsP "05/test_input.txt"
    5 @=? overlapsA segments 


testOverlapsB = TestCase $ do
    (Just segments) <- parseInput segmentsP "05/test_input.txt"
    12 @=? overlapsB segments 


testA = TestList
    [ TestLabel "Segment" testSegment
    , TestLabel "Overlaps A" testOverlapsA]

testB = TestList
    [ TestLabel "Overlaps B" testOverlapsB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()