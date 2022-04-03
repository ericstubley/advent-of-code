import Test.HUnit
import System.IO
import Puzzle_2018_03

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO [(Int, Rectangle)]
parsedInput = do
    rects <- parseInput "test_input.txt"
    return rects


testTotalArea = TestCase (do
    rects <- parsedInput
    let justRects = map snd rects
    let to_all = totalOverlap justRects
    let to_init = totalOverlap $ init justRects
    let to_tail = totalOverlap $ tail justRects
    4 @=? to_all
    4 @=? to_init
    0 @=? to_tail)


testExtractID = TestCase (do
    rects <- parsedInput
    let ident = extractID rects
    3 @=? ident)


testA = TestList
    [TestLabel "Overlap" testTotalArea]

testB = TestList
    [TestLabel "extraction" testExtractID]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()