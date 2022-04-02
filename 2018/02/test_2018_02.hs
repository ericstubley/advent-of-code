import Test.HUnit
import System.IO
import Puzzle_2018_02

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput = do
    raw <- readFile "test_input.txt"
    return $ lines raw

parsedInputB = do
    ids <- parseInput "test_input_2.txt"
    return ids

testTwo = TestCase (do
    ids <- parsedInput
    let two_ids = map hasDoubleLetter ids
    [False, True, True, False, True, True, False] @=? two_ids)

testThree = TestCase (do
    ids <- parsedInput
    let three_ids = map hasTripleLetter ids
    [False, True, False, True, False, False, True] @=? three_ids)

testFindCorrect = TestCase (do
    ids <- parsedInputB
    "fgij" @=? findCorrect ids)

testA = TestList
    [ TestLabel "Double Letters" testTwo
    , TestLabel "Triple Letters" testThree]

testB = TestList
    [ TestLabel "OffByOne" testFindCorrect]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()