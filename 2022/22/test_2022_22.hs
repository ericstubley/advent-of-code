import Test.HUnit
import Puzzle_2022_22
import Parsing (parseInput)
import Grid (Direction(..))

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse = TestCase $ do
    (Just (board, path)) <- parseInput inputP "22/test_input.txt"
    print ""
    print board
    print ""


testIntermediate = TestCase $ do
    (Just (board, path)) <- parseInput inputP "22/test_input.txt"
    ((1, 11), East)  @=? end board (take 1 path)
    ((1, 11), South) @=? end board (take 2 path)
    ((6, 11), South) @=? end board (take 3 path)
    ((6, 11), East)  @=? end board (take 4 path)
    ((6, 4), East)   @=? end board (take 5 path)
    ((6, 4), South)  @=? end board (take 6 path)
    ((8, 4), South)  @=? end board (take 7 path)
    ((8, 4), East)   @=? end board (take 8 path)
    ((8, 8), East)   @=? end board (take 9 path)
    ((8, 8), South)  @=? end board (take 10 path)
    ((6, 8), South)  @=? end board (take 11 path)
    ((6, 8), East)   @=? end board (take 12 path)
    ((6, 8), East)   @=? end board (take 13 path)



testExtract = TestCase $ do
    (Just (board, path)) <- parseInput inputP "22/test_input.txt"
    6032 @=? extract (end board path)


testExtractB = TestCase $ do
    (Just (board, path)) <- parseInput inputP "22/test_input.txt"
    5031 @=? extract (end board path)



testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Intermediate Steps" testIntermediate
    , TestLabel "Extraction" testExtract]

testB = TestList
    [ TestLabel "Extraction B" testExtractB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()