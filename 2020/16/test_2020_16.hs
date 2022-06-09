import Test.HUnit
import Puzzle_2020_16
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testScanningError :: Test
testScanningError = TestCase $ do
    (Just (ranges, _, tickets)) <- parseInput notesP "16/test_input.txt"
    [4, 55, 12] @=? filter (invalid ranges) (concat tickets) 


testIdentifyFields :: Test
testIdentifyFields = TestCase $ do
    (Just (ranges, _, tickets)) <- parseInput notesP "16/test_input_2.txt"
    [ranges !! 1, ranges !! 0, ranges !! 2] @=? identifyFields ranges tickets



testA = TestList
    [ TestLabel "Scanning Error Rate" testScanningError]

testB = TestList
    [ TestLabel "Identify Fields" testIdentifyFields]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()