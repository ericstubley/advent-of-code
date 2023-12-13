import Test.HUnit
import Puzzle_2023_12
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testPossibilities = TestCase $ do
    [[Operational]] @=? possibilities [Operational]
    [[Damaged]] @=? possibilities [Damaged]
    [[Operational], [Damaged]] @=? possibilities [Unknown]
    [[Operational, Operational], [Operational, Damaged], [Damaged, Operational], [Damaged, Damaged]] @=? possibilities [Unknown, Unknown]


testArrangements = TestCase $ do
    (Just reports) <- parseInput reportsP "12/test_input.txt"
    1 @=? countArrangements (Report [Damaged] [1])
    1 @=? countArrangements (Report [Operational] [])
    [1, 4, 1, 1, 4, 10] @=? map countArrangements reports
    -- mapM_ (print . printer) (options . last $ reports)

testArrangements' = TestCase $ do
    (Just reports) <- parseInput reportsP "12/test_input.txt"
    1 @=? countArrangements' (Report [Damaged] [1])
    1 @=? countArrangements' (Report [Operational] [])
    2 @=? countArrangements' (Report [Unknown, Unknown] [1])
    [1, 4, 1, 1, 4, 10] @=? map countArrangements' reports

testUnfold = TestCase $ do
    let r  = Report [Operational, Damaged] [1]
    -- .#?.#?.#?.#?.# 1,1,1,1,1
    let r' = Report [Operational, Damaged, Unknown, Operational, Damaged, Unknown, Operational, Damaged, Unknown, Operational, Damaged, Unknown, Operational, Damaged] [1, 1, 1, 1, 1]
    r' @=? unfold r

testUnfolded = TestCase $ do
    (Just reports) <- parseInput reportsP "12/test_input.txt"
    [1, 16384, 1, 16, 2500, 506250] @=? map (countArrangements' . unfold) reports

testA = TestList
    [ TestLabel "Possibilities" testPossibilities
    , TestLabel "Count arrangements" testArrangements]

testB = TestList
    [ TestLabel "Arrangements'" testArrangements'
    , TestLabel "Unfold" testUnfold
    , TestLabel "Unfolded arrangments" testUnfolded]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()