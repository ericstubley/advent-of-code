import Test.HUnit
import Puzzle_2022_23
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testExtract = TestCase $ do
    (Just grove) <- parseInput groveP "23/test_input.txt"
    110 @=? (countEmpty $ simulate grove 10)


testStabilize = TestCase $ do
    (Just grove) <- parseInput groveP "23/test_input.txt"
    20 @=? stabilize grove


testA = TestList
    [ TestLabel "Final extraction" testExtract]

testB = TestList
    [ TestLabel "Stabilize" testStabilize]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()