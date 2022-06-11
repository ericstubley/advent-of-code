import Test.HUnit
import Puzzle_2020_18
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testEvaluate :: Test
testEvaluate = TestCase $ do
    (Just exprs) <- parseInput exprsP "18/test_input.txt"
    [71, 51, 26, 437, 12240, 13632] @=? map evaluate exprs 


testAdvanced :: Test
testAdvanced = TestCase $ do
    (Just exprs) <- parseInput advancedsP "18/test_input.txt"
    [231, 51, 46, 1445, 669060, 23340] @=? map advanced exprs


testA = TestList
    [ TestLabel "Evaluate" testEvaluate]

testB = TestList
    [ TestLabel "Advanced" testAdvanced]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()